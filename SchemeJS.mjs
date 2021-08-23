//
// SchemeJS: Scheme in JavaScript
//
// Copyright 2021 Stan Switzer -- (sjswitzer [at] gmail [dot] com)
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

import * as SchemeJS from './SchemeJSCore.mjs';

export const VERSION = SchemeJS.VERSION;
export const LogicError = SchemeJS.LogicError;
const isArray = Array.isArray;
const MUL = '\u00d7', DIV = '\u00f7';

// So that optional parameters show up pretty when printed
const optional = undefined;

//
// Creates a SchemeJS instance.
//
export function createInstance(schemeOpts = {}) {
  let globalScope = SchemeJS.createInstance(schemeOpts);
  const defineBindings = schemeOpts.defineBindings ?? true;
  const readFile = schemeOpts.readFile;
  const latin1 = schemeOpts.latin1 ?? false;
  const supplemental = schemeOpts.supplemental ?? false;
  const dumpIdentifierMap = schemeOpts.dumpIdentifierMap ?? false;
  const jitThreshold = schemeOpts.jitThreshold ?? undefined;
  const TRACE_INTERPRETER = !!(schemeOpts.traceInterpreter ?? false);
  const TRACE_COMPILER = !!(schemeOpts.traceCompiler ?? false);
  const TRACE_COMPILER_CODE = !!(schemeOpts.traceCompilerCode ?? false);
  const _reportError = schemeOpts.reportError = error => console.log(error); // Don't call this one
  const reportSchemeError = schemeOpts.reportSchemeError ?? _reportError; // Call these instead
  const reportSystemError = schemeOpts.reportError ?? _reportError;
  const reportLoadInput = schemeOpts.reportLoadInput ?? (expr => undefined);
  const reportLoadResult = schemeOpts.reportLoadResult ?? ((result, expr) => console.log(string(result)));
  const linePrinter = schemeOpts.linePrinter ?? (line => console.log(line));
  const arraysAreLists = schemeOpts.arraysAreLists ?? true;
  const generatorsAreLists = schemeOpts.generatorsAreLists ?? true;
  const standardIteratorsAreLists = schemeOpts.standardIteratorsAreLists ?? true;
  const allIteratorsAreLists = schemeOpts.standardIteratorsAreLists ?? false;
  const lambdaStr = schemeOpts.lambdaStr ?? "\\";
  const firstName = schemeOpts.firstName ?? "first";
  const restName = schemeOpts.firstName ?? "rest";
  const nilName = schemeOpts.nilName ?? "NIL";

  let string = globalScope.string ?? required();
  let exportAPI = globalScope.exportAPI ?? required();
  let defineGlobalSymbol = globalScope.defineGlobalSymbol ?? required();
  let defineBinding = globalScope.defineBinding ?? required();
  let isList = globalScope.isList ?? required();
  let isNil = globalScope.isNil ?? required();
  let iterateAsList = globalScope.iterateAsList ?? required();
  let isIterable = globalScope.isIterable ?? required();
  let moreList = globalScope.moreList ?? required();
  let LIST = globalScope.LIST ?? required();
  let MORELIST = globalScope.MORELIST ?? required();
  let FIRST = globalScope.FIRST ?? required();
  let REST = globalScope.REST ?? required();
  let NIL = globalScope.NIL ?? required();
  let LAMBDA_CHAR = globalScope.LAMBDA_CHAR ?? required();
  let QUOTE_ATOM = globalScope.QUOTE_ATOM ?? required();
  let _eval = globalScope._eval ?? required();
  let compare_hooks = globalScope.compare_hooks ?? required();
  let Atom = globalScope.Atom ?? required();
  let isAtom = globalScope.isAtom ?? required();
  let cons = globalScope.cons ?? required();
  let car = globalScope.car ?? required();
  let cdr = globalScope.cdr ?? required();
  let iteratorFor = globalScope.iteratorFor ?? required();
  let schemeTrue = globalScope.schemeTrue ?? required();
  let SchemeEvalError = globalScope.SchemeEvalError ?? required();
  let compileEval = globalScope.compileEval ?? required();
  let SchemeError = globalScope.SchemeError ?? required();
  let SchemeCompileError = globalScope.SchemeCompileError ?? required();
  let EVALUATE_KEY_VALUE = globalScope.EVALUATE_KEY_VALUE ?? required();
  let ESCAPE_STRINGS = globalScope.ESCAPE_STRINGS ?? required();
  function required() { throw "required" }

  { // (Local scope so we don't hang onto the property descriptors forever)
    // Import global JavaScript symbols
    let propDescs = Object.getOwnPropertyDescriptors(globalThis);
    for (let name in propDescs) {
      let {value, get} = propDescs[name];
      if (name === 'eval') name = "js-eval";
      if (!get && value)
        defineGlobalSymbol(name, value, { schemeOnly: true, dontInline: true, group: "imported" });
    }

    // Object static methods: Object-getOwnPropertyDescriptors, etc.
    propDescs = Object.getOwnPropertyDescriptors(Object);
    for (let name in propDescs) {
      let {value, get} = propDescs[name];
      if (!get && typeof value === 'function')
        defineGlobalSymbol(`Object-${name}`, value, { schemeOnly: true, dontInline: true, group: "imported" });
    }

    // Stuff the whole Math class in there!
    propDescs = Object.getOwnPropertyDescriptors(Math);
    for (let name in propDescs) {
      let {value, get} = propDescs[name];
      if (!get && value) {
        // SIOD defines *pi* so I'll just define them all like that
        if (typeof value === 'number')
          defineGlobalSymbol(`*${name.toLowerCase()}*`, value, { schemeOnly: true }, `Math-${name}`);
        // SIOD defines sin, cos, asin, etc. so I'll just define them all like that,
        // but also as Math-sin, etc.
        if (typeof value === 'function')
          defineGlobalSymbol(name, value, { schemeOnly: true }, `Math-${name}`);
      }
    }
    defineGlobalSymbol("abs", a => a < 0 ? -a : a);  // Overwrite Math.abs; this deals with BigInt too
    // This is redundant but I want them defined in the "builtin" group.
    defineGlobalSymbol("globalThis", globalThis, { schemeOnly: true });
    for (let obj of [Object, Boolean, Symbol, Number, String, BigInt, Array])
      defineGlobalSymbol(obj.name, obj, { schemeOnly: true });
    defineGlobalSymbol("Date-now", Date.now, { schemeOnly: true });
  }

  //
  // Equality
  //
  // Because lists can be circular and stack depths are finite, equal
  // must have bounds on its recursion and looping. The bounds are large and
  // user-optionble, but they are finite. If the bounds are exceeded, deep_eq
  // returns "undefined," which is "falsey" but distinguishable from
  // "false."
  //
  // A client can pass in a "report" object which is filled-in with a report
  // on where and how objects differ. This has proved useful for unit testing
  // but can be generally useful. The report can also contain a strCmp
  // property that determines how strings are compared, for instance you can ignore case
  // or leading and trailing whitespace; sometimes you're playing horseshoes.
  // You can also opt that NaNs are considered equal.
  //
  // An EQUAL_FUNCTION can be attached to any object or class to define how equal
  // compares the object.
  //
  let EQUAL_FUNCTION = Symbol("SchemeJS-EQUAL-FUNCTION");
  exportAPI("EQUAL_FUNCTION", EQUAL_FUNCTION);

  exportAPI("equal", equal);
  defineGlobalSymbol("equal?", equal, { dontInline: true });
  function equal(a, b, maxDepth = 10000, maxLength = 10000000, report = {}) {
    if (a === b) return true;
    let stringCompare = report.stringCompare ?? ((a, b) => a === b);
    let NaNsEqual = report.NaNsEqual;
    let res = deep_eq(a, b, 0, 0), originalReport = report;
    if (report !== originalReport)
      originalReport.reason = report;
    return res;
    function deep_eq(a, b, depth, length) {
      if (a === b) return true;
      if (depth > maxDepth) {
        report.maxedOut = report.maxDepth = depth;
        return undefined;
      }
      if (length > maxLength) {
        report.maxedOut = report.maxLength = length;
        return undefined;
      }
      // Any object or class can have an EQUAL_FUNCTION
      // The EQUAL_FUNCTION can deem objects of different types equal if it chooses,
      // So this precedes the type check.
      let equalFunction = (a != null && a[EQUAL_FUNCTION]) ?? (b != null && b[EQUAL_FUNCTION]);
      if (equalFunction) {
        let res = equalFunction(a, b);
        // If nullish, continue with other tests!
        if (res != null) {
          report.a = a, report.b = b;
          report.equalFunction = equalFunction;
          return res;
        }
      }
      if (typeof a !== typeof b) {
        report.a = a, report.b = b;
        report.typesDiffer = true;
        return false;
      }
      // Both types same now so no need to test typeof b
      if (typeof a === 'string') {
        let res = stringCompare(a, b);
        if (!res) {
          report.a = a, report.b = b;
          report.stringsDiffer = true;
        }
        return res;
      }
      // Normally NaNs are not equal to anything, but we can opt that they are
      if (typeof a === 'number' && NaNsEqual && isNaN(a) && isNaN(b))
        return true;
      if ((a == null || b == null) || typeof a !== 'object') { // this includes Functions, which are strangely not 'object'
        let res = a === b;
        if (!res) {
          report.a = a, report.b = b;
          report.valuesDiffer = true;
        }
        return res;
      }
      if (iterateAsList(a)) {
        if (!iterateAsList(b)) {
          report.a = a, report.b = b;
          report.valuesDiffer = true;
          return false;
        }
        let i = 0, aRest = a, bRest = b;
        for ( ; iterateAsList(aRest) && iterateAsList(bRest); ++i, ++length, aRest = aRest[REST], bRest = bRest[REST]) {
          let res = deep_eq(aRest[FIRST], bRest[FIRST], depth+1, length);
          if (!res) {
            report.list = a, report.a = aRest[FIRST], report.b = bRest[FIRST];
            report.elementsDiffer = i;
            report = { reason: report };
            return res;
          }
        }
        if (isNil(aRest) && isNil(bRest))
          return true;
        return deep_eq(aRest, bRest, depth+1, length+1);
      } else if (iterateAsList(b)) {
        report.a = a, report.b = b;
        report.valuesDiffer = true;
        return false;
      }
      if (Object.getPrototypeOf(a) !== Object.getPrototypeOf(b)) {
        report.a = a, report.b = b;
        report.prototypesDiffer = true;
        return false;
      }
      // Since the protos are the same; if either is an array, both are.
      // But I might change my mind about the prototype check, so leave the additional
      // tests in for now.
      if (isArray(a)) {
        if (a.length != b.length) {
          report.a = a, report.b = b;
          report.aVal = a.length, report.bVal = b.length;
          report.property = 'length';
          report.valuesDiffer = true;
          return false;
        }
        // Array elements are also properties so fall through to Object comparison.
        // This also distinguishes Arrays that have missing elements.
      }
      // Object compare, symbols first (since Scheme is symbol-oriented), then names.
      let res = compareProps(Object.getOwnPropertySymbols(a), Object.getOwnPropertySymbols(b));
      if (!res) return res;
      return compareProps(Object.getOwnPropertyNames(a), Object.getOwnPropertyNames(b));

      function compareProps(aProps, bProps) {
        for (let property of aProps) {
          if (!b.hasOwnProperty(property)) {
            report.a = a, report.b = b;
            report.aVal = a[property];
            report.bMissingProperty = property;
            report = { reason: report };
            return false;
          }
        }
        for (let property of bProps) {
          if (!a.hasOwnProperty(property)) {
            report.a = a, report.b = b;
            report.bVal = b[property];
            report.aMissingProperty = property;
            report = { reason: report };
            return false;
          }
          let res = deep_eq(a[property], b[property], depth+1, length);
          if (!res) {
            report.a = a, report.b = b;
            report.aVal = a[property], report.bVal = b[property];
            report.elementsDiffer = property;
            report.valuesDiffer = true;
            report = { reason: report };
            return res;
          }
        }
        return true;
      }
    }
  }

  defineGlobalSymbol("=", scheme_eq, { evalArgs: 2, compileHook: scheme_eq_hook, group: "compare-op" });
  function scheme_eq(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!equal(a, b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
    }
    return true;
  }
  function scheme_eq_hook(args, ssaScope, tools) {
    if (args.length < 2) return 'true';
    tools.use(tools.bind(equal, "equal"));
    return compare_hooks(args, ssaScope, tools, 'equal(A, B)', 'scheme_eq');
  }

  //
  // Sorting
  //

  // (qsort list predicate-fcn access-fcn)
  //   "qsort" is a lie for API compatibility with SIOD, but this sort has
  //   comparable performance and is excellent with partially-sorted lists.
  defineGlobalSymbol("mergesort", mergesort, { dontInline: true, group: "list-op" }, "sort", "qsort");
  function mergesort(list, predicateFn = optional, accessFn = optional) {
    if (isNil(list)) return NIL;
    // Sort Arrays as Arrays
    if (isArray(list))
      return in_place_mergesort(list.slice(0), predicateFn, accessFn);
    // Lists and other iterables are sorted as lists
    if (isList(list))
      return in_place_mergesort(copy_list(list), predicateFn, accessFn);
    let copied = NIL, last;
    if (!isIterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
    for (let item of list) {
      item = cons(item, NIL);
      if (last) last = last[REST] = item;
      else copied = last = item;
    }
    return in_place_mergesort(copied, predicateFn, accessFn);
  }

  defineGlobalSymbol("in-place-mergesort", in_place_mergesort, { dontInline: true, group: "list-op" }, "in-place-sort", "nsort");
  function in_place_mergesort(list, predicateFn = optional, accessFn = optional) {
    if (isNil(list)) return NIL;
    // Reduce the optional predicete and access function to a single (JavaScript) "before" predicate
    let before = predicateFn, scope = this;
    if (predicateFn) {
      if (accessFn) {
        before = (a, b) => predicateFn.call(scope, accessFn.call(scope, a), accessFn.call(scope, b));
      }
    } else {
      if (accessFn) {
        before = (a, b) => accessFn.call(scope, a) <  accessFn.call(scope, b);
      } else {
        before = (a, b) => a < b
      }
    }
    // Sort arrays as arrays
    if (isArray(list)) {
      // ES10 stipulates that it only cares whether the compare function
      // returns > 0, which means move "b"  before "a," or <= zero,
      // which means leave "a" before "b". There's no need to distinguish
      // the "equal" case. Which is nice for us because the "before"
      // predicate doesn't distinguish that case (without a second call
      // with reversed arguments.)
      list.sort((a,b) => before.call(scope, a, b) ? -1 : 1);
      return list;
    }
    if (isList(list)) {
      return llsort.call(this, list, before);
    }
    throw new TypeError(`Not a list or array ${string(list)}`);
  }
  
  // A bottom-up mergesort that coalesces runs of ascending or descending items.
  // Runs are extended on either end, so runs include more than strictly ascending
  // or descending sequences. The upshot is that it executes in O(n log m) where "m"
  // is the number of runs. Lists that are mostly or partly ordered sort MUCH faster
  // and already-sorted or even reverse-sorted lists sort in linear time because
  // there's only one run. Sorting a few new items into an already sorted list
  // is particularly fast.
  //
  // This combines run-accumulation from TimSort with the well-known (bottom-up) mergesort.
  // A run will always be at least two elements long (as long as there are two elements
  // remaining) but will often be much longer. As far as I know, this is novel.
  //    https://en.wikipedia.org/wiki/Merge_sort#Bottom-up_implementation_using_lists
  //    https://gist.github.com/sjswitzer/1dc76dc0b4dcf67a7fef
  //    https://gist.github.com/sjswitzer/b98cd3647b7aa0ef9ecd
  function llsort(list, before) {
    let stack = [];
    while (moreList(list)) {
      // Accumulate a run that's already sorted.
      let run = list, runTail = list;
      list = list[REST];
      while (moreList(list)) {
        let listNext = list[REST];
        runTail[REST] = NIL;
        if (before.call(this, list[FIRST], run[FIRST])) {
          list[REST] = run;
          run = list;
        } else {
          if (!before.call(this, list[FIRST], runTail[FIRST])) {
            runTail[REST] = list;
            runTail = list;
          } else {
            break;
          }
        }
        list = listNext;
      }

      // The number of runs at stack[i] is either zero or 2^i and the stack size is bounded by 1+log2(nruns).
      // There's a passing similarity to Timsort here, though Timsort maintains its stack using
      // something like a Fibonacci sequence where this uses powers of two.
      //
      // It's instructive--and kinda fun--to put a breakpoint right here, "watch" these
      // expressions then invoke (apropos), which uses sort internally:
      //     string(list)
      //     string(run)
      //     string(stack[0])
      //     string(stack[1])
      //     etc.
      let i = 0;
      for ( ; i < stack.length; ++i) {
        if (isNil(stack[i])) {
          stack[i] = run;
          run = NIL;
          break;
        };
        run = merge(stack[i], run);
        stack[i] = NIL;
      }
      if (!isNil(run))
        stack.push(run);
    }
    // Merge all remaining stack elements
    let run = NIL;
    for (let i = 0; i < stack.length; ++i)
      run = merge(stack[i], run);
    return run;

    function merge(left, right) {
      // When equal, left goes before right
      let merged = NIL, last;
      while (moreList(left) && moreList(right)) {
        if (before.call(this, right[FIRST], left[FIRST])) {
          let next = right[REST];
          if (last) last[REST] = right;
          else merged = right;
          last = right;
          right = next;
        } else {
          let next = left[REST];
          if (last) last[REST] = left;
          else merged = left;
          last = left;
          left = next;
        }
        last[REST] = NIL;
      }
      // Can't both be Cons cells; the loop above ensures it
      if (moreList(left)) {
        if (last) last[REST] = left;
        else merged = left;
      } else if (moreList(right)) {
        if (last) last[REST] = right;
        else merged = right;
      }
      return merged;
    }
  }

  //
  // Lispy stuff
  //

  const caaar = a => car(car(car(a)));
  const caadr = a => car(car(cdr(a)));
  const caar = a => car(car(a));
  const cadar = a => car(cdr(car(a)));
  const caddr = a => car(cdr(cdr(a)));
  const cadr = a => car(cdr(a));
  const cdaar = a => cdr(car(car(a)));
  const cdadr = a => cdr(car(cdr(a)));
  const cdar = a => cdr(car(a));
  const cddar = a => cdr(cdr(car(a)));
  const cdddr = a => cdr(cdr(cdr(a)));
  const cddr = a => cdr(cdr(a));
  defineGlobalSymbol("caaar", caaar);
  defineGlobalSymbol("caar", caar);
  defineGlobalSymbol("caadr", caadr);
  defineGlobalSymbol("cadar", cadar);
  defineGlobalSymbol("caddr", caddr);
  defineGlobalSymbol("cadr", cadr);
  defineGlobalSymbol("cdaar", cdaar);
  defineGlobalSymbol("cdadr", cdadr);
  defineGlobalSymbol("cdar", cdar);
  defineGlobalSymbol("cddar", cddar);
  defineGlobalSymbol("cdddr", cdddr);
  defineGlobalSymbol("cddr", cddr);

  defineGlobalSymbol("intern", Atom, { dontInline: true });

  defineGlobalSymbol("list", list, { group: "list-op", compileHook: list_hook });
  function list(...elements) {
    let val = NIL;
    for (let i = elements.length; i > 0; --i)
      val = cons(elements[i-1], val);
    return val;
  }
  function list_hook(args, ssaScope, tools) {
    let emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
    // The compiler can inline the list function just fine, but it's better to do it this way
    // because no argument array needs to be constructed.
    let ssaResult = newTemp("list");
    emit(`let ${ssaResult} = NIL;`);
    for (let i = args.length; i > 0; --i)
      emit(`${ssaResult} = cons(${args[i-1]}, ${ssaResult});`)
    return ssaResult;
  }

  defineGlobalSymbol("copy-list", copy_list, { dontInline: true, group: "list-op" });  // TODO: unit tests!
  function copy_list(...lists) {
    let res = NIL, last;
    for (let list of lists) {
      for ( ; iterateAsList(list); list = list[REST]) {
        let item = cons(list[FIRST], NIL);
        if (last) last = last[REST] = item;
        else res = last = item;
      }
      if (!isNil(list)) {
        for (let item of list) {
          item = cons(item, NIL);
          if (last) last = last[REST] = item;
          else res = last = item;
          list = list[REST];
        }
      }
    }
    return res;
  }

  // (apropos substring) -- Returns a list of all atoms containing the given substring in their names
  defineGlobalSymbol("apropos", apropos, { usesDynamicScope: true, dontInline: true });
  function apropos(substring) {
    if (!substring) substring = "";
    // Normalize to upper case because some localles have squirreley
    // lower-case rules.
    substring = substring.toUpperCase();
    let matches = NIL, scope = this;
    for ( ; scope && scope !== Object; scope = Object.getPrototypeOf(scope)) {
      let symbols = Object.getOwnPropertySymbols(scope);
      for (let symbol of symbols) {
        if (!isAtom(symbol)) continue;
        let name = string(symbol);
        if (name.toUpperCase().includes(substring))
          matches = cons(symbol, matches);
      }
    }
    return this.nsort(matches,
      (a,b) => a.description.toUpperCase() < b.description.toUpperCase());
  }

  defineGlobalSymbol("to-lower-case", to_lower_case);
  function to_lower_case(str, locale = optional) {
    if (typeof str !== 'string') throw new TypeError(`${string(str)} is not a string}`);
    let result;  // write this way so that it can be a compiler template
    if (locale === undefined) result = str.toLowerCase();
    else result = str.toLocaleLowerCase(locale);
    return result;
  }

  defineGlobalSymbol("to-upper-case", to_upper_case);
  function to_upper_case(str, locale = optional) {
    if (typeof str !== 'string') throw new TypeError(`${string(str)} is not a string}`);
    let result;  // write this way so that it can be a compiler template
    if (locale === undefined) result = str.toUpperCase();
    else result = str.toLocaleUpperCase(locale);
    return result;
  }

  defineGlobalSymbol("max", max, { group: "var-op" });
  function max(a, b, ...rest) {
    let val = a;
    if (val < b) val = b;
    for (b of rest)
      if (val < b) val = b;
    return val;
  }

  defineGlobalSymbol("min", min, { group: "var-op" });
  function min(a, b, ...rest) {
    let val = a;
    if (val > b) val = b;
    for (b of rest)
      if (val > b) val = b;
    return val;
  }

  // (prog1 form1 form2 form3 ...)
  defineGlobalSymbol("prog1", prog1, { evalArgs: 0, compileHook: prog1_hook, group: "core" });
  function prog1(...forms) {
    let res = NIL;
    for (let i = 0, formsLength = forms.length; i < formsLength; ++i) {
      let val = _eval(forms[i], this);
      if (i === 0)
        res = val;
    }
    return res;
  }
  function prog1_hook(args, ssaScope, tools) {
    let ssaResult = 'NIL';
    for (let i = 0; i< args.length; ++i) {
      let res = compileEval(args[i], ssaScope, tools);
      if (i === 0)
        ssaResult = res;
    }
    return ssaResult;
  }

  // (cond clause1 clause2 ...)  -- clause is (predicate-expression form1 form2 ...)
  defineGlobalSymbol("cond", cond, { evalArgs: 0, compileHook: cond_hook, group: "core", schemeOnly: true });
  function cond(...clauses) {
    // Prescan for errors; the compiler needs to do it so the interpreter should too
    for (let i = 0, clausesLength = clauses.length; i < clausesLength; ++i) {
      let clause = clauses[i];
      if (!isList(clause))
        throw new SchemeEvalError(`Bad clause in "cond" ${string(clause)}`);
    }
    for (let i = 0, clausesLength = clauses.length; i < clausesLength; ++i) {
      let clause = clauses[i];
      let predicateForm = clause[FIRST], forms = clause[REST];
      let evaled = _eval(predicateForm, this);
      if (schemeTrue(evaled)) {
        let res = NIL;
        for ( ; iterateAsList(forms); forms = forms[REST])
          res = _eval(forms[FIRST], this);
        if (!isNil(forms)) {
          for (let form of forms)
            res = _eval(form, this);
        }
        return res;
      }
    }
    return NIL;
  }
  function cond_hook(args, ssaScope, tools) {
    let clauses = args;
    let ssaResult = tools.newTemp('cond');
    tools.emit(`let ${ssaResult} = NIL; ${ssaResult}: {`);
    let saveIndent = tools.indent;
    tools.indent += '  ';
    for (let clause of clauses) {
      if (!isList(clause))
        throw new SchemeCompileError(`Bad cond clause${string(clause)}`);
      let predicateForm = clause[FIRST], forms = clause[REST];
      let ssaPredicateValue = compileEval(predicateForm, ssaScope, tools);
      tools.emit(`if (schemeTrue(${ssaPredicateValue})) {`)
      let saveIndent = tools.indent;
      tools.indent += '  ';
      let ssaValue = 'NIL';
      for (let form of forms) {
        ssaValue = compileEval(form, ssaScope, tools);
      }
      tools.emit(`${ssaResult} = ${ssaValue};`);  // Another PHI node
      tools.emit(`break ${ssaResult};`)
      tools.indent = saveIndent;
      tools.emit(`}`);
    }
    tools.indent = saveIndent;
    tools.emit(`}`);
    return ssaResult;
  }

  defineGlobalSymbol("require", require_, { dontInline: true });
  function require_(path) {
    let sym = Atom(`*${path}-loaded*`);
    if (!schemeTrue(globalScope[sym])) {
      this.load(path);
      globalScope[sym] = true;
      return sym;
    }
    return NIL;
  }

  // (load fname noeval-flag)
  //   If the neval-flag is true then a list of the forms is returned otherwise the forms are evaluated.
  defineGlobalSymbol("load", load, { dontInline: true });
  function load(path, noEval = false) {
    let scope = this, result = NIL, last;
    let fileContent;
    try {
      if (!readFile) throw new SchemeEvalError("No file reader defined");
      fileContent = readFile(path);
    } catch (error) {
      let loadError = new SchemeEvalError(`Load failed ${string(path)}`);
      loadError.cause = error;
      loadError.path = path;
      return false;
    }
    let characterGenerator = iteratorFor(fileContent, LogicError);
    let assignSyntax = false;  // NEVER allow assign syntax in loaded files.
    for(;;) {
      let expr;
      try {
        expr = parseSExpr(characterGenerator, { path, assignSyntax });
        if (!expr) break;
        reportLoadInput(expr);
        if (noEval) {
          if (last) last = last[REST] = cons(expr, NIL);
          else result = last = cons(expr, NIL);
        } else {
          let value = _eval(expr, scope);
          reportLoadResult(value, expr);
        }
      } catch (error) {
        if (error instanceof SchemeError)
          reportSchemeError(error, expr);
        else
          reportSystemError(error, expr);
      }
    }
    return result;
  }

  // Provisional but useful
  defineGlobalSymbol("println", println);
  function println(...lines) {
    for (let line of lines)
      linePrinter(line);
    return true;
  }

  // Can be inlined (if isList, isIterator, isList, etc. are bound), but doesn't seem wise
  defineGlobalSymbol("append", append, { dontInline: true, group: "list-op" });
  function append(...lists) {
    let res = NIL, last;
    for (let list of lists) {
      for ( ; iterateAsList(list); list = list[REST])
        if (last) last = last[REST] = cons(list[FIRST], NIL);
        else res = last = cons(list[FIRST], NIL);
     if (!isNil(list)) {
        for (let value of list) {
          let item = cons(value, NIL);
          if (last) last = last[REST] = item;
          else res = last = item;
        }
      }
    }
    return res;
  }

  defineGlobalSymbol("last", last, { dontInline: true, group: "list-op" });
  function last(list) {
    if (!isIterable(list))
      throw new TypeError(`not a list or iterable ${string(list)}`);
    let res = NIL;
    if (!list || isNil(list)) return NIL; // XXX check this.
      for ( ; iterateAsList(list); list = list[REST])
        res = list[FIRST];
    if (!isNil(list)) {
      // Don't special-case string; its iterator returns code points by combining surrogate pairs
      if (isArray(list)) {
        if (list.length > 0)
          return list[list.length-1];
        return NIL;
      }
      for (let value of list)
        res = value;
    }
    return res;
  }

  defineGlobalSymbol("butlast", butlast, { dontInline: true, group: "list-op" }); // TODO: needs unit test!
  function butlast(list) {
    let res = NIL, last;
    for ( ; iterateAsList(list) && moreList(list[REST]); list = list[REST])
      if (last) last = last[REST] = cons(list[FIRST], NIL);
      else res = last = cons(list[FIRST], NIL);
    if (!isNil(list)) {
      let previous, first = true;;
      for (let value of list) {
        if (!first) {
          let item = cons(previous, NIL);
          if (last) last = last[REST] = item;
          else res = last = item;
        }
        first = false;
        previous = value;
      }
    }
    return res;
  }

  defineGlobalSymbol("reverse", reverse, { dontInline: true, group: "list-op" });
  function reverse(...lists) {
    let res = NIL;
    for (let list of lists) {
      for ( ; iterateAsList(list); list = list[REST])
        res = cons(list[FIRST], res)
      if (!isNil(list)) {
        for (item of list)
          res = cons(item, res);
      }
    }
    return res;
  }

  defineGlobalSymbol("nreverse", in_place_reverse, { dontInline: true, group: "list-op" });  // Name from SIOD
  function in_place_reverse(list) {
    let res = NIL;
    while (iterateAsList(list)) {
      let next = list[REST];
      list[REST] = res;
      res = list;
      list = next;
    }
    if (!isNil(list)) throw new TypeError(`not in-place reversible`);
    return res;
  }

  // XXX TODO: member and memq are almost certainly wrong. Need to find out about SIOD equality.
  // (member key list)
  //     Returns the portion of the list where FIRST is equal to the key, or () if none found.
  defineGlobalSymbol("member", member, { dontInline: true, group: "list-op" });
  function member(key, list) {
    for ( ; moreList(list); list = list[REST])
      if (key === list[FIRST])   // TODO: == or ===?
        return list;
    return NIL;
  }

  // (memq key list)
  //     Returns the portion of the list where FIRST is eq to the key, or () if none found.
  defineGlobalSymbol("memq", memq, { dontInline: true, group: "list-op" });
  function memq(key, list) {
    for ( ; moreList(list); list = list[REST])
      if (key === list[FIRST])
        return list;
    return NIL;
  }

  // (nth index list)
  //     Reference the list using index, with the first element being index 0.
  defineGlobalSymbol("nth", nth, { dontInline: true, group: "list-op" });
  function nth(index, list) {
    if (typeof index !== 'number' || Math.trunc(index) !== index)
      throw new TypeError(`not an integer ${string(index)}`);
    if (index < 0) throw new RangeError(`negative index`);
    for ( ; index > 0 && iterateAsList(list); list = list[REST])
      index -= 1;
    if (!isNil(list)) {
      if (index === 0 && isList(list))
        return list[FIRST];
      if (isArray(list) || typeof list === 'string')
        if (index < list.length)
          return list[index];
      for (let value of list) {
        if (index <= 0)
          return value;
        index -= 1;
      }
    }
    throw new RangeError(`nth`);
  }

  let gensym_count = 0;
  const gensym = (name = `*gensym-${gensym_count++}*`) => Symbol(name);
  defineGlobalSymbol("gensym", gensym);

  //
  // SIOD-style try/catch
  //
  class SchemeJSThrow extends SchemeError {
    constructor(tag, value, msg) {
      value;
      super(msg);
      this.tag = tag;
      this.value = value;
    }
    toString() {
      return `${super.toString()} ${string(this.tag)} ${string(this.value)}`;
    }
  };
  SchemeJSThrow.prototype.name = "SchemeJSThrow";

  // (*throw tag value) -- SIOD style
  defineGlobalSymbol("*throw", schemeThrow, { dontInline: true });
  function schemeThrow(tag, value) { throw new SchemeJSThrow(tag, value)}

  // (*catch tag form ...) -- SIOD style
  defineGlobalSymbol("*catch", schemeCatch, { evalArgs: 1, compileHook: siod_catch_hook });
  function schemeCatch(tag, form, ...forms) {  // XXX order of args?
    let val;
    try {
      val = _eval(form, this);
      for (let i = 0, formsLength = forms.length; i < forms.length; ++i)
        val = _eval(forms[i], this);
    } catch (e) {
      if (!(e instanceof SchemeJSThrow)) throw e;  // rethrow
      if (e.tag !== tag) throw e;
      val = e.value;
    }
    return val;
  }
  function siod_catch_hook(args, ssaScope, tools) {
    let emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
    if (args.length < 2) throw new LogicError(`Bad catch`);
    let ssaTag = args[0];
    let ssaResult = newTemp('siod_catch'), ssaValue = 'NIL';
    emit(`let ${ssaResult};`);
    emit(`try {`);
    let saveIndent = tools.indent;
    tools.indent += '  ';
    for (let i = 1; i < args.length; ++i)
      ssaValue = compileEval(args[i], ssaScope, tools);
    emit(`${ssaResult} = ${ssaValue};`);
    tools.indent = saveIndent;
    emit(`} catch (e) {`);
    tools.indent += '  ';
    let ssaSchemeJSThrow = use(bind(SchemeJSThrow));
    emit(`if (!(e instanceof ${ssaSchemeJSThrow}) || e.tag !== ${ssaTag}) throw e;`)
    emit(`${ssaResult} = e.value;`);
    tools.indent = saveIndent;
    emit(`}`);
    return ssaResult;
  }

  defineGlobalSymbol("to-string", (obj, maxCarDepth = 100, maxCdrDepth = 10000) => string(obj, { maxCarDepth, maxCdrDepth }));

  defineGlobalSymbol("eval-string", eval_string, { dontInline: true });
  function eval_string(str, scope = this) {
    let expr = parseSExpr(str);
    return _eval(expr, scope);
  }

  let quitRepl = false;
  const quit = _ => quitRepl = true;
  defineGlobalSymbol("quit", quit);

  exportAPI("REPL", REPL);
  function REPL(readline, opts = {}) {  // readline(prompt) => str | nullish
    let scope = this;
    opts = { ...schemeOpts, ...opts };
    let name = opts.name ?? "SchemeJS";
    let prompt = opts.prompt ?? name + " > ";
    let print = opts.print ?? (x => console.log(string(x)));
    let reportSchemeError = opts.reportSchemeError ?? (x => console.log(String(x)));;
    let reportSystemError = opts.reportSystemError ?? (x => console.log(name + " internal error:", String(x), x));;
    let endTest = opts.endTest ?? (_ => false);
    let parseContext = opts.parseContext = [];
    quitRepl = false;
    defineGlobalSymbol("readline", (...prompt) => readline(prompt[0] ?? "? "));
    function* charStreamPromptInput() {
      for(;;) {
        let indent = "";
        if (parseContext.currentToken?.type !== 'string' && parseContext.length > 0)
          indent =  " ".repeat(parseContext[parseContext.length-1].lineChar + 1);
        let line = readline(prompt + indent);
        if (line == null || endTest(line)) {
          quitRepl = true;
          return;
        }
        // Feed the charachers
        for (let ch of line)
          yield ch;
        // And then a newline
        yield '\n';
      }
    }
    let charStream = charStreamPromptInput();
    while (!quitRepl) {
      try {
        let expr = parseSExpr(charStream, opts);
        if (!expr) break;
        let evaluated = _eval(expr, scope);
        print(evaluated);
      } catch (error) {
        if (error instanceof SchemeError)
          reportSchemeError(error);
        else
          reportSystemError(error);
      }
    }
  }

  //
  // S-epression tokenizer and parser
  //
  
  class SchemeParseError extends SchemeError {};
  SchemeParseError.prototype.name = "SchemeParseError";
  defineGlobalSymbol("SchemeParseError", SchemeParseError);

  class SchemeSyntaxError extends SchemeParseError {
    path; errorToken; position; line; lineChar
    constructor(msg, path, errorToken) {
      let position = errorToken.position, line = errorToken.line, lineChar = errorToken.lineChar;
      if (path) msg = `${path}(${line},${lineChar}) ${msg}`;
      super(msg);
      this.path = path;
      this.errorToken = errorToken;
      this.position = position;
      this.line = line;
      this.lineChar = lineChar;
    }
  };
  SchemeSyntaxError.prototype.name = "SchemeSyntaxError";
  defineGlobalSymbol("SchemeSyntaxError", SchemeSyntaxError);

  class SchemeParseIncompleteError extends SchemeParseError {
    path; token; parseContext; position; line; lineChar;
    constructor(path, token, parseContext) {
      let position = token.position, line = token.line, lineChar = token.lineChar;
      let msg = "";
        if (path) msg = `${path}(${line},${lineChar}) ${msg}`;
      super(msg);
      this.path = path;
      this.token = token;
      this.parseContext = parseContext;
      this.position = position;
      this.line = line;
      this.lineChar = lineChar
    }
  };
  SchemeParseIncompleteError.prototype.name = "SchemeParseIncompleteError";
  defineGlobalSymbol("SchemeParseIncompleteError", SchemeParseIncompleteError);

  //
  // Character clases for parsing
  //
  const NBSP = '\u00a0', VTAB = '\u000b', FORMFEED = '\u000c', ELIPSIS = '\u0085';
  const NL = {}, SINGLE_CHAR_TOKENS = {}, QUOTES = {};
  const DIGITS = {}, HEXDIGITS = Object.create(DIGITS), NUM1 = {}, NUM2 = Object.create(NUM1);
  // IDENT2 includes IDENT1 by inheritence, as does WSNL WS.
  const IDENT1 = {}, IDENT2 = Object.create(IDENT1), WS = {}, WSNL = Object.create(WS);
  for (let ch of `0123456789`)
    DIGITS[ch] = IDENT2[ch] = NUM1[ch] = ch.codePointAt(0);
  for (let ch of `abcdefABCDEF'`)
    HEXDIGITS[ch] = ch.codePointAt(0);
  for (let ch of `+-.`)
    NUM1[ch] = ch.codePointAt(0);
  for (let ch of `eEoOxXbBn`)
    NUM2[ch] = ch.codePointAt(0);
  for (let ch of ` \t${VTAB}${FORMFEED}${NBSP}`) WS[ch] = ch.codePointAt(0);
  for (let ch of `\n\r`) NL[ch] = WSNL[ch] = ch.codePointAt(0);
  for (let ch of `()[]{}'.:,`) SINGLE_CHAR_TOKENS[ch] = ch.codePointAt(0);
  for (let ch of `\`"`) QUOTES[ch] = ch.codePointAt(0);
  globalScope.WS = WS;
  globalScope.NL = NL;

  // Drag Unicode character properties out of the RegExp engine.
  // This can take a bit of time and a LOT of memory, but people should
  // be able to use their own languages. By default it includes the
  // the Basic Multilingual Plane, but you can option it down to Latin-1
  // or up to include all the supplemental planes.
  // In addition to the memory used by the table I suspect the RegExp engine
  // drags in some libraries dynamically when the "u" flag is specified.
  // And for that matter using RegExp at all probably drags in a dynammic library
  // so, to reduce memory footprint, don't use it for Latin-1.

  // Basic Latin (ASCII)
  for (let codePoint = 0x21; codePoint < 0x7f; ++codePoint) {
    let ch = String.fromCodePoint(codePoint)
     // All printable charactes except single-char tokens, digits and quotes
    if (!SINGLE_CHAR_TOKENS[ch] && !DIGITS[ch] && !QUOTES[ch] &&!ch !== ';')
      IDENT1[ch] = codePoint;
  }
  // Latin-1 Supplement (all printable characters)
  for (let codePoint = 0xa1; codePoint <= 0xff; ++codePoint)
    if (codePoint !== 0xad)  // Exclude invisible soft-hyphen
      IDENT1[String.fromCodePoint(codePoint)] = codePoint;

  if (!latin1) {
    for (let codePoint = 0x100; codePoint < 0xD800; ++codePoint)
      analyzeCodepoint(codePoint);
    for (let codePoint = 0xE000; codePoint < 0x10000; ++codePoint)
      analyzeCodepoint(codePoint);
    if (supplemental) {
      for (let codePoint = 0x10000; codePoint < 0x110000; ++codePoint)
        analyzeCodepoint(codePoint);
    }
  }

  function analyzeCodepoint(codePoint) {
    let ch = String.fromCodePoint(codePoint);
    if (ch.match( /^\p{Alphabetic}$/u )) IDENT1[ch] = codePoint;
    if (ch.match( /^\p{Math}$/u )) IDENT1[ch] = codePoint;
    if (!NL[ch] && ch !== ELIPSIS && ch.match( /^\p{White_Space}$/u )) WS[ch] = codePoint;
  }

  if (dumpIdentifierMap) {
    if (typeof dumpIdentifierMap !== 'function')
      dumpIdentifierMap = console.info;
    showCodepoints("NL", NL);
    showCodepoints("WS", WS);
    showCodepoints("IDENT1", IDENT1);
    function showCodepoints(tableName, table) {
      let characters = Object.getOwnPropertyNames(table);
      process.stdout.write(`Table ${tableName}, ${characters.length} characters\n`);
      for (let ch of characters) {
        let charCode = table[ch];
        dumpIdentifierMap("CHARTAB", tableName, charCode, ch, jsChar(charCode));
      }
    }
  }
  
  defineGlobalSymbol("scheme-token-generator", schemeTokenGenerator, { dontInline: true });
  function* schemeTokenGenerator(characterSource, opts = {}) {
    let parseContext = opts.parseContext ?? [];
    let characterGenerator = iteratorFor(characterSource, LogicError);
    let ch = '', _peek = [], _done = false;
    let position = 0, charCount = 0, line = 0, lineCount = 0, lineChar = 0, lineCharCount = 0;
    if (!parseContext) parseContext = [];
    nextc();

    getToken:
    while (ch) {
      while (WS[ch])
        nextc();
      position = charCount-1;
      line = lineCount+1;
      lineChar = lineCharCount-1;

      if (NL[ch]) {
        yield { type: 'newline', position, line, lineChar };
        nextc();
        continue;
      }

      if (ch === ';') {  // ; begins a comment
        parseContext.push({ type: 'comment', value: ch === ';' ? ch : '//', position, line, lineChar });
        while (ch && !NL[ch])
          nextc();
        parseContext.currentToken = { type: 'endcomment', value: ';', endPosition: charCount-1, endWidth: 1, line, lineChar };
        parseContext.pop();
        yield { type: (ch ? 'newline': 'newline'), position, line, lineChar };
        continue;
      }

      if (ch === '/' && peekc() === '*') {
        parseContext.push({ type: 'comment', value: '/*', position, line, lineChar });
        nextc(); nextc();
        while (ch && !(ch === '*' && peekc() === '/'))
          nextc();
        parseContext.currentToken = { type: 'endcomment', value: '*/', endPosition: charCount-2, endWidth: 2, line, lineChar };
        parseContext.pop();
        if (!ch)
          yield { type: 'partial', position, line, lineChar };
        nextc(); nextc();
        continue;
      }

      if (ch === '"') {
        let str = '', multiline = false;
        let popped = false, tok = { type: 'string', value: '"', position, line, lineChar };
        parseContext.push(tok);
        parseContext.currentToken = tok;
        nextc();
        while (ch && ch !== '"' && (multiline || !NL[ch])) {
          if (ch === '\\') {
            nextc();
            if (NL[ch]) {  // traditional string continuation
              nextc();
              continue;
            } else if (ch === '\\' && NL[peekc()]) {  // Extended string continuation!
              nextc();
              multiline = true;
            } else if (ch === '') {
              break;
            } else if (ch === 'x') {  // \xXX
              nextc();
              if (!(HEXDIGITS[ch] && HEXDIGITS[peekc()]))
                throwSyntaxError('Invalid hexadecimal escape sequence');
              let str = `0x${ch}${peekc()}`;
              nextc();
              ch = String.fromCharCode(Number(str));
            } else if (ch === 'u') {  // \uXXXX or \u{X...}
              nextc();
              if (ch === '{') {
                let pos = 0, str = '0x';
                for ( ; HEXDIGITS[peekc(pos)]; ++pos)
                  str += peekc(pos);
                let codePoint = Number(str);
                if (peekc(pos) !== '}' || isNaN(codePoint))
                  yield { type: 'garbage', value: `\\u{${str}${peekc(pos)}`,  position, line, lineChar };
                if (codePoint >= 0x110000)
                  yield { type: 'garbage', value: `\\u{${str}}`,  position, line, lineChar };
                nextc();
                while (pos-- > 0) nextc();
                ch = String.fromCodePoint(codePoint);
              } else {
                if (!(HEXDIGITS[ch] && HEXDIGITS[peekc(0)] && HEXDIGITS[peekc(1)] && HEXDIGITS[peekc(2)]))
                  yield { type: 'garbage', value: `\\u${ch}${peekc(0)}${peekc(1)}${peekc(2)}`,  position, line, lineChar };
                let str = `0x${ch}${peekc(0)}${peekc(1)}${peekc(2)}`;
                nextc(), nextc(), nextc();
                ch = String.fromCharCode(Number(str));
              }
            } else {
              ch = ESCAPE_STRINGS[ch] ?? ch;
            }
          } else if (NL[ch]) {
            if (multiline) {
              nextc();
              while (WS[nextc()]) {}  // skips WS
              if (ch === '') break;
              if (ch === '+') {  // + continues
                nextc();
                continue;
              }
              if (ch === '|') {  // : newline then continues
                nextc();
                str += '\n';
                continue;
              }
              if (ch === '"') {  // " ends string
                break;
              }
            }
            parseContext.pop();
            popped = true;
            yield { type: 'garbage', value: '"' + str,  position, line, lineChar };
            continue getToken;  
          }
          str += ch;
          nextc();
        }
        if (!popped) {
          parseContext.pop();
        }
        if (!ch) {
          yield { type: 'partial', value: `"${str}`,  position, line, lineChar };
          return;
        }
        if (ch === '"') {
          yield { type: 'string', value: str, position, endPosition: charCount-1, endWidth: 1, line, lineChar };
          nextc();
        }
        continue;
      }

      if (ch === '.' && !DIGITS[peekc()]) {
        yield { type: ch, position, line, lineChar };
        nextc();
        continue;
      }

      // JS numbers are weird. The strategy here is to match anything that looks vaguely like a number
      // then let JS sort it all out.
      if (NUM1[ch]) {
        // Keep + and - from having to jump through unnecessary hoops
        let pc = peekc(), falsePlusMinus = (ch === '+' || ch === '-') && !(pc !== '.' || DIGITS[pc]);
        if (!falsePlusMinus) {
          let pos = 0, str = ch, value;
          for (;;) {
            let ch = peekc(pos++);
            if (!NUM2[ch])
              break;
            str += ch;
          }
          if (str.endsWith('n')) {
            str = str.substr(0, str.length-1);
            try {  // maybe it's a BigInt?
              value = BigInt(str);
            } catch (e) {
              // eat it
            }
          } else {
            let numVal = Number(str);
            if (!isNaN(numVal))
              value = numVal;
          }
          if (value !== undefined) {
            // Consume all the characters that we peeked and succeed
            while (pos-- > 0) nextc();
            yield { type: 'literal', value, position, line, lineChar };
            continue;
          }
        }
      }

      if (SINGLE_CHAR_TOKENS[ch]) {
        yield { type: ch, position, line, lineChar };
        nextc();
        continue;
      }

      if (IDENT1[ch]) {
        let str = '';
        while (ch && IDENT2[ch]) {
          // lambda symbols are special so we can parse \x as \ x
          if ((str[0] === '\\' || str[0] === LAMBDA_CHAR) && IDENT1[ch] && ch !== '#')
            break;
          str += ch, nextc();
        }
        if (str === 'true')
          yield { type: 'literal', value: true, position, line, lineChar };
        else if (str === 'false')
          yield { type: 'literal', value: false, position, line, lineChar };
        else if (str === 'undefined')
          yield { type: 'literal', value: undefined, position, line, lineChar };
        else if (str === 'null')
          yield { type: 'literal', value: null, position, line, lineChar };
        else
          yield { type: 'atom', value: Atom(str), position, line, lineChar };
        continue;
      }

      if (!ch) break;
      yield { type: 'garbage', value: ch, position, line, lineChar };
      nextc();
    }
    yield { type: 'end', position, line, lineChar };
    return;

    function nextc() {
      if (_peek.length > 0)
        return ch = _peek.shift();
      if (_done) return ch = '';
      let { done, value } = characterGenerator.next();
      if (done) {
        _done = true;
        return ch = '';
      }
      charCount += 1;
      lineCharCount += 1;
      // Among the [NL] chars, only use '\n' in the line count;
      // there may still be CRLFs in the wild.
      if (ch === '\n') {
        lineCount += 1;
        lineCharCount = 0;
      }
      return ch = value;
    }

    function peekc(n = 0) {
      for (let get = n - _peek.length + 1; get > 0; --get) {
        let { done, value } = characterGenerator.next();
        if (done) {
          _done = true;
          return '';
        }
        charCount += 1;
        lineCharCount += 1;
        if (NL[ch]) {
          lineCount += 1;
          lineCharCount = 0;
        }
        _peek.push(value);
      }
      return _peek[n];
    }
  }

  defineGlobalSymbol("parse", parseSExpr, { dontInline: true });
  exportAPI("parseSExpr", parseSExpr);
  function parseSExpr(characterSource, opts = {}) {
    opts = { ...schemeOpts, ...opts };
    let parseContext = opts.parseContext ?? [];
    opts.parseContext = parseContext;
    parseContext.length = 0;
    let path = opts.path;
    let assignSyntax = opts.assignSyntax ?? false;
    let _tokens = [], _done = false;
    if (typeof characterSource === 'string')
      characterSource = iteratorFor(characterSource);
    let tokenGenerator = schemeTokenGenerator(characterSource, opts);
    let expr;
    if (assignSyntax && token().type === 'atom' &&
        token(1).type === 'atom' && token(1).value === Atom('=')) {
      // Allows the REPL to select a mode where, at the top-level only,
      //    a = expr
      // is the same as
      //    (def a expr)
      // It might or might not be a good idea. It isn't worse than "evalquote", if anyone
      // else remembers that. In any case it's opt-in.
      let sym = token().value;
      parseContext.push({ type: 'assign', value: sym });
      consumeToken(), consumeToken();
      let assigned = parseExpr();
      parseContext.pop();
      expr = list(Atom("def"), sym, assigned);
    } else {
      expr = parseExpr(0);
    }
    let initialNewlineOK = true;
    let tok = token(0, initialNewlineOK);
    if (tok.type === 'newline' || tok.type === 'end')
      return expr;
    throwSyntaxError();

    function parseExpr() {
      if (token().type === 'string' || token().type === 'literal') {
        let thisToken = consumeToken();
        return thisToken.value;
      }

      if (token().type === 'atom') {
        let thisToken = consumeToken();
        return thisToken.value;
      }

      if (token().type === '(') {
        parseContext.push(token());
        consumeToken();
        return parseListBody();
        function parseListBody() {
          let head = NIL, tail;
          for (;;) {
            if (token().type === ')') {
              parseContext.pop();
              consumeToken();
              return head;
            } else if (token().type === '.') {
              consumeToken();
              let val = parseExpr();
              parseContext.pop();
              if (token().type !== ')') throwSyntaxError();
              consumeToken();
              if (tail) tail[REST] = val;
              else head = val;
              return head;
            }
            if (token().type === 'end' || token().type === 'partial')
              throw new SchemeParseIncompleteError(path, token(), parseContext);
            if (token().type === 'garbage') throwSyntaxError();
            let item = parseExpr();
            item = cons(item, NIL);
            if (tail) tail = tail[REST] = item;
            else head = tail = item;
          }
        }
      }

      if (token().type === '[') {  // JavaScript Array
        let res = [];
        parseContext.push(token());
        consumeToken();
        for (;;) {
          if (token().type === ']') {
            parseContext.pop();
            consumeToken();
            return res;
          }
          let item = parseExpr();
          res.push(item);
          if (token().type === ',')  // Comma is optional
            consumeToken();
          if (token().type === 'end' || token().type === 'partial')
            throw new SchemeParseIncompleteError(path, token(), parseContext);
        }
      }

      if (token().type === '{') {  // JavaScript Object
        let res = {}, evalCount = 0;
        parseContext.push(token());
        consumeToken();
        for (;;) {
          if (token().type === '}') {
            parseContext.pop();
            consumeToken();
            break;
          }
          let gotIt = false;
          if (token().type === 'atom' || token().type === 'string'
                || token().type === 'literal' || token().type === '[') {
            let evaluatedKey = false, sym;
            if (token().type === '[') {
              parseContext.push(token());
              consumeToken();
              sym = parseExpr();
              parseContext.pop();
              if (token().type !== ']') break;
              evaluatedKey = true;
              consumeToken();
            } else {
              sym = token().value;
              // Don't convert symbols to strings; symbols can be keys.
              if (!(typeof sym === 'string' || typeof sym === 'symbol'))
                sym = String(sym);
              consumeToken();
            }
            if (token().type === ':') {
              parseContext.push(token());
              consumeToken();
              let val = parseExpr();
              parseContext.pop();
              if (evaluatedKey)
                res[EVALUATE_KEY_VALUE] = [sym, val];
              else
                res[sym] = val;
            }
            gotIt = true;
            if (token().type === ',')  // Comma is optional
              consumeToken();
            if (token().type === 'end'|| token().type === 'partial')
              throw new SchemeParseIncompleteError(path, token(), parseContext);
          }
          if (!gotIt) throwSyntaxError();
        }
        return res;
      }

      if (token().type === "'") {
        consumeToken();
        parseContext.push(token());
        let quoted = parseExpr();
        return cons(QUOTE_ATOM, cons(quoted, NIL));
      }
      if (token().type === 'end')
        return null;
      throwSyntaxError();
    }

    function token(n = 0, initialNewlineOK) {
      // Two main ideas here, mostly in support of the REPL:
      // (1) Don't read anything until absolutely necessary.
      // (2) Foil attempts to peek for tokens across a line boundary by leaving
      //     'newline' tokens in the peek buffer. But to simplify the parser,
      //      token(0) skips past any newline tokens.
      for (;;) {
        for (let get_minus_1 = n - _tokens.length; get_minus_1 >= 0 && !_done; --get_minus_1) {
          let { done, value } = tokenGenerator.next();
          if (done) {
            _done = true;
          } else {
            _tokens.push(value);
            parseContext.currentToken = value;
          }
        }
        // Never return a 'newline' as the current token (unless told otherwise)
        if (!initialNewlineOK) {
          while (_tokens.length > 0 && _tokens[0].type === 'newline')
            _tokens.shift();
        }
        if (n < _tokens.length)
          return _tokens[n];
        if (_done)
          return { type: 'end' };
      }
    }

    function consumeToken() {
      return _tokens.shift();
    }

    function throwSyntaxError() {
      let str = "", errorToken = token();
      if (errorToken.type === 'partial')
        throw new SchemeParseIncompleteError(path, errorToken, parseContext)
      let newline = false;
      while (_tokens.length > 0) {
        // "detokenize" any lookahead tokens
        let token = _tokens.pop();
        newline = (token.type === 'newline');
        str += (token.value !== undefined ? string(token.value) : token.type);
        str += " ";
      }
      while (!newline) {
        if (_done) break;
        let { done, value: ch } = characterSource.next();
        if (done) {
          _done = true;
          break;
        }
        if (NL[ch]) break;
        str += ch;
      }
      throw new SchemeSyntaxError(str, path, errorToken);
    }
  }

  //
  // Bindings!
  //

  if (defineBindings) {
    defineBinding("cons", "cons", {
      group: "main", sample: `(cons a b)`, 
      blurb: `Constructs a Pair, a.k.a, a list node.`
    });
    defineBinding("car", "car", "first", {
      group: "main", sample: `(car list)`, 
      blurb: `Returns the first element of a list`
    });
    defineBinding("cdr", "cdr", "rest", {
      group: "main", sample: `(car list)`, 
      blurb: `Returns the first element of a list.`
    });
    defineBinding("NIL", "nil",, {
      group: "main", sample: `nil`, 
      blurb: `An empty list.`
    });
    defineBinding("undefined", "undefined", {
      group: "main", sample: `undefined`, 
      blurb: `The JavaScript "undefined" value.`
    });
    defineBinding("null", "null", {
      group: "main", sample: `null`, 
      blurb: `The JavaScript "null" value.`
    });
    defineBinding("true", "true", {
      group: "main", sample: `true`, 
      blurb: `The JavaScript "true" value.`
    });
    defineBinding("false", "false", {
      group: "main", sample: `false`, 
      blurb: `The JavaScript "false" value.`
    });
    defineBinding("NaN", "NaN", {
      group: "main", sample: `NaN`, 
      blurb: `The JavaScript "NaN" value.`
    });
    defineBinding("Infinity", "Infinity", {
      group: "main", sample: `Infinity`, 
      blurb: `The JavaScript "Infinity" value.`
    });
    defineBinding("globalScope", "globalScope", {
      group: "main", sample: `globalScope`, 
      blurb: `The Scheme "globalScope" value.`
    });
    defineBinding("globalThis", "globalThis", {
      group: "main", sample: `globalThis`, 
      blurb: `The JavaScript "globalThis" value.`
    });
    defineBinding("typeof", "typeof", {
      group: "main", sample: `(typeof object)`, 
      blurb: `The JavaScript type of the object.`
    });
    defineBinding("not", "!", "not", {
      group: "logical-op", sample: `(! value)`, 
      blurb: `Logical "not. If "value" is a function, returns a function with a negated result."`
    });
    defineBinding("and", "&&", "and", {
      group: "logical-op", sample: `(&& value ...)`, 
      blurb: `Logical "and." Stops evaluating and returns the first value that is false; otherwise returns the last value.`
    });
    defineBinding("or", "||", "or", {
      group: "logical-op", sample: `(|| value ...)`, 
      blurb: `Logical "or." Stops evaluating and returns the first value that is true; otherwise returns the last value.`
    });
    defineBinding("nullish", "??", "nullish", {
      group: "logical-op", sample: `(xxx value ...)`, 
      blurb: `"Nullish" operator. Stops evaluating and returns the first value that neither null or undefined; otherwise returns the last value.`
    });
    defineBinding("bit_not", "~", "bit-not", {
      group: "bitwise-op", sample: `(~ value)`, 
      blurb: `Bitwise XXX of each value.`
    });
    defineBinding("bit_or", "|", "bit-or", {
      group: "bitwise-op", sample: `(| value ...)`, 
      blurb: `Bitwise "or" of each value.`
    });
    defineBinding("bit_and", "&", "bit-and", {
      group: "bitwise-op", sample: `(| value ...)`, 
      blurb: `Bitwise "and" of each value.`
    });
    defineBinding("bit_xor", "^", "bit-xor", {
      group: "bitwise-op", sample: `(| value ...)`, 
      blurb: `Bitwise "exclusive or" of each value.`
    });
    defineBinding("bit_shl", "<<", "bit-shl", {
      group: "bitwise-op", sample: `(<< value shift)`,
      blurb: `Left shift of "value" considered as a 32-bit integer by "shift" bits.`
    });
    defineBinding("bit_shr", ">>", "bit-shr", {
      group: "bitwise-op", sample: `(<< value shift)`,
      blurb: `Right shift of "value" considered as a 32-bit integer by "shift" bits.`
    });
    defineBinding("bit_ash", ">>>", "bit-ash", {
      group: "bitwise-op", sample: `(>> value shift)`,
      blurb: `Arithmetic right shift of "value" considered as a 32-bit integer by "shift" bits, extending the sign bit`
    });
    defineBinding("add", "+", "add", {
      group: "arith-op", sample: `(+ value ...)`, 
      blurb: `Adds each value.`
    });
    defineBinding("sub", "-", "sub", {
      group: "arith-op", sample: `(- value ...)`, 
      blurb: `Subtracts each value from the first. If only one value is given, it is negated.`
    });
    defineBinding("mul", "*", MUL, "mul", {
      group: "arith-op", sample: `(* value ...)`, 
      blurb: `Multiplies each value.`
    });
    defineBinding("div", "/", DIV, "div", {
      group: "arith-op", sample: `(/ value ...)`, 
      blurb: `Divides the first value by each subsequnt value. If only one value is given, it is inverted.`
    });
    defineBinding("rem", "%", "rem", {
      group: "arith-op", sample: `(% value value)`, 
      blurb: `Remainder of the first value divided by the second.`
    });
    defineBinding("pow", "**", "pow", {
      group: "arith-op", sample: `(** value value)`, 
      blurb: `The first value taken to the power of the second.`
    });
    defineBinding("lt", "<", "lt", {
      group: "compare-op", sample: `(< value ...)`, 
      blurb: `Returns true if each value is less than the previous. Evaluation ends as soon as the comparison fails.`
    });
    defineBinding("le", "<=", "le", {
      group: "compare-op", sample: `(<= value ...)`, 
      blurb: `Returns true if each value is less than or equal the previous. Evaluation ends as soon as the comparison fails.`
    });
    defineBinding("gt", ">", "gt", {
      group: "compare-op", sample: `(> value ...)`, 
      blurb: `Returns true if each value is XXX than the previous. Evaluation ends as soon as the comparison fails.`
    });
    defineBinding("ge", ">=", "ge", {
      group: "compare-op", sample: `(>= value ...)`, 
      blurb: `Returns true if each value is greater than or equal the previous. Evaluation ends as soon as the comparison fails.`
    });
    defineBinding("eq", "==", {
      group: "compare-op", sample: `(== value ...)`, 
      blurb: `Returns true if each value is equal to the previous, in the JavaScript "==" sense. Evaluation ends as soon as the comparison fails.`
    });
    defineBinding("eeq", "===", {
      group: "compare-op", sample: `(=== value ...)`, 
      blurb: `Returns true if each value is equal to the previous, in the JavaScript "===" sense. Evaluation ends as soon as the comparison fails.`
    });
    defineBinding("neq", "!=", {
      group: "compare-op", sample: `(!= value ...)`, 
      blurb: `Returns true unless each value is equal to the previous, in the JavaScript "==" sense. Evaluation ends as soon as the comparison fails.`
    });
    defineBinding("neeq", "!==", {
      group: "compare-op", sample: `(!== value ...)`, 
      blurb: `Returns true unless each value is equal to the previous, in the JavaScript "===" sense. Evaluation ends as soon as the comparison fails.`
    });
    defineBinding("equal", "equal" {
      group: "compare-op", sample: `(equal value value [options])`, 
      blurb: `Returns true if the two values are "deeply equal`
    });
    defineBinding("nequal", "nequal" {
      group: "compare-op", sample: `(equal value value [options])`, 
      blurb: `Returns false if the two values are "deeply equal`
    });
    defineBinding("if", "?", "if", {
      group: "pred-op", sample: `(? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is "true" in the Scheme sense (neither false, undefined, null nor nil), ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_bigint", "bigint?", {
      group: "pred-op", sample: `(?bigint value [t-expr true] [f-expr false])`, 
      blurb: `If the value's type is "bigint," ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_atom", "atom?", {
      group: "pred-op", sample: `(atom? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is an atom ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_list", "list?", {
      group: "pred-op", sample: `(list? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is a list, ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_undefined", "undefined?", {
      group: "pred-op", sample: `(undefined? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is "undefined," ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_null", "null?", {
      group: "pred-op", sample: `(null? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is JavaScript's null, exactly, ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_nullish", "nulish?", {
      group: "pred-op", sample: `(nullish? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is Javascript's null or undefined, " ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_nil", "nil?", {
      group: "pred-op", sample: `(null? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is nil, ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_boolean", "boolean?", {
      group: "pred-op", sample: `(boolean? value [t-expr true] [f-expr false])`, 
      blurb: `If the value's type is boolean, ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_number", "number?", {
      group: "pred-op", sample: `(number? value [t-expr true] [f-expr false])`, 
      blurb: `If the value's type is "number," ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_numeric", "numeric?", {
      group: "pred-op", sample: `(numeric? value [t-expr true] [f-expr false])`, 
      blurb: `If the value's type is 'number' or 'bigint'. ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_string", "string?", {
      group: "pred-op", sample: `(string? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is a String, " ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_symbol", "symbol?", {
      group: "pred-op", sample: `(symbol? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is a Symbol, " ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_function", "function?", {
      group: "pred-op", sample: `(function? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is a function, ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_object", "object?", {
      group: "pred-op", sample: `(xxx? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is an Onject (but not a function), ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_array", "array?", {
      group: "pred-op", sample: `(xxx? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is an array, ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_NaN", "NaN?", {
      group: "pred-op", sample: `(NaN? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is not a number (ugh, although its type may be 'number'), ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_finite", "finite?", {
      group: "pred-op", sample: `(finite? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is +/-infinity or NaN, ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("is_xxx", "xxx?", {
      group: "pred-op", sample: `(xxx? value [t-expr true] [f-expr false])`, 
      blurb: `If the value is xxx, ` +
         `evaluates t-expr and returns its value (default true). ` +
         `Otherwise, evaluates and returns f-expr (default false).`
    });
    defineBinding("begin", "begin", {
      group: "flow-op", sample: `(begin expr ...)`, 
      blurb: `Evaluates each expression in turn, resulting in the value of the last.`
    });
    defineBinding("xxx", "xxx", {
      group: "flow-op", sample: `(xxx expr ...)`, 
      blurb: `Evaluates each expression in turn, xxx.`
    });
    defineBinding("xxx", "xxx", {
      group: "flow-op", sample: `(xxx expr ...)`, 
      blurb: `Evaluates each expression in turn, xxx.`
    });
    defineBinding("xxx", "xxx", {
      group: "flow-op", sample: `(xxx expr ...)`, 
      blurb: `Evaluates each expression in turn, xxx.`
    });
    defineBinding("xxx", "xxx", {
      group: "flow-op", sample: `(xxx expr ...)`, 
      blurb: `Evaluates each expression in turn, xxx.`
    });
    
    defineBinding((a,b) => a in b, "in", { group: "js-op", sample: `(in a b)` });
    defineBinding((cls, ...args) => new cls(...args), "new", { group: "js-op", sample: `(new cls . args)` });
    defineBinding((obj, cls) => obj instanceof cls, "instanceof", { group: "js-op", sample: `(instanceof obj cls)` });
    defineBinding((a, b) => a[b], "@", { group: "js-op", sample: `(@ a b)` });
    defineBinding((a, b, c) => a[b][c], "@@", { group: "js-op", sample: `(@@ a b c)` });
    defineBinding((a, b, c, d) => a[b][c][d], "@@@", { group: "js-op", sample: `(@@@ a b c d)` });
    defineBinding((a, b) => a?.[b], "@?", { group: "js-op", sample: `(@? a b)` });
    defineBinding((a, b, c) => a?.[b]?.[c], "@@?", { group: "js-op", sample: `(@@? a b c)` });
    defineBinding((a, b, c, d) => a?.[b]?.[c]?.[d], "@@@?", { group: "js-op", sample: `(@@@? a b c d)` });
    defineBinding((a, b, ...params) => a[b](...params), "@!", { group: "js-op", sample: `(@! a b . params)` });
    defineBinding(xxx, "xxx", { group: "js-op", sample: `(xxx xxx)` });
    defineBinding(xxx, "xxx", { group: "js-op", sample: `(xxx xxx)` });
    defineBinding(xxx, "xxx", { group: "js-op", sample: `(xxx xxx)` });
    defineBinding(xxx, "xxx", { group: "js-op", sample: `(xxx xxx)` });
    defineBinding(xxx, "xxx", { group: "js-op", sample: `(xxx xxx)` });
    defineBinding(xxx, "xxx", { group: "js-op", sample: `(xxx xxx)` });
    defineBinding(xxx, "xxx", { group: "js-op", sample: `(xxx xxx)` });
    defineBinding(xxx, "xxx", { group: "js-op", sample: `(xxx xxx)` });

    
    exportApi("@!", (a, b, ...params) => a[b](...params), { group: "js-op" });
    exportApi("@@!", (a, b, c, ...params) => a[b][c](...params), { group: "js-op" });
    exportApi("@@@!", (a, b, c, d, ...params) => a[b][c][d](...params), { group: "js-op" });
    exportApi("@?!", (a, b, ...params) => a?.[b](...params), { group: "js-op" });
    exportApi("@@?!", (a, b, c, ...params) => a?.[b]?.[c](...params), { group: "js-op" });
    exportApi("@@@?!", (a, b, c, d, ...params) => a?.[b]?.[c]?.[d](...params), { group: "js-op" });
    exportApi("@=", (a, b, c) => a[b] = c, { group: "js-op" }, "js-assign");
    exportApi("@@=", (a, b, c, d) => a[b][c] = d), { group: "js-op" };
    exportApi("@@@=", (a, b, c, d, e) => a[b][b][c] = d, { group: "js-op" });
    exportApi("delete", (a, b) => delete a[b]), { schemeOnly: true, group: "js-op" };
    exportApi("void", _ => undefined, { schemeOnly: true, group: "js-op" });
    exportApi("not", a => typeof a === 'function' ? ((...params) => !a(...params)) : !a), { group: "logical-op" };

    
    defineBinding("in", "in", {
      group: "js-op", sample: `(in property object)`, 
    });
    defineBinding("at", "@", {
      group: "js-op", sample: `(@ object key)`, 
    });


    defineBinding("xxx", "xxx", "xxx", {
      group: "compare-op", sample: `(xxx value ...)`, 
      blurb: `Returns true if each value is XXX than the previous. Evaluation ends as soon as the comparison fails.`
    });
    defineBinding("xxx", "xxx", "xxx", {
      group: "compare-op", sample: `(xxx value ...)`, 
      blurb: `Returns true if each value is XXX than the previous. Evaluation ends as soon as the comparison fails.`
    });
    defineBinding("xxx", "xxx", "xxx", {
      group: "compare-op", sample: `(xxx value ...)`, 
      blurb: `Returns true if each value is XXX than the previous. Evaluation ends as soon as the comparison fails.`
    });
    defineBinding("xxx", "xxx", "xxx", {
      group: "logical-op", sample: `(xxx value)`, 
      blurb: `Logical XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
    defineBinding("XXX", "XXX",, {
      group: "main", sample: `XXX`, 
      blurb: `XXX.`
    });
  
  }

  return globalScope;

}