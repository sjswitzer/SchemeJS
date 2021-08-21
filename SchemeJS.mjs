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

// So that optional parameters show up pretty when printed
const optional = undefined;

//
// Creates a SchemeJS instance.
//
export function createInstance(schemeOpts = {}) {
  let globalScope = SchemeJS.createInstance(schemeOpts);
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
  let _eval = globalScope._eval ?? required();
  let compare_hooks = globalScope.compare_hooks ?? required();
  let Atom = globalScope.Atom ?? required();
  let isAtom = globalScope.isAtom ?? required();
  let cons = globalScope.cons ?? required();
  let car = globalScope.car ?? required();
  let cdr = globalScope.cdr ?? required();
  let schemeTrue = globalScope.schemeTrue ?? required();
  let parseSExpr = globalScope.parseSExpr ?? required();
  function required() { throw "required" }

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
  defineGlobalSymbol("equal?", equal, { usesDynamicScope: false, dontInline: true });
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
        for ( ; moreList(aRest) && moreList(bRest); ++i, ++length, aRest = aRest[REST], bRest = bRest[REST]) {
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
  defineGlobalSymbol("mergesort", mergesort, { usesDynamicScope: false, dontInline: true, group: "list-op" }, "sort", "qsort");
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

  defineGlobalSymbol("in-place-mergesort", in_place_mergesort, { usesDynamicScope: false, dontInline: true, group: "list-op" }, "in-place-sort", "nsort");
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

  defineGlobalSymbol("intern", Atom, { usesDynamicScope: false, dontInline: true });

  defineGlobalSymbol("copy-list", copy_list, { usesDynamicScope: false, dontInline: true, group: "list-op" });  // TODO: unit tests!
  function copy_list(...lists) {
    let res = NIL, last;
    for (let list of lists) {
      if (isNil(list)) return NIL;
      if (iterateAsList(list)) {
        for ( ; moreList(list); list = list[REST]) {
          let item = cons(list[FIRST], NIL);
          if (last) last = last[REST] = item;
          else res = last = item;
        }
      } else if (isIterable(list)) {
        for (let item of list) {
          item = cons(item, NIL);
          if (last) last = last[REST] = item;
          else res = last = item;
          list = list[REST];
        }
      } else {
        throw new TypeError(`Not a list or iterable ${list}`);
      }
    }
    return res;
  }

  // (apropos substring) -- Returns a list of all atoms containing the given substring in their names
  defineGlobalSymbol("apropos", apropos, { dontInline: true });
  function apropos(substring) {
    if (!substring) substring = "";
    substring = substring.toLowerCase();
    let matches = NIL, scope = this;
    for ( ; scope && scope !== Object; scope = Object.getPrototypeOf(scope)) {
      let symbols = Object.getOwnPropertySymbols(scope);
      for (let symbol of symbols) {
        if (!isAtom(symbol)) continue;
        let name = string(symbol);
        if (name.toLowerCase().includes(substring))
          matches = cons(symbol, matches);
      }
    }
    return this.nsort(matches,
      (a,b) => a.description.toLowerCase() < b.description.toLowerCase());
  }

  // Pokemon gotta catch 'em' all!
  defineGlobalSymbol("!", a => !schemeTrue(a), { group: "logical-op" });
  defineGlobalSymbol("~", a => ~a, { group: "bitwise-op" }, "bit-not");
  defineGlobalSymbol("**", (a,b) => a ** b, { /* classification? */ }, "pow");  // overrides Math.pow
  defineGlobalSymbol("%", (a,b) => a % b, { /* classification? */ }, "rem");
  defineGlobalSymbol("<<", (a,b) => a << b, { group: "bitwise-op" }, "bit-shl");
  defineGlobalSymbol(">>", (a,b) => a >> b, { group: "bitwise-op" }, "bit-shr");
  defineGlobalSymbol(">>>", (a,b) => a >>> b, { group: "bitwise-op" }, "bit-ushr");
  defineGlobalSymbol("ash", (a, b) => b < 0 ? a >>> -b : a << b, { schemeOnly: true, group: "bitwise-op" });  // SIOD
  defineGlobalSymbol("in", (a,b) => a in b, { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("new", (cls, ...args) => new cls(...args), { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("instanceof", (a,b) => a instanceof b, { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("@", (a, b) => a[b], { group: "js-op" }, "aref");  // indexing and member access (SIOD: aref)
  defineGlobalSymbol("@@", (a, b, c) => a[b][c], { group: "js-op" });
  defineGlobalSymbol("@@@", (a, b, c, d) => a[b][c][d], { group: "js-op" });
  defineGlobalSymbol("@?", (a, b) => a?.[b], { group: "js-op" });  // conditional indexing and member access
  defineGlobalSymbol("@@?", (a, b, c) => a?.[b]?.[c], { group: "js-op" });
  defineGlobalSymbol("@@@?", (a, b, c, d) => a?.[b]?.[c]?.[d], { group: "js-op" });
  defineGlobalSymbol("@!", (a, b, ...params) => a[b](...params), { group: "js-op" });
  defineGlobalSymbol("@@!", (a, b, c, ...params) => a[b][c](...params), { group: "js-op" });
  defineGlobalSymbol("@@@!", (a, b, c, d, ...params) => a[b][c][d](...params), { group: "js-op" });
  defineGlobalSymbol("@?!", (a, b, ...params) => a?.[b](...params), { group: "js-op" });
  defineGlobalSymbol("@@?!", (a, b, c, ...params) => a?.[b]?.[c](...params), { group: "js-op" });
  defineGlobalSymbol("@@@?!", (a, b, c, d, ...params) => a?.[b]?.[c]?.[d](...params), { group: "js-op" });
  defineGlobalSymbol("@=", (a, b, c) => a[b] = c, { group: "js-op" }, "js-assign");
  defineGlobalSymbol("@@=", (a, b, c, d) => a[b][c] = d), { group: "js-op" };
  defineGlobalSymbol("@@@=", (a, b, c, d, e) => a[b][b][c] = d, { group: "js-op" });
  defineGlobalSymbol("delete", (a, b) => delete a[b]), { schemeOnly: true, group: "js-op" };
  defineGlobalSymbol("void", _ => undefined, { schemeOnly: true, group: "js-op" });

  defineGlobalSymbol("not", a => typeof a === 'function' ? ((...params) => !a(...params)) : !a), { group: "logical-op" };

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

  return globalScope;

}