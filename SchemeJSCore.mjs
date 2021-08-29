//
// SchemeJS: Scheme in JavaScript
//
// Copyright 2021 Stan Switzer -- (sjswitzer [at] gmail [dot] com)
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

export const VERSION = "1.1 (alpha)";
export const LogicError = Error;
const isArray = Array.isArray;
const MUL = '\u00d7', DIV = '\u00f7';

// So that optional parameters show up pretty when printed
const optional = undefined;

//
// Creates a SchemeJSCore instance.
//
// Instances are distinct to the bones; they do not even recognize each other's
// Pairs, Atoms or NIL values. This is by design. People should be able
// to poke things into class definitions to experiment with different ideas
// but that should only affect that specific SchemeJS instance; others should
// be unaffected.
//
// Broadly speaking SchemeJSCore hardly knows it implements Scheme; rather it is
// more like an AST for JavaScript that happens to be defined in terms of lists,
// is executable, compilable, and has a JIT. Every function defined by the core
// (and, hence, in SchemeJS or any other language built on the core) is a JavaScript
// function whether it is compiled or not. Compiled code is perfectly normal
// JavaScript that executes just as fast as the JavaScript you'd write by hand.
//
// Lists can be built from Cons cells (Pairs) but Arrays, Iterables, and generator
// functions are lists too (or can be, subject to instantiation options.)
//
// The core module defines listness, implements an interpreter, a compiler and JIT,
// and defines a suite of operations corresponding to JavaScript language primitives.
// Lispy operations are defined in SchemeJS, as well as optional language bindings
// for a Scheme implemnentation. The core itself is language-agnostic and can simply
// be used as a handy dynamic code geration toolkit.
//
export function createInstance(schemeOpts = {}) {
  const JIT_THRESHOLD = schemeOpts.jitThreshold ?? undefined;
  const TRACE_INTERPRETER = !!(schemeOpts.traceInterpreter ?? false);
  const TRACE_COMPILER = !!(schemeOpts.traceCompiler ?? false);
  const TRACE_COMPILER_CODE = !!(schemeOpts.traceCompilerCode ?? false);
  const generatorsAreLists = schemeOpts.generatorsAreLists ?? true;
  const standardIterablesAreLists = schemeOpts.standardIterablesAreLists ?? true;
  const lambdaStr = schemeOpts.lambdaStr ?? "\\";
  const firstName = schemeOpts.firstName ?? "first";
  const restName = schemeOpts.firstName ?? "rest";
  const nilName = schemeOpts.nilName ?? "NIL";
  const schemeTrueOverride = schemeOpts.schemeTrueOverride;
  const bottomIsLNIL = schemeOpts.bottomIsLNIL ?? true;
  const restParamStr = schemeOpts.restParamStr ?? "&";

  const COMPILE_INFO = Symbol("COMPILE-INFO");
  const COMPILED = Symbol("SchemeJS-COMPILED"), JITCOMPILED = Symbol("SchemeJS-JITCOMPILED");
  const PARAMETER_DESCRIPTOR = Symbol('SchemeJS-PARAMETER-DESCRIPTOR');
  const MAX_INTEGER = (2**31-1)|0;  // Presumably allows JITs to do small-int optimizations
  const analyzedFunctions = new WeakMap(), namedObjects = new WeakMap();
  const JSIDENT1 = {}, JSIDENT2 = Object.create(JSIDENT1), WS = {}, WSNL = Object.create(WS);
  for (let ch of `abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$`)
    JSIDENT1[ch] = ch.codePointAt(0);
  for (let ch of "0123456789")
    JSIDENT2[ch] = ch.codePointAt(0);
  for (let ch of " \t")
    WS[ch] = ch.codePointAt(0);
  for (let ch of "\n\r")
    WSNL[ch] = ch.codePointAt(0);

  // Three kinds of macros:
  //   MACRO_TAG: Conventional macro
  //      Called with the remainder of the form.
  //      Returns what to replace the form it heads with
  //   PARAMETER_MACRO_TAG: For every form element, including the first
  //      Called with the remainder of the form.
  //      Returns the replacement for itself and successors.
  //   EVALUATED_PARAMETER_MACRO_TAG: For evaluated parameters only
  //      Called with the remainder of the form.
  //      Returns the replacement for itself and successors.
  const FUNCTION_TAG = 0, MACRO_TAG = 1, EVALUATED_PARAMETER_MACRO_TAG = 2, PARAMETER_MACRO_TAG = 3;

  // Create a new scope with newScope(enclosingScope, "description").
  // A new scope's prototype is the enclosing scope.
  // This way, a scope chain is a prototype chain and resolving
  // a symbol is as simple as "scope[sym]"
  //
  // SCOPE_TYPE_SYMBOL hints the printer that it's a scope... and what kind.
  const SCOPE_TYPE_SYMBOL = Symbol("SCOPE_IS");
  let globalScope = new Object();
  globalScope[SCOPE_TYPE_SYMBOL] = "global-scope";
  let helpInfo = globalScope._helpInfo_ = {};
  helpInfo.jsAPI = {};

  exportAPI("newScope", newScope);
  function newScope(enclosingScope, scope_type) {
    let scope = Object.create(enclosingScope);
    scope[SCOPE_TYPE_SYMBOL] = scope_type;
    return scope;
  }

  exportAPI("exportAPI", exportAPI, { dontInline: true })
  function exportAPI(name, value, opts) {
    if (typeof value === 'function')
      examineFunctionForCompilerTemplates(name, value, opts);
    globalScope[name] = value;
    helpInfo.jsAPI[name] = value;
  }

  exportAPI("COMPILE_INFO", COMPILE_INFO);
  exportAPI("FUNCTION_TAG", FUNCTION_TAG  );
  exportAPI("MACRO_TAG", MACRO_TAG);
  exportAPI("PARAMETER_MACRO_TAG", PARAMETER_MACRO_TAG);
  exportAPI("EVALUATED_PARAMETER_MACRO_TAG", EVALUATED_PARAMETER_MACRO_TAG);

  //
  // Unlike most Lisps, the Cons cell (Pair) is not central to this design, but a _list_ is.
  // A list is like an iterator and all lists are iterable, but a list is different
  // frmm an iterator in that accessing the "next" element does not "consume"
  // an item and change the state of the list; a list is stateless and its "next"
  // (REST) can always be accessed.
  //
  // Cons cells (pairs) are a kind of list but are no more a list than any other.
  // To be a list requires only to implement the list protocol.
  //
  // The list protocol is defined by a set of properties implemented (or not)
  // by an object. There are a lot of properties but the idea is that, although property
  // access is cheap, I still want each predicate to test one and only one property.
  //
  //   MORELIST -- A property which, if true, says that there are FIRST and REST
  //               properties. If false (exactly), the list is a NIL VALUE.
  //   FIRST    -- The first item in the list ("car," if you will).
  //   REST     -- The remainder of the list.
  //   LIST     -- If true, the item is a list (null is also a list... of nothing)
  //               This allows a quick check wheter an object is a list or not.
  //
  // Additionally, some optional properties that mostly hint the printer
  // to keep it from doing bad things like invoking iterators behind your back
  //
  //   LAZYFIRST -- The dynamic evaluator for the FIRST property.
  //   LAZYREST  -- The dynamic evaluator for the REST property.
  //   SUPERLAZY -- Even the MORELIST property is dynamically evaluated.
  //                The list doesn't even know yet whether it is null or not.
  //
  // Finally, since all lists are iterable and all iterables can be lists,
  // a property to indicate which is the "natural" iteration strategy for that object.
  //
  //   ITERATE_AS_LIST -- Use this idiom to iterate:
  //     for (let current = list; moreList(current); current = current[REST]) {
  //       let item = current[FIRST];
  //        ...
  //      }
  //    Otherwise, use this idiom:  (I trust JavaScript to optimize the case of Arrays here)
  //      for (item of obj) {
  //        ...
  //      }
  //
  // But there is an even better way to iterate:
  //      let current = obj;
  //      for ( ; iterateAsList(current); current = current[REST])
  //         ...;
  //      if (!isNil(current)) {
  //        for (let tmp of obj)
  //          ...;
  //      }
  // Because a list can start out list iterable and end up normally iterable,
  // for example, (cons 1 [2 4 4])) is a perfectly good list. See the implementation
  // of "length" for an example.
  //

  // Since these symbols can be tagged on external JS functions and objects, label them as ours as a courtesy.
  const FIRST = Symbol("SchemeJS-FIRST"), REST = Symbol("SchemeJS-REST");
  const LIST = Symbol("SchemeJS-LIST"), MORELIST = Symbol("SchemeJS-MORELIST");
  const LAZYFIRST = Symbol("SchemeJS-LAZYFIRST"), LAZYREST = Symbol("SchemeJS-LAZYREST"), SUPERLAZY = Symbol("SchemeJS-SUPERLAZY");
  let ITERATE_AS_LIST = Symbol("SchemeJS-ITERATE-AS-LIST");

  // I trust JITs to inline these
  const isList = obj => obj != null && obj[LIST] === true;
  const isNil = obj => obj != null && obj[MORELIST] === false;
  const iterateAsList = obj => obj != null && !!obj[ITERATE_AS_LIST];
  const displayAsList = obj => obj != null && obj[ITERATE_AS_LIST] === true;;
  const moreList = obj => obj != null && obj[MORELIST] === true;

  exportAPI("isList", isList);
  exportAPI("isNil", isNil);
  exportAPI("iterateAsList", iterateAsList);
  exportAPI("moreList", moreList);
  exportAPI("LIST", LIST);
  exportAPI("MORELIST", MORELIST);
  exportAPI("FIRST", FIRST);
  exportAPI("REST", REST);

  // Objects that "eval" to themselves
  // I trust the JavaScript runtime and JITs to reduce this to some
  // tag-bit inspection that they're probably already doing.
  // (Except for the NIL check, which is last for that exact reason.)
  const isPrimitive = obj => obj == null ||
      (typeof obj !== 'symbol' && typeof obj !== 'object')
      || obj[MORELIST] === false;   // NIL test
  exportAPI("isPrimitive", isPrimitive);
  
  //
  // First off, Arrays are lists
  //
  Object.defineProperties(Array.prototype, {
    [LIST]: { value: true },
    [ITERATE_AS_LIST] : { value: false },  // not really necessary since it's default
    // Null instead of false signals that there's REST, but I'm not a NIL
    [MORELIST]: { get: function() { return this.length > 0 ? true : null } },
    [FIRST]: { get: function() {
      if (this.length > 0) return this[0];
      throw new TypeError(`${firstName} of []`);
    } },
    [REST]: { get: function() {
      return new ArrayList(this, 1);
    } },
  });

  //
  // Secondly, all generators can be lists (subject to generatorsAreLists)
  //
  if (generatorsAreLists) {
    function* dummyGenerator() {}
    monkeyPatchListProtocolForGenerators(dummyGenerator());
    // TODO: I need to understand async generators better
    //   async function* dummyAsyncGenerator() {}
    //   monkeyPatchListProtocolForGenerators(dummyAsyncGenerator());
  }

  //
  // Thirdly, Map, Set, and TypedArray(s) are lists (subject to standardIterablesAreLists).
  // For some reason, WeakMap and WeakSet do not currently (Aug 2021) support iteration.
  //
  let standardIterables = [Map, Set,
    Int8Array, Uint8Array, Uint8ClampedArray,
    Int16Array, Uint16Array,
    Int32Array, Uint32Array,
    Float32Array, Float64Array,
  ]
  // Safari doesn't yet support (Aug 2021)
  if (typeof BigInt64Array === 'object') standardIterables.push(BigInt64Array);
  if (typeof BigUint64Array === 'object') standardIterables.push(BigUint64Array);

  if (standardIterablesAreLists) {
    for (let cls of standardIterables)
      monkeyPatchListProtocolForIterable(cls.prototype);
  }

  //
  // Finally, a Pair is a list too but has no special status.
  // (But sometimes you just need to make a list, so cons *is* handy to have around.)
  //
  class Pair {
    [FIRST]; [REST];
    constructor(first, rest) {
      this[FIRST] = first;
      this[REST] = rest;
    }
    toString() { return string(this); }
    [Symbol.iterator] = pairIterator;
    // static [LIST] = true;  // Hmm; Shouldn't this work?
  }
  Pair.prototype[LIST] = true;
  Pair.prototype[ITERATE_AS_LIST] = true;
  Pair.prototype[MORELIST] = true;
  exportAPI("Pair", Pair, { dontInline: true });

  function pairIterator() {
    let current = this;
    return {
      next() {
        if (!moreList(current))
          return { done: true, value: current };  // value is whatever wasn't a cons cell
        let value = current[FIRST];
        current = current[REST];
        return { done: false, value };
      },
      // So that the iterator itself is iterable, with a fresh iterator at the current position
      [Symbol.iterator]() { return current[Symbol.iterator]() }
    }
  }
  
  class ArrayList {
    _array; _pos; _max;
    constructor(array, pos, max = MAX_INTEGER) {
      this._array = array;
      this._pos = pos;
      this._max = max;
    }
    get [FIRST]() {
      let array = this._array, pos = this._pos;
      if (pos < this._max && pos < array.length)
        return array[pos];
      throw new TypeError(`${firstName} beyond end of array`);
    }
    get [REST]() {
      return new ArrayList(this._array, this._pos + 1, this._max);
    }
    get [MORELIST]() {
      let pos = this._pos;
      return pos < this._max && pos < this._array.length;
    }
    [Symbol.iterator] = pairIterator;
  };
  ArrayList.prototype[LIST] = true;
  // Once we get into the tail of the array, use list iteration
  ArrayList.prototype[ITERATE_AS_LIST] = true;

  function monkeyPatchListProtocolForIterable(prototype) {
    Object.defineProperties(prototype, {
      [LIST]: { value: true },
      [ITERATE_AS_LIST]: { value: false }, // this is default bahavior, but might as well be explicit
      [SUPERLAZY]: { value: true },
      [FIRST]: { get: function() {
        let moreList = this[MORELIST];
        if (moreList)
          return moreList[0];
        throw new TypeError(`${firstName} on exhausted iterator`);
      } },
      [REST]: { get: function() {
        let moreList = this[MORELIST];
        if (moreList)
          return moreList[1];
        throw new TypeError(`${restName} on exhausted iterator`);
      } },
      [MORELIST]: { get: function iterableMoreList() {
        // Note that typical loops will first test whether something is a list or try accessing
        // FIRST before traversing to the REST. So that ends up inetantiating the iterator
        // two or three times. But there's nothing that can be done about it because the underlying
        // collection can change from time to time and we need to see the current values at the
        // time of iteration. To take a specific example, FIRST should always be the current
        // value of the first item in the collection.
        // There might be a gimmick to get around it but I dunno.
        // The alternative is to opt out of this feature and ask users to invoke lazy_list explicitly
        // on non-array iterators.
        if (typeof this[Symbol.iterator] !== 'function')
          throw new TypeError(`no iterator`);
        let iter = this[Symbol.iterator]();
        if (typeof iter.next !== 'function')
          throw new TypeError(`bad iterator`);
        let { done, value } = iter.next();
        if (!done)
          return [value, new LazyIteratorList(iter)];
        return false;
      } },
    });
  }

  function monkeyPatchListProtocolForGenerators(gen) {
    for ( ; ; gen = Object.getPrototypeOf(gen)) {
      // LogicError because these are tested at load time
      if (gen == null)
        throw new LogicError(`generator has no iterator`);
      if (gen.hasOwnProperty(Symbol.iterator)) {
        if (typeof gen[Symbol.iterator] !== 'function')
          throw new LogicError(`generator has bad iterator`);
        Object.defineProperties(gen, {
          [LIST]: { value: true },
          [ITERATE_AS_LIST]: { value: false },
          [SUPERLAZY]: { value: true },
          [FIRST]: { get: function() {
            this[MORELIST];
            return this[FIRST];
          } },
          [REST]: { get: function() {
            this[MORELIST];
            return this[REST];
          } },
          [MORELIST]: { get: function iterableMoreList() {
            if (typeof this[Symbol.iterator] !== 'function')
              throw new TypeError(`no iterator`);
            let iter = this[Symbol.iterator]();
            if (typeof iter.next !== 'function')
              throw new TypeError(`bad iterator`);
            let { done, value } = iter.next();
            if (done) {
              Object.defineProperties(this, {
                [FIRST]: { get: function() { throw new TypeError(`${firstName} on exhausted iterator`)} },
                [REST]: { value },
                [MORELIST]: { value: false },
              });
              return false;
            }
            Object.defineProperties(this, {
              [FIRST]: { value },
              [REST]: { value: new LazyIteratorList(iter) },
              [MORELIST]: { value: true },
              [SUPERLAZY]: { value: false },
            });
            return true;
          } },
        });
        break;
      }
    }
  }

  //
  // NIL is an utter degenerate that signals errors when accessed incorrectly.
  // NIL inherits from null rather than Object so that, once frozen, there is literally
  // no way that NIL can be changed.
  //
  const NIL = Object.create( null, {
    NIL: { value: "NIL" },  // Makes it clear in the debugger
    [FIRST]: {
      get: () => { throw new TypeError(`${firstName} of ${nilName}`) },
      set: _ => { throw new TypeError(`set ${firstName} of ${nilName}`) }
    },
    [REST]: {
      get: () => { throw new TypeError(`${restName} of ${nilName}`) },
      set: _ => { throw new TypeError(`set ${restName} of ${nilName}`) }
    },
    [Symbol.iterator]: { value: function createNilIterator() {
      return {
        next() { return { done: true, value: NIL } },
        [Symbol.iterator]() { return current[Symbol.iterator]() }
      } }
    },
    [LIST]: { value: true },
    [ITERATE_AS_LIST]: { value: false },
    [MORELIST]: { value: false },
    // Make sure it has Object methods to keep from blowing up the universe
    toString: { value: function NILtoString() { return nilName } },
    toLocaleString: { value: function NILtoLocaleString() { return nilName } },
    hasOwnProperty: { value: function NILhasOwnProperty(_) { return false } },
    isPrototypeOf: { value: function NILisPrototypeOf(_) { return false } },
    propertyIsEnumerable: { value: function NILpropertyIsEnumerable(_) { return false } },
    valueOf: { value: function NILvalueOf(_) { return false } },
  });
  Object.freeze(NIL);
  exportAPI("NIL", NIL);

  // But what is the "bottom" value anyway? Lisps usually use NIL, but JS uses "undefined", You choose.
  const BOTTOM = bottomIsLNIL ? NIL : undefined;
  exportAPI("BOTTOM", BOTTOM);

  const isIterable = obj => obj != null && typeof obj[Symbol.iterator] === 'function';
  exportAPI("isIterable", isIterable);
  
  //
  // Atoms are Symbols that are in the ATOMS dictionary
  //
  const ATOMS = {};
  const isAtom = obj => typeof obj === 'symbol' && ATOMS[obj.description] === obj;
  exportAPI("isAtom", isAtom);

  exportAPI("Atom", Atom, { dontInline: true });
  function Atom(name) {
    // If they pass in an atom, just return it
    if (isAtom(name)) return name;
    if (typeof name !== 'string')
      throw new TypeError(`Not a string ${name}`);
    let atom = ATOMS[name];
    if (atom !== undefined) return atom;
    atom = Symbol(name);
    ATOMS[name] = atom;
    return atom;
  }

  const LAMBDA_ATOM = Atom("lambda"), SLAMBDA_ATOM = Atom("lambda#"), LAMBDA_CHAR = "\u03BB";
  // Atomic-level aliases for "lambda" and "lambda#"
  ATOMS["\\"]  = ATOMS[LAMBDA_CHAR]     = ATOMS[lambdaStr]     = LAMBDA_ATOM;
  ATOMS["\\#"] = ATOMS[LAMBDA_CHAR+"#"] = ATOMS[lambdaStr+"#"] = SLAMBDA_ATOM;
  const CLOSURE_ATOM = Atom("%%closure");
  const SCLOSURE_ATOM = Atom("%%closure#");
  const QUOTE_ATOM = Atom("quote");
  const REST_PARAM_ATOM = Atom(restParamStr);
  ATOMS["'"] = QUOTE_ATOM;
  exportAPI("LAMBDA_CHAR", LAMBDA_CHAR);
  exportAPI("LAMBDA_ATOM", LAMBDA_ATOM);
  exportAPI("SLAMBDA_ATOM", SLAMBDA_ATOM);
  exportAPI("QUOTE_ATOM", QUOTE_ATOM);
  exportAPI("REST_PARAM_ATOM", REST_PARAM_ATOM);

  exportAPI("iteratorFor", iteratorFor, { dontInline: true });
  function iteratorFor(obj, throwException = TypeError) {
    if (obj != null) {
      if (typeof obj[Symbol.iterator] === 'function') return obj[Symbol.iterator]();
      if (typeof obj.next === 'function') return obj;
    }
    if (throwException) throw new throwException(`Not an iterable or list ${obj}`);
  }  

  exportAPI("defineBinding", defineBinding, { dontInline: true });
  function defineBinding(name, value, ...names) {
    let opts = names[names.length-1];
    if (typeof opts === 'object')
      names.pop();
    else
      opts = {};
    if (typeof value === 'string') {
      if (!globalScope.hasOwnProperty(value))
        throw LogicError(`${value} not in globalScope`);
      value = globalScope[value];
    } else {
      if (typeof value === 'function')
        examineFunctionForCompilerTemplates(name, value, opts);
    }
    names = [name, ...names];
    for (let i = 0; i < names.length; ++i)
      names[i] = Atom(names[i]);
    opts.atoms = names;
    opts.value = value;
    for (name of names) {
      globalScope[name] = value;
      helpInfo[name] = opts;
    }
  }

  // Substitute for String.prototype.replaceAll until Node.js supports it.
  // (Maybe I just need to undate Node? Well, I can't be the only one, so...)
  // This isn't strictly correct since it blows up if newSubstr contains substr,
  // but it's good enough for our purposes.
  function replaceAll(str, substr, newSubstr) {
    let prevStr;
    do {
      prevStr = str;
      str = str.replace(substr, newSubstr);
    } while (str !== prevStr);
    return str;
  }

  function examineFunctionForCompilerTemplates(name, fn, opts = {}) {
    let evalCount = opts.evalCount ?? MAX_INTEGER;
    let compileHook = opts.compileHook;
    let requiresScope = opts.requiresScope;
    let tag = opts.tag ?? 0;
    examineFunctionForParameterDescriptor(fn, evalCount, tag);
    let fnInfo = analyzeJSFunction(fn);
    fnInfo.evalCount = evalCount;
    if (opts.requiredCount !== undefined) // allows override
      fnInfo.requiredCount = opts.requiredCount;
    fnInfo.tag = tag;
    if (compileHook)
      fnInfo.compileHook = compileHook;
    if (compileHook || evalCount !== MAX_INTEGER) {
      fnInfo.valueTemplate = fnInfo.bodyTemplate = undefined;
    }
    if (opts.dontInline) {
      fnInfo.valueTemplate = fnInfo.bodyTemplate = undefined;
    } else if (fnInfo.native || tag !== FUNCTION_TAG) {
      // not an error
    } else if (!compileHook && evalCount !== MAX_INTEGER) {
      throw new LogicError(`Special function requires compile hook ${name}`);
    } else if (!fnInfo.valueTemplate && !fnInfo.compileHook) {
      throw new LogicError(`Function requires templatable definition or compile hook ${name}`);
    }
    // Compile hooks will set ssaScope.dynamicScopeUsed if they want to
    if (compileHook)
      requiresScope = false;
    fnInfo.requiresScope = requiresScope;
    fn[COMPILE_INFO] = fnInfo;
  }

  exportAPI("augmentFunctionInfo", augmentFunctionInfo, { dontInline: true });
  function augmentFunctionInfo(fn, info) {
    if (typeof fn === 'string') fn = Atom(fn);
    if (isAtom(fn)) fn = globalScope[fn];
    if (typeof fn !== 'function') throw new LogicError(`not a function ${string(fn)}`);
    let fnInfo = fn[COMPILE_INFO];
    if (fnInfo) Object.assign(fnInfo, info);
    else fn[COMPILE_INFO] = Object.assign({}, info);
  }

  exportAPI("VERSION", VERSION);

  class SchemeError extends Error {};
  SchemeError.prototype.name = "SchemeError";
  exportAPI("SchemeError", SchemeError, { dontInline: true });

  class SchemeEvalError extends SchemeError {};
  SchemeEvalError.prototype.name = "SchemeEvalError";
  exportAPI("SchemeEvalError", SchemeEvalError, { dontInline: true });

  class SchemeCompileError extends SchemeError {};
  SchemeCompileError.prototype.name = "SchemeCompileError";
  exportAPI("SchemeCompileError", SchemeCompileError, { dontInline: true });

  exportAPI("LogicError", LogicError), { dontInline: true };

  let EVALUATE_KEY_VALUE_SYMBOL = Symbol("EVALUATE-KEY-VALUE")
  exportAPI("EVALUATE_KEY_VALUE_SYMBOL", EVALUATE_KEY_VALUE_SYMBOL);

  //
  // SchemeJS strives to maintain JavaScript consistency wherever possibe but enough is enough.
  // In SchemeJS, "NIL", null, undefined, and false are false and everything else is true.
  //
  // Written this way so that actual bools do not need to be compared to NIL; most of this
  // is just a tag-bit compare in the runtime.
  //
  // I'm likely to revisit this. Differet Schemes and Lisps have different policies
  // here. What I'd like to do is define schemeTrue in a way that the JIT can trivially evaluate.
  // In particular, treating NIL as false is slightly more costly than I'd like.
  // Anyway, overrride it if you want to by setting the schemeTrueOverride option.
  //
  const schemeTrue = typeof schemeTrueOverride === 'function' ? schemeTrueOverride :
      (a => a === true || (a !== false && a != null && a[MORELIST] !== false));
  exportAPI("schemeTrue", schemeTrue);

  let quote = quoted => quoted[FIRST];
  exportAPI("quote", quote, { dontInline: true, evalCount: 0 }, "quote");

  exportAPI("this", function scope() { return this }, { requiresScope: true });

  // Pokemon gotta catch 'em' all!
  exportAPI("bit_not", a => ~a);
  exportAPI("pow", (a,b) => a ** b);
  exportAPI("rem", (a,b) => a % b);
  exportAPI("bit_shl", (a,b) => a << b);
  exportAPI("bit_shr", (a,b) => a >> b);
  exportAPI("bit_ash", (a,b) => a >>> b);
  exportAPI("in", (a,b) => a in b);
  exportAPI("new", (cls, ...args) => new cls(...args));
  exportAPI("instanceof", (a,b) => a instanceof b);
  exportAPI("at", (a, b) => a[b]);
  exportAPI("at_if", (a, b) => a?.[b]);
  exportAPI("call", (a, ...params) => a(...params));
  exportAPI("method_call", (a, b, ...params) => a[b](...params));
  exportAPI("assign", (a, b, c) => a[b] = c);
  exportAPI("delete", (a, b) => delete a[b]);
  exportAPI("void", _ => undefined);
  exportAPI("not", a => !a);
  exportAPI("typeof", a => typeof a);

  //
  // Variable args definitions
  //
  exportAPI("add", add, { compileHook: add_hook });
  function add(a, b, ...rest) {
    a += b;
    for (b of rest)
      a += b;
    return a;
  }
  function add_hook(args, ssaScope, tools) {
    return n_ary_hooks(args, ssaScope, tools, '+', 'add');
  }
  function n_ary_hooks(args, ssaScope, tools, op, name) {
    if (args.length === 0) return 'NaN';
    let ssaResult = tools.newTemp(name);
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` ${op} ${args[i]}`;
    str += `)`;
    tools.emit(`let ${ssaResult} = ${str}`);
    return ssaResult;
  }

  exportAPI("sub", sub, { compileHook: sub_hook });
  function sub(a, ...rest) {
    if (rest.length === 0) return -a;
    for (let b of rest)
      a -= b;
    return a;
  }
  function sub_hook(args, ssaScope, tools) {
    if (args.length === 0) return 'NaN';
    if (args.length === 1)
      return `(-${args[0]})`;
    return n_ary_hooks(args, ssaScope, tools, '-', 'sub');
  }

  exportAPI("mul", mul, { compileHook: mul_hook });
  function mul(a, b, ...rest) {
    a *= b;
    for (let b of rest)
      a *= b;
    return a;
  }
  function mul_hook(args, ssaScope, tools) {
    return n_ary_hooks(args, ssaScope, tools, '*', 'mul');
  }

  exportAPI("div", div, { compileHook: div_hook });
  function div(a, ...rest) {
    if (rest.length === 0) return 1/a;
    for (let b of rest)
      a /= b;
    return a;
  }
  function div_hook(args, ssaScope, tools) {
    if (args.length === 0) return 'NaN';
    if (args.length === 1)
      return `(1 / ${args[0]})`;
    return n_ary_hooks(args, ssaScope, tools, '/', 'div');
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` / ${args[i]}`;
    str += `)`;
    return str;
  }

  exportAPI("bit_and", bit_and, { compileHook: bit_and_hook });
  function bit_and(a, b, ...rest) {
    a &= b;
    for (b of rest)
      a &= b;
    return a;
  }
  function bit_and_hook(args, ssaScope, tools) {
    if (args.length < 2) return 0;
    return n_ary_hooks(args, ssaScope, tools, '&', 'bitAnd');
  }

  exportAPI("bit_or", bit_or, { compileHook: bit_or_hook });
  function bit_or(a, b, ...rest) {
    a |= b;
    for (let b of rest)
      a |= b;
    return a;
  }
  function bit_or_hook(args, ssaScope, tools) {
    if (args.length < 2) return 0;
    return n_ary_hooks(args, ssaScope, tools, '|', 'bitOr');
  }

  exportAPI("bit_xor", bit_xor, { compileHook: bit_xor_hook });
  function bit_xor(a, b, ...rest) {
    a ^= b;
    for (let b of rest)
      a ^= b;
    return a;
  }
  function bit_xor_hook(args, ssaScope, tools) {
    if (args.length < 2) return 0;
    return n_ary_hooks(args, ssaScope, tools, '^', 'bitXor');
    return str;
  }

  exportAPI("lt", lt, { evalCount: 2, compileHook: lt_hook });
  function lt(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!(a < b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
      if (this[RETURN_SYMBOL]) return;
    }
    return true;
  }
  function lt_hook(args, ssaScope, tools) {
    return compare_hooks(args, ssaScope, tools, 'A < B', 'lt');
  }

  exportAPI("compare_hooks", compare_hooks, { dontInline: true })
  function compare_hooks(args, ssaScope, tools, op, name) {
    if (args.length < 2) return 'false'; // individual invokers need to override this
    if (args.length === 2) {
      let str = '';
      for (let ch of op) {
        if (ch === 'A') ch = args[0];
        else if (ch === 'B') ch = args[1];
        str += ch;
      }
      return `(${str})`;
    }
    let result = tools.newTemp(name);
    tools.emit(`let ${result} = false; ${result}: {`);
    let saveIndent = tools.indent;
    tools.indent += '  ';
    tools.emit(`let A = ${args[0]}, B = ${args[1]};`);
    tools.emit(`if (!(${op})) break ${result};`);
    for (let i = 2; i < args.length; ++i) {
      tools.emit(`A = B;`);
      let ssaB = compileEval(args[i], ssaScope, tools, tools.newTemp);
      tools.emit(`B = ${ssaB};`);
      tools.emit(`if (!(${op})) break ${result};`);
    }
    tools.emit(`${result} = true;`);
    tools.indent = saveIndent;
    tools.emit(`}`);
    return result;
  }

  exportAPI("le", le, { evalCount: 2, compileHook: le_hook });
  function le(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!(a <= b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
      if (this[RETURN_SYMBOL]) return;
    }
    return true;
  }
  function le_hook(args, ssaScope, tools) {
    return compare_hooks(args, ssaScope, tools, 'A <= B', 'le');
  }

  exportAPI("gt", gt, { evalCount: 2, compileHook: gt_hook });
  function gt(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!(a > b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
      if (this[RETURN_SYMBOL]) return;
    }
    return true;
  }
  function gt_hook(args, ssaScope, tools) {
    return compare_hooks(args, ssaScope, tools, 'A > B', 'gt');
  }

  exportAPI("ge", ge, { evalCount: 2, compileHook: ge_hook });
  function ge(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!(a >= b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
      if (this[RETURN_SYMBOL]) return;
    }
    return true;
  }
  function ge_hook(args, ssaScope, tools) {
    return compare_hooks(args, ssaScope, tools, 'A >= B', 'ge');
  }

  exportAPI("eq", eq, { evalCount: 2, compileHook: eq_hook });
  function eq(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!(a == b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
      if (this[RETURN_SYMBOL]) return;
    }
    return true;
  }
  function eq_hook(args, ssaScope, tools) {
    if (args.length < 2) return 'true';
    return compare_hooks(args, ssaScope, tools, 'A == B', 'eq');
  }

  exportAPI("eeq", eeq, { evalCount: 2, compileHook: eeq_hook });
  function eeq(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!(a === b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
      if (this[RETURN_SYMBOL]) return;
    }
    return true;
  }
  function eeq_hook(args, ssaScope, tools) {
    if (args.length < 2) return 'true';
    return compare_hooks(args, ssaScope, tools, 'A === B', 'eeq');
  }

  exportAPI("neq", neq, { evalCount: 2, compileHook: neq_hook });
  function neq(a, b, ...rest) {
    return !eq.call(this, a, b, ...rest);
  }
  function neq_hook(args, ssaScope, tools) {
    if (args.length < 2) return 'false';
    let eq = compare_hooks(args, ssaScope, tools, 'A == B', 'eq');
    return `(!${eq})`;
  }

  exportAPI("neeq", neeq, { evalCount: 2, compileHook: neeq });
  function neeq(a, b, ...rest) {
    return !eeq.call(this, a, b, ...rest);
  }
  function neeq(args, ssaScope, tools) {
    let eeq = compare_hooks(args, ssaScope, tools, 'A == B', 'eq');
    return `(!${eeq})`;
  }

  // Logical & Conditional

  exportAPI("and", and, { evalCount: 0, compileHook: and_hook });
  function and(...forms) {
    let val = true;
    for (let i = 0, formsLength = forms.length; i < formsLength; ++i) {
      val = _eval(forms[i], this);
      if (this[RETURN_SYMBOL]) return;
      if (!schemeTrue(val)) return val;
    }
    return val;
  }
  function and_hook(args, ssaScope, tools) {
    return and_or_hook(args, ssaScope, tools, 'and', 'true', '!schemeTrue');
  }

  function and_or_hook(args, ssaScope, tools, name, init, test) {
    if (args.length < 1)
      return init;
    if (args.length === 1)
      return compileEval(args[0], ssaScope, tools);
    let ssaResult = tools.newTemp(name), saveIndent = tools.indent;
    tools.indent += '  ';
    tools.emit(`let ${ssaResult} = ${compileEval(args[0], ssaScope, tools)}; ${ssaResult}: {`);
    for (let i = 1; i < args.length; ++i) {
      tools.emit(`if (${test}(${ssaResult})) break ${ssaResult};`);
      tools.emit(`${ssaResult} = ${compileEval(args[i], ssaScope, tools)};`);
    }
    tools.indent = saveIndent;
    tools.emit('}');
    return ssaResult;
  }

  exportAPI("or", or, { evalCount: 0, compileHook: or_hook });
  function or(...forms) {
    let val = false;
    for (let i = 0, formsLength = forms.length; i < formsLength; ++i) {
      val = _eval(forms[i], this);
      if (this[RETURN_SYMBOL]) return;
      if (schemeTrue(val)) return val;
    }
    return val;
  }
  function or_hook(args, ssaScope, tools,) {
    return and_or_hook(args, ssaScope, tools, 'or', 'false', 'schemeTrue');
  }

  exportAPI("nullish", nullish, { evalCount: 0, compileHook: nullish_hook });
  function nullish(...forms) {
    let val = undefined;
    for (let i = 0, formsLength = forms.length; i < formsLength; ++i) {
      val = _eval(forms[i], this);
      if (this[RETURN_SYMBOL]) return;
      if (val != null) return val;
    }
    return val;
  }
  function nullish_hook(args, ssaScope, tools) {
    if (args.length < 1)
      return 'undefined';
    if (args.length === 1)
      return compileEval(args[0], ssaScope, tools);
    let ssaResult = tools.newTemp('nullish'), saveIndent = tools.indent;
    tools.indent += '  ';
    tools.emit(`let ${ssaResult} = ${compileEval(args[0], ssaScope, tools)}; ${ssaResult}: {`);
    for (let i = 1; i < args.length; ++i) {
      tools.emit(`if ((${ssaResult}) != null) break ${ssaResult};`);
      tools.emit(`${ssaResult} = ${compileEval(args[i], ssaScope, tools)};`);
    }
    tools.indent = saveIndent;
    tools.emit('}');
    return ssaResult;
  }

  //
  // Conditionals
  //
  exportAPI("ifelse", ifelse, { evalCount: 1, compileHook: ifelse_hook });
  function ifelse(p, t = true, f = false) {
    p = schemeTrue(p);
    if (p)
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function ifelse_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'if', 'schemeTrue(*)');
  }

  function conditionalHooks(args, ssaScope, tools, name, test) {
    let a = args[0], t = args[1], f = args[2];
    test = replaceAll(test, '*', a);
    let ssaResult = tools.newTemp(name);  // It's like a PHI node in SSA compilers. Sorta.
    if (t === undefined && f === undefined) {
      tools.emit(`let ${ssaResult} = !!(${test});`);
      return ssaResult;
    }
    tools.emit(`let ${ssaResult};`);
    tools.emit(`if (${test}) {`);
    let saveIndent = tools.indent;
    tools.indent += '  ';
    let tResult = t === undefined ? 'true' : compileEval(t, ssaScope, tools);
    tools.emit(`${ssaResult} = ${tResult};`);
    tools.indent = saveIndent;
    tools.emit(`} else {`);
    tools.indent += '  ';
    let fResult = f === undefined ? 'false' : compileEval(f, ssaScope, tools);
    tools.emit(`${ssaResult} = ${fResult};`);
    tools.indent = saveIndent;
    tools.emit(`}`);
    return ssaResult;
  }

  //
  // Predicate/conditional functions.
  // The pattern here is that you can either write
  //   (? (bigint x) a b) or simply (bigint? x a b)
  //
  exportAPI("is_bigint", is_bigint, { evalCount: 1, compileHook: is_bigint_hook });
  function is_bigint(a, t = true, f = false) {
    if (typeof a === 'bigint')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_bigint_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_bigint', `typeof * === 'bigint'`);
  }

  exportAPI("is_atom", is_atom, { evalCount: 1, compileHook: is_atom_hook });
  function is_atom(a, t = true, f = false) {
    if (isAtom(a))
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_atom_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_atom', `isAtom(*)`);
  }

  exportAPI("is_list", isList, { evalCount: 1, compileHook: is_list_hook } );
  function is_list(a, t = true, f = false) {
    if (isList(a))
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_list_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_list', `isList(*)`);
  }

  exportAPI("is_undefined", is_undefined, { evalCount: 1, compileHook: is_undefined_hook })
  function is_undefined(a, t = true, f = false) {
    if (a === undefined)
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_undefined_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_undefined', `* === undefined`);
  }

  exportAPI("is_null", is_null, { evalCount: 1, compileHook: is_null_hook });
  function is_null(a, t = true, f = false) {
    if (a === null)
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_null_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_null', `* === null`);
  }

  exportAPI("is_nil", is_nil, { evalCount: 1, compileHook: is_nil_hook });
  function is_nil(a, t = true, f = false) {
    if (isNil(a))
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_nil_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_nil', `isNil(*)`);
  }

  exportAPI("is_nullish", is_nullish, { evalCount: 1, compileHook: is_nullish_hook });
  function is_nullish(a, t = true, f = false) {
    if (a == null)
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_nullish_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_nullish', `* == null`);
  }

  exportAPI("is_boolean", is_boolean, { evalCount: 1, compileHook: is_boolean_hook });
  function is_boolean(a, t = true, f = false) {
    if (typeof a === 'boolean')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_boolean_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_boolean', `typeof * === 'boolean'`);
  }

  exportAPI("is_number", is_number, { evalCount: 1, compileHook: is_number_hook });
  function is_number(a, t = true, f = false) {
    if (typeof a === 'number')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_number_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_number', `typeof * === 'number'`);
  }

  exportAPI("is_numeric", is_numeric, { evalCount: 1, compileHook: is_numeric_hook });
  function is_numeric(a, t = true, f = false) {
    if (typeof a === 'number' || typeof a === 'bigint')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_numeric_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_numeric', `typeof * === 'number' || typeof a === 'bigint`);
  }

  exportAPI("is_string", is_string, { evalCount: 1, compileHook: is_string_hook });
  function is_string(a, t = true, f = false) {
    if (typeof a === 'string')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_string_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_string', `typeof * === 'string'`);
  }

  exportAPI("is_symbol", is_symbol, { evalCount: 1, compileHook: is_symbol_hook });
  function is_symbol(a, t = true, f = false) {
    if (typeof a === 'symbol')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_symbol_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_symbol', `typeof * === 'symbol'`);
  }

  exportAPI("is_function", is_function, { evalCount: 1, compileHook: is_function_hook });
  function is_function(a, t = true, f = false) {
    if (typeof a === 'function')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_function_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_function', `typeof * === 'function'`);
  }

  exportAPI("is_object", is_object, { evalCount: 1, compileHook: is_object_hook });
  function is_object(a, t = true, f = false) {
    if (a !== null && typeof a === 'object')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_object_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_object', `* !== null && typeof * === 'object'`);
  }

  exportAPI("is_array", is_array, { evalCount: 1, compileHook: is_array_hook });
  function is_array(a, t = true, f = false) {
    if (isArray(a))
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_array_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_array', `Array.isArray(*)`);
  }

  exportAPI("is_NaN", is_nan, { evalCount: 1, compileHook: is_nan_hook });
  function is_nan(a, t = true, f = false) {
    if (isNaN(a))
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_nan_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_nan', `isNaN(*)`);
  }

  exportAPI("is_finite", is_finite, { evalCount: 1, compileHook: is_finite_hook });
  function is_finite(a, t = true, f = false) {
    if (isFinite(a))
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_finite_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_finite', `isFinite(*)`);
  }

  // (begin form1 form2 ...)
  exportAPI("begin", begin, { evalCount: 0, compileHook: begin_hook });
  function begin(...forms) {
    let res = BOTTOM;
    for (let i = 0, formsLength = forms.length; i < formsLength; ++i) {
      res = _eval(forms[i], this);
      if (this[RETURN_SYMBOL]) return;
    }
    return res;
  }
  function begin_hook(args, ssaScope, tools) {
    let ssaResult = `${BOTTOM}`;
    for (let i = 0; i < args.length; ++i)
      ssaResult = compileEval(args[i], ssaScope, tools);
    return ssaResult;
  }

  exportAPI("list", list, { compileHook: list_hook });
  function list(...elements) {
    let val = NIL;
    for (let i = elements.length; i > 0; --i)
      val = new Pair(elements[i-1], val);
    return val;
  }
  function list_hook(args, ssaScope, tools) {
    let emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
    // The compiler can inline the list function just fine, but it's better to do it this way
    // because no argument array needs to be constructed.
    let ssaResult = newTemp("list");
    emit(`let ${ssaResult} = NIL;`);
    for (let i = args.length; i > 0; --i)
      emit(`${ssaResult} = new Pair(${args[i-1]}, ${ssaResult});`)
    return ssaResult;
  }

  exportAPI("length", length, { dontInline: true });
  function length(list) {
    let n = 0;
    for ( ; iterateAsList(list); list = list[REST])
      n += 1;
    // This is tricky... a list can begin as list-iterable but fall into soething normally-iterable.
    // Don't special-case string. Its iterator returns code points by combining surrogate pairs
    if (isArray(list) && list.length > 0)
      return list.length + n;
    if (isNil(list)) return n;
      if (!isIterable(list)) throw new TypeError(`Not a list or iterable ${string(list)}`);
    for (let _ of list)
      n += 1;
    return n;
  }

  // (map fn list1 list2 ...)
  exportAPI("map", map, { dontInline: true });
  function map(fn, ...lists) {
    let result = NIL, last;
    for (let list of lists) {
      for ( ; iterateAsList(list); list = list[REST]) {
        let item = list[FIRST];
        item = fn.call(this, item);
        item = new Pair(item, NIL)
        if (last) last = last[REST] = item;
        else result = last = item;
      }
      // Yes, fall thru!
      if (!isNil(list)) {
        for (let item of list) {
          item =  fn.call(this, item);
          item = new Pair(item, NIL);
          if (last) last = last[REST] = item;
          else result = last = item;
        }
      }
    }
    return result;
  }

  // Same as map but results in an Array
  exportAPI("array_map", array_map, { dontInline: true });
  function array_map(fn, ...lists) {
    let res = [];
    for (let list of lists) {
      for ( ; iterateAsList(list); list = list[REST]) 
        res.push(fn(list[FIRST]));
      // Yes, fall thru!
      if (!isNil(list)) {
        for (let item of list)
          res.push(fn(item));
      }
    }
    return res;
  }

  // (filter fn list1 list2 ...)
  exportAPI("filter", filter, { dontInline: true });
  function filter(predicateFn, ...lists) {
    let result = NIL, last;
    for (let list of lists) {
      for ( ; iterateAsList(list); list = list[REST]) {
        let item = list[FIRST];
        if (schemeTrue(predicateFn(item))) {
          item = new Pair(item, NIL);
          if (last) last = last[REST] = item;
          else result = last = item;
        }
      }
      if (!isNil(list)) {
        for (let item of list) {
          if(schemeTrue(predicateFn(item))) {
            item = new Pair(item, NIL);
            if (last) last = last[REST] = item;
            else result = last = item;
          }
        }
      }
    }
    return result;
  }

  //
  // Lazy lists by decaying down to ordinary Cons cells as evaluated.
  //
  class LazyFirstList {
    [LAZYFIRST]; [REST];
    constructor(getFirst, rest) {
      this[LAZYFIRST] = getFirst;
      this[REST] = rest;
    }
    toString() { return string(this) }
    get [FIRST]() {
      let first = this[LAZYFIRST]();
      delete this[LAZYFIRST];
      Object.setPrototypeOf(this, Pair.prototype);
      return this[FIRST] = first;
    }
    set [FIRST](val) {
      delete this[LAZYFIRST];
      Object.setPrototypeOf(this, Pair.prototype);
      this[FIRST] = val;
    }
    [Symbol.iterator] = pairIterator();
  }
  LazyFirstList.prototype[LIST] = true;
  LazyFirstList.prototype[LAZYFIRST] = true;
  LazyFirstList.prototype[ITERATE_AS_LIST] = true;
  LazyFirstList.prototype[MORELIST] = true;

  //
  // Doesn't even know if it's a cons cell or null yet!
  //
  class LazyIteratorList {
    [LAZYFIRST]; [LAZYREST];
    constructor(iterator, mapper) {
      this[LAZYFIRST] = mapper;
      this[LAZYREST] = iterator;
    }
    get [FIRST]() {
      if (!this[MORELIST]) throw new TypeError(`${firstName} of ${nilName}`);
      return this[FIRST];
    }
    set [FIRST](val) {
      let mapper = this[LAZYFIRST];
      if (!this[MORELIST]) throw new TypeError(`set ${firstName} of ${nilName}`);
      if (mapper)
        Object.setPrototypeOf(this, Pair.prototype);
      this[FIRST] = val;
    }
    get [REST]() {
      if (!this[MORELIST]) throw new TypeError(`${restName} of ${nilName}`);
      return this[REST];
    }
    set [REST](val) {
      if (!this[MORELIST]) throw new TypeError(`set ${restName} of ${nilName}`);
      this[REST] = val;
    }
    get [MORELIST]() {
      let iterator = this[LAZYREST], mapper = this[LAZYFIRST];
      let { done, value: first } = iterator.next();
      if (done) {
        Object.setPrototypeOf(this, Object.getPrototypeOf(NIL));
        delete this[LAZYREST];
        delete this[LAZYFIRST];
        delete this[FIRST];
        delete this[REST];
        return this[MORELIST] = false;
      }
      let cdr = new LazyIteratorList(iterator, mapper);
      if (mapper) {
        Object.setPrototypeOf(this, LazyFirstList.prototype);
        delete this[LAZYREST];
        delete this[LAZYFIRST];
        this[LAZYFIRST] = () => mapper(first);
      } else {
        Object.setPrototypeOf(this, Pair.prototype);
        delete this[LAZYREST];
        delete this[LAZYFIRST];
        this[FIRST] = first;
      }
      this[REST] = cdr;
      return true;
    }
    get [ITERATE_AS_LIST]() { return this[MORELIST] }
    toString() { return string(this) }
    [Symbol.iterator] = pairIterator();
  }
  LazyIteratorList.prototype[LIST] = true;
  LazyIteratorList.prototype[SUPERLAZY] = true;
  LazyIteratorList.prototype[LAZYFIRST] = true;
  LazyIteratorList.prototype[LAZYREST] = true;

  exportAPI("list_view", list_view, { dontInline: true });
  function list_view(obj) {
    let iterator = iteratorFor(obj, TypeError);
    return new LazyIteratorList(iterator);
  }

  exportAPI("lazy_map", lazy_map, { dontInline: true });
  function lazy_map(fn, obj) {
    let iterator = iteratorFor(obj, TypeError);
    return new LazyIteratorList(iterator, a => fn(a))
  }

  // TODO: lazy-filter?

  // (let (binding1 binding2 ...) form1 form2 ...) -- let* behavior
  //     (let ((x 10)
  //           (y 20))
  //       (+ x y))
  // Because this implementation uses a scope chain instead
  // of an environment, each kind of let is as powerful as "letrec".
  //
  // TODO: Reconsider that; it's easy to implement let and let*;
  // it's just that I think they're bad ideas, historically baked-in.
  // But it's possible there's existing code that depends on the
  // behavior of let and let*, for instance,
  //    (let ((x (something-that-uses-outer-scope-x) ...
  //
  // "letrec" can be partially-applied, returning a function that
  // evaluates its arguments in the let scope!
  //
  exportAPI("letrec", letrec, { requiresScope: true, evalCount: 0, compileHook: letrec_hook });
  function letrec(bindings, form, ...forms) {
    let scope = newScope(this, "letrec-scope");
    for ( ; moreList(bindings); bindings = bindings[REST]) {
      let binding = bindings[FIRST];
      if (!isList(binding))
        throw new SchemeEvalError(`Bad binding ${string(binding)}`);
      let boundVar = binding[FIRST], bindingForms = binding[REST];
      if (typeof boundVar !== 'symbol')
        throw new SchemeEvalError(`Bad binding ${string(binding)}`);
      let val = NIL;
      for ( ; moreList(bindingForms); bindingForms = bindingForms[REST]) {
        val = _eval(bindingForms[FIRST], scope);
        if (scope[RETURN_SYMBOL]) return;
      }
      scope[boundVar] = val;
    }
    let res = _eval(form, scope);
    if (scope[RETURN_SYMBOL]) return;
    for (let i = 0, formsLength = forms.length; i < formsLength ; ++i) {
      res = _eval(forms[i], scope);
      if (scope[RETURN_SYMBOL]) return;
    }
    return res;
  }
  function letrec_hook(args, ssaScope, tools) {
    // XXX TODO: This really only implemnents let*.
    // For "letrec", emit the `let ${ssaBoundVar} = NIL;` segnemts first,
    // then the initialization bodies afterwards.
    // For "let" emit the bodies first, then the initializations.
    // For "let*" keep doing this.
    let emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
    if (args.length < 2) throw new SchemeCompileError(`Bad letrec`);
    let bindings = args[0];
    let ssaResult = newTemp("letrec");
    let saveSsaScope = ssaScope, scopeLines = [];
    ssaScope = newScope(ssaScope, "compiler-letrec-scope");
    let ssaTmpScope = newTemp("scope_tmp");
    scopeLines.push(emit(`let ${ssaTmpScope} = scope;`));
    emit(`let ${ssaResult} = NIL; { // letrec`);
    let saveIndent = tools.indent;
    tools.indent += '  ';
    scopeLines.push(emit(`let scope = newScope(${ssaTmpScope}, "compiled-letrec-scope");`));
    for( ; moreList(bindings); bindings = bindings[REST]) {
      let binding = bindings[FIRST];
      if (!isList(binding))
        throw new SchemeCompileError(`Bad binding ${string(binding)}`)
      let boundVar = binding[FIRST], bindingForms = binding[REST];
      if (typeof boundVar !== 'symbol')
        throw new SchemeEvalError(`Bad binding ${string(binding)}`);
      let ssaBoundVar = newTemp(boundVar);
      emit(`let ${ssaBoundVar} = NIL;`);
      for ( ; moreList(bindingForms); bindingForms = bindingForms[REST]) {
        let ssaVal = compileEval(bindingForms[FIRST], ssaScope, tools);
        emit(`${ssaBoundVar} = ${ssaVal};`);
      }
      let paramAtom = use(bind(boundVar));
      ssaScope[boundVar] = ssaBoundVar;
      scopeLines.push(emit(`scope[${paramAtom}] = ${ssaBoundVar};`));
    }
    for (let i = 1; i < args.length; ++i) {
      let ssaVal = compileEval(args[i], ssaScope, tools);
      emit(`${ssaResult} = ${ssaVal};`);
    }
    tools.indent = saveIndent;
    emit(`}`);
    if (ssaScope.dynamicScopeUsed)
      saveSsaScope.dynamicScopeUsed = true;
    else
      tools.deleteEmitted(scopeLines);
    return ssaResult;
  }

  // (for-in key-symbol value-symbol obj form forms...)
  exportAPI("for_in", for_in, { evalCount: 0, compileHook: for_in_hook });
  function for_in(keySymbol, valueSymbol, obj, form, ...forms) {
    let scope = this;
    obj = _eval(obj, scope);
    if (scope[RETURN_SYMBOL]) return;
    scope = newScope(this, "for-in-scope");
    let val = NIL;
    if (isIterable(obj)) {
      let index = 0;
      for (let value of obj) {
        scope[keySymbol] = index++;
        scope[valueSymbol] = value;
        val = _eval(form, scope);
        if (scope[RETURN_SYMBOL]) return;
        for (let i = 0, formsLength = forms.length; i < formsLength ; ++i) {
          val = _eval(forms[i], scope);
          if (scope[RETURN_SYMBOL]) return;
        }
      }
      return val;
    }
    for (let key in obj) {
      let value = obj[key];
      scope[keySymbol] = key;
      scope[valueSymbol] = value;
      val = _eval(form, scope);
      if (scope[RETURN_SYMBOL]) return;
      for (let form of forms) {
        val = _eval(form, scope);
        if (scope[RETURN_SYMBOL]) return;
      }
    }
    return val;
  }
  function for_in_hook(args, ssaScope, tools) {
    let emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
    if (args.length < 4) throw new SchemeCompileError(`Bad for-in`);
    let indexVar = args[0], valueVar = args[1];
    if (!isAtom(indexVar)) throw new SchemeCompileError(`bad index variable in for-in ${indexVar}`);
    if (!isAtom(valueVar)) throw new SchemeCompileError(`bad value variable in for-in ${valueVar}`);
    let ssaIndexVarAtom = use(bind(indexVar)), ssaValueVarAtom  = use(bind(valueVar));
    let ssaObj = compileEval(args[2], ssaScope, tools);
    let ssaTmpScope = newTemp("scope_tmp");
    let saveSsaScope = ssaScope, scopeLines = [];
    scopeLines.push(emit(`let ${ssaTmpScope} = scope;`));
    ssaScope = newScope(ssaScope, "compiler-for-in-scope");
    let ssaIndexVar = newTemp(indexVar.description), ssaValueVar = newTemp(valueVar.description);
    ssaScope[indexVar] = ssaIndexVar;
    ssaScope[valueVar] = ssaValueVar;
    let ssaFn = newTemp('for_in_fn)');
    let ssaValue = `${BOTTOM}`;
    emit(`function ${ssaFn}(${ssaIndexVar}, ${ssaValueVar}) { // (for-in ${string(indexVar)} ${string(valueVar)} ...)`);
    let saveIndent = tools.indent;
    tools.indent += '  ';
    scopeLines.push(emit(`let scope = newScope(${ssaTmpScope}, "compiled-for-in-scope");`));
    scopeLines.push(emit(`scope[${ssaIndexVarAtom}] = ${ssaIndexVar};`));
    scopeLines.push(emit(`scope[${ssaValueVarAtom}] = ${ssaValueVar};`));
    for (let i = 3; i < args.length; ++i)
      ssaValue = compileEval(args[i], ssaScope, tools);
    emit(`return ${ssaValue};`)
    tools.indent = saveIndent;
    emit('}');
    tools.bindLiterally(isIterable, "isIterable");
    let ssaResult = newTemp('for_in_result');
    emit(`let ${ssaResult} = ${BOTTOM};`);
    emit(`if (isIterable(${ssaObj})) {`);
    emit(`  let index = 0;`);
    emit(`  for (let value of ${ssaObj})`);
    emit(`    ${ssaResult} = ${ssaFn}(index++, value);`);
    emit(`} else {`);
    emit(`  for (let key in ${ssaObj})`);
    emit(`    ${ssaResult} = ${ssaFn}(key, ${ssaObj}[key]);`);
    emit(`}`);
    if (ssaScope.dynamicScopeUsed)
      saveSsaScope.dynamicScopeUsed = true;
    else
      tools.deleteEmitted(scopeLines);
    return ssaResult;
  }

  // (for-of value-symbol obj form forms...)
  exportAPI("for_of", for_of, { evalCount: 0, compileHook: for_of_hook });
  function for_of(valueSymbol, obj, form, ...forms) {
    let scope = this;
    obj = _eval(obj, scope);
    if (scope[RETURN_SYMBOL]) return;
    scope = newScope(this, "for-of-scope");
    let val = BOTTOM;
    for (let value of obj) {
      scope[valueSymbol] = value;
      val = _eval(form, scope);
      if (scope[RETURN_SYMBOL]) return;
      for (let form of forms) {
        val = _eval(form, scope);
        if (scope[RETURN_SYMBOL]) return;
      }
    }
    return val;
  }
  function for_of_hook(args, ssaScope, tools) {
    let emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
    if (args.length < 3) throw new SchemeCompileError(`Bad for-of`);
    let valueVar = args[1];
    if (!isAtom(valueVar)) throw new SchemeCompileError(`bad value variable in for-of ${valueVar}`);
    let ssaValueVarAtom  = use(bind(valueVar));
    let ssaObj = compileEval(args[1], ssaScope, tools);
    let ssaValueVar = newTemp(valueVar.description);
    ssaScope[valueVar] = ssaValueVar;
    let ssaResult = newTemp('for_of_result'), ssaValue = `${BOTTOM}`;
    emit(`let ${ssaResult} = ${ssaValue};`)
    let ssaTmpScope = newTemp("scope_tmp");
    let saveSsaScope = ssaScope, scopeLines = [];
    scopeLines.push(emit(`let ${ssaTmpScope} = scope;`));
    ssaScope = newScope(ssaScope, "compiler-for-of-scope");
    emit(`{`);
    scopeLines.push(emit(`let scope = newScope(${ssaTmpScope}, "compiled-for-in-scope");`));
    emit(`  for (let ${ssaValueVar} of ${ssaObj}) {`);
    let saveIndent = tools.indent;
    tools.indent += '    ';
    scopeLines.push(emit(`scope[${ssaValueVarAtom}] = ${ssaValueVar};`));
    for (let i = 2; i < args.length; ++i)
      ssaValue = compileEval(args[i], ssaScope, tools);
    emit(`${ssaResult} = ${ssaValue};`);
    tools.indent = saveIndent;
    emit(`  }`);
    emit(`}`);
    if (ssaScope.dynamicScopeUsed)
      saveSsaScope.dynamicScopeUsed = true;
    else
      tools.deleteEmitted(scopeLines);
    return ssaResult;
  }

  // (while predicate obj form forms...)
  exportAPI("while", _while, { evalCount: 0, compileHook: while_hook });
  function _while(predicate, form, ...forms) {
    let scope = this;
    let val = BOTTOM;
    while (schemeTrue(_eval(predicate, scope))) {
      if (scope[RETURN_SYMBOL]) return;
      val = _eval(form, scope);
      if (scope[RETURN_SYMBOL]) return;
      for (let form of forms) {
        val = _eval(form, scope);
        if (scope[RETURN_SYMBOL]) return;
      }
    }
    return val;
  }
  function while_hook(args, ssaScope, tools) {
    let emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
    if (args.length < 2) throw new SchemeCompileError(`Bad for-of`);
    let predicate = args[0];
    let ssaResult = newTemp('while_result'), ssaValue = `${BOTTOM}`;
    emit(`let $ssaResult = ${ssaValue};`);
    emit(`for (;;) { // while loop`);
    let saveIndent = tools.indent;
    tools.indent += '  ';
    let ssaPredicateValue = compileEval(predicate, scope, tools);
    emit(`if (!schemeTrue(${ssaPredicateValue})) break;`)
    for (let i = 1; i < args.length; ++i)
      ssaValue = compileEval(args[i], ssaScope, tools);
    emit(`${ssaResult} = ${ssaValue};`)
    tools.indent = saveIndent;
    emit(`}`);
    return ssaResult;
  }

  // Something like this would be nice, but it's not quite right
  //  let setSymWithWith = new Function("symbol", "value", "scope",
  //    "with (scope) { return symbol = value }");
  // The point is that the JavaScript runtime HAS a suitable primitive; I just don't
  // think you can get at it directly from user code.

  exportAPI("setq", setq, { evalCount: 0, compileHook: setq_hook });
  function setq(symbol, valueForm, ...values) {
    let value = _eval(valueForm, this);
    if (this[RETURN_SYMBOL]) return;
    for (let valueForm of values) {
      value = _eval(valueForm, this);
      if (this[RETURN_SYMBOL]) return;
    }
    let result = setSym(symbol, value, this);
    return result;
  }
  function setq_hook(args, ssaScope, tools) {
    let emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use, scopeLines = tools.scopeLines;
    if (args.lenght < 1)
      throw new SchemeCompileError(`Bad setq arguments ${args}`);
    let varSym = args[FIRST], valForms = args[REST];
    let ssaValue = `${BOTTOM}`;
    for (let form of valForms)
      ssaValue = compileEval(form, ssaScope, tools);
    let boundVar = ssaScope[varSym];
    if (boundVar) {
      emit(`${boundVar} = ${ssaValue};`);
      let ssaVarSym = use(bind(varSym));
      scopeLines.push(emit(`scope[${ssaVarSym}] = ${ssaVarSym}`));
    } else {
      let ssaSetSym = use(bind(setSym));
      ssaScope.dynamicScopeUsed = true;
      emit(`${ssaSetSym}.call(scope, ${ssaValue});`);
    }
    return ssaValue;
  }

  exportAPI("set", set, { requiresScope: true });
  function set(symbol, value) { let result = setSym(symbol, value, this); return result; }

  function setSym(symbol, value, scope) {
    // Can _almost_ do this using "with." Maybe come back to that.
    // The hell of it is that JS has such a primitive internally to do
    // the same operation but you can't seem to get at it as a user.
    do {
      if (scope.hasOwnProperty(symbol)) {
        scope[symbol] = value;
        return val;
      }
      scope = Object.getPrototypeOf(scope);
    } while (scope && scope !== globalScope);  // I knew I'd use this someday!
    throw new EvalError(`set(q), ${string(symbol)} not in scope`)
  }

  //
  // throw/catch/finally
  //

  const js_throw = value => { throw value; return undefined; }; // "return" lets compiler use template
  exportAPI("throw", js_throw);

  // (catch (var forms) forms) -- JavaScript style
  exportAPI("catch", js_catch, { evalCount: 0, compileHook: js_catch_hook });
  function js_catch(catchClause, form, ...forms) {
    if (!isList(catchClause))
      throw new SchemeEvalError(`Bad catch clause ${string(catchClause)}`);
    let catchVar = catchClause[FIRST], catchForms = catchClause[REST];
    if (!isList(catchForms))
      throw new SchemeEvalError(`Bad catch clause ${string(catchClause)}`);
    if (!isList(catchForms))
      throw new SchemeEvalError(`Bad catch clause ${string(catchClause)}`);
    let val;
    try {
      val = _eval(form, this);
      if (this[RETURN_SYMBOL]) return;
      for (let form of forms) {
        val = _eval(form, this);
        if (this[RETURN_SYMBOL]) return;
      }
    } catch (e) {
      let scope = newScope(this, "js-catch-scope");
      scope[catchVar] = e;
      for (let form of catchForms) {
        val = _eval(form, scope);
        if (scope[RETURN_SYMBOL]) return;
      }
    }
    return val;
  }
  function js_catch_hook(args, ssaScope, tools) {
    let emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
    if (args.length < 2) throw new LogicError(`Bad js_catch`);
    let catchClause = args[0];
    if (!isList(catchClause) && isList(catchClause[REST]))
      throw new SchemeCompileError(`Bad catch clause ${string(catchClause)}`);
    let catchVar = catchClause[FIRST], catchForms = catchClause[REST];
    let ssaCatchSym = newTemp(catchVar);
    let ssaResult = newTemp('js_catch'), ssaValue = `${BOTTOM}`;
    let saveSSaScope = ssaScope, scopeLines = [];
    let ssaTmpScope = newTemp('tmp_scope');
    scopeLines.push(emit(`let ${ssaTmpScope} = scope;`));
    emit(`let ${ssaResult};`);
    emit(`try {`);
    let saveIndent = tools.indent;
    tools.indent += '  ';
    for (let i = 1; i < args.length; ++i)
      ssaValue = compileEval(args[i], ssaScope, tools);
    emit(`${ssaResult} = ${ssaValue};`);
    tools.indent = saveIndent;
    emit(`} catch (${ssaCatchSym}) {`);
    tools.indent += '  ';
    ssaScope = newScope(ssaScope, "compiled-js-catch-scope")
    ssaScope[catchVar] = ssaCatchSym;
    scopeLines.push(emit(`let scope = newScope(${ssaTmpScope}, "compiled-js-catch-scope");`));
    let ssaCatchAtom = use(bind(catchVar));
    scopeLines.push(emit(`scope[${ssaCatchAtom}] = ${ssaCatchSym};`));
    let ssaCatchVal = `${BOTTOM}`;
    for (let form of catchForms)
      ssaCatchVal = compileEval(form, ssaScope, tools);
    emit(`${ssaResult} = ${ssaCatchVal};`);
    tools.indent = saveIndent;
    emit(`}`);
    if (ssaScope.dynamicScopeUsed)
      saveSSaScope.dynamicScopeUsed = true;
    else
      tools.deleteEmitted(scopeLines);
    return ssaResult;
  }

  // (finally (forms) forms) -- finally
  exportAPI("finally", js_finally, { evalCount: 0, compileHook: js_finally_hook });
  function js_finally(finallyForms, form, ...forms) {
    if (!isList(finallyForms))
      throw new SchemeEvalError(`bad finally forms ${string(finallyForms)}`);
    let catchVar = finallyForms[FIRST], catchForms = finallyForms[REST];
    let val;
    try {
      val = _eval(form, this);
      if (this[RETURN_SYMBOL]) return;
      for (let form of forms) {
        val = _eval(form, this);
        if (this[RETURN_SYMBOL]) return;
      }
    } finally {
      for (let form of finallyForms) {
        _eval(form, scope);
        if (scope[RETURN_SYMBOL]) return;
      }
    }
    return val;
  }
  function js_finally_hook(args, ssaScope, tools) {
    let emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
    if (args.length < 2) throw new LogicError(`bad finally`);
    let finallyClause = args[0];
    let finallyForms = args[1];
    if (!isList(finallyClause))
      throw new SchemeCompileError(`bad finally clause ${string(finallyClause)}`);
    let ssaResult = newTemp('js_finally'), ssaValue = `${BOTTOM}`;
    emit(`let ${ssaResult};`);
    emit(`try {`);
    let saveIndent = tools.indent;
    tools.indent += '  ';
    for (let i = 1; i < args.length; ++i)
      ssaValue = compileEval(args[i], ssaScope, tools);
    emit(`${ssaResult} = ${ssaValue};`);
    tools.indent = saveIndent;
    emit(`} finally {`);
    tools.indent += '  ';
    for (let form of finallyForms)
      compileEval(form, ssaScope, tools);
    tools.indent = saveIndent;
    emit(`}`);
    return ssaResult;
  }

  // (def variable value)
  // (def (fn args) forms)
  exportAPI("def", def, { requiresScope: true, evalCount: 0, dontInline: true });
  function def(defined, value, ...rest) {
    let scope = this, name = defined;
    if (isList(defined)) {
      name = defined[FIRST];
      let params = defined[REST];
      value = lambda.call(scope, params, value, ...rest);
    } else {
      value = _eval(value, scope);
      if (scope[RETURN_SYMBOL]) return;
    }
    if (typeof name === 'string') name = Atom(name);
    // Prevent a tragic mistake that's easy to make by accident. (Ask me how I know!)
    if (name === QUOTE_ATOM) throw new SchemeEvalError("Can't redefine quote");
    if (typeof name !== 'symbol')
      throw new TypeError(`Must define symbol or string ${string(defined)}`);
    if (value != null &(typeof value === 'function' || typeof value === 'object'))
      namedObjects.set(value, name.description);
    globalScope[name] = value;
    // Make available to JavaScript as well
    let jsName = normalizeExportToJavaScriptName(name);
    if (jsName)
      globalScope[jsName] = value;
    return name;
  }

  function normalizeExportToJavaScriptName(name) {
    if (typeof name === 'symbol')
      name = name.description;
    let jsName = replaceAll(name, "-", "_");
    if (!JSIDENT1[jsName[0]]) {
      jsName = undefined;
    } else {
      for (let ch of jsName) {
        if (!JSIDENT2[ch]) {
          jsName = undefined;
          break;
        }
      }
    }
    return jsName;
  }

  const RETURN_SYMBOL = Symbol("RETURNS"), RETURNS_VALUE_SYMBOL = Symbol("RETURNS-VALUE"), RETURN_SCOPE_SYMBOL = Symbol('RETURN-SCOPE');
  exportAPI("RETURN_SYMBOL", RETURN_SYMBOL);

  //
  // Spread operator.
  //
  // The main use of parameter macros will be for the spread operator (...), but it could
  // also be used for constants like __FILE__, __LINE__, and __DATE__.
  //
  // It's never _necessary_ to specialize for the comppiled case, but if not the
  // compiler will kick out to the interpreter for evaluation.
  //
  exportAPI("spread", spread, { tag: EVALUATED_PARAMETER_MACRO_TAG });
  function spread(args, ssaScope, tools) {
    if (!isList(args))
      throw new SchemeEvalError(`bad spread operator arguments ${string(args)}`);
    let spreadArg = args[FIRST];
    if (ssaScope) {
      tools.macroCompiled = true;
      spreadArg = compileEval(spreadArg, ssaScope, tools);
    }
    // Returns Pair of arguments to stuff
    // and the rewritten (or not) remainder of the arg list.
    return new Pair(spreadArg, args[REST]);
  }

  //
  // Macros
  //

  exportAPI("defmacro", defmacro, { requiresScope: true, evalCount: 0, dontInline: true });
  function defmacro(nameAndParams, ...forms) {
    if (!isList(nameAndParams)) new TypeError(`First parameter must be a list ${forms}`);
    let name = Atom(nameAndParams[FIRST]);
    let params = nameAndParams[REST];
    if (typeof name !== 'symbol') new TypeError(`Function name must be an atom or string ${forms}`)    
    let lambda = new Pair(LAMBDA_ATOM, new Pair(params, forms));
    let compiledFunction = compile_lambda.call(this, name, name.description, lambda);
    examineFunctionForCompilerTemplates(name, compiledFunction, { tag: MACRO_TAG });
    namedObjects.set(compiledFunction, name.description);
    globalScope[name] = compiledFunction;
    // Make available to JavaScript as well
    let jsName = normalizeExportToJavaScriptName(name);
    if (jsName)
      globalScope[jsName] = compiledFunction;
    globalScope[name] = compiledFunction;
    return name;
  }

  // (when (cond) form...) => (if (cond) (begin form...))
  // Not using defmacro since that creates a binding, and the core doesn't do that.
  exportAPI("when", when, { tag: MACRO_TAG });
  function when(params) {
    return list(
        globalScope.ifelse,
        params[FIRST],
        new Pair(globalScope.begin, params[REST]));
  }

  //
  // This is where the magic happens
  //
  // Beware that compileEval closely parallels this function, if you make a change
  // here you almost certainly need to make a corresponding one there.
  //

  exportAPI("_eval", _eval, { requiresScope: true, dontInline: true });
  function _eval(form, scope = this) {
    // Can't be called "eval" because "eval", besides being a global definition,
    // is effectively a keyword in JavaScript.
    if (scope[RETURN_SYMBOL])
      throw new LogicError(`someone forgot to check scope[RETURNS_SYMBOL]`)
    for (;;) { // Macro expansion loop
      if (isNil(form)) return form;
      if (isPrimitive(form)) return form;
      if (typeof form === 'symbol') { // atom resolution is the most common case
        let val = scope[form];
        if (val === undefined) checkUndefinedInScope(form, scope);
        return val;
      }
      if (TRACE_INTERPRETER)
        console.log("EVAL", string(form));
      if (isList(form) && !isArray(form)) {
        let fn = form[FIRST];
        let args = form[REST];
        if (fn === QUOTE_ATOM) // QUOTE is a special function that will do this but catch it here anyway.
          return args[FIRST];
        if (typeof fn === 'symbol') {  // check for a macro _without evaluating_
          let symVal = scope[fn];
          if (typeof symVal === 'function') {
            let parameterDescriptor = symVal[PARAMETER_DESCRIPTOR];
            if (parameterDescriptor != null) {
              let tag = parameterDescriptor & 0xff;
              if (tag === MACRO_TAG || tag === PARAMETER_MACRO_TAG) {
                form = symVal.call(scope, args);
                continue;
              }
            }
          }
        }
        fn = _eval(fn, scope);
        if (scope[RETURN_SYMBOL]) return;
        if (typeof fn !== 'function') throwBadForm();
        // See makeParameterDescriptor for the truth, but
        //   parameterDescriptor = (evalCount << 20) | (requiredCount << 8) | tag
        let parameterDescriptor = fn[PARAMETER_DESCRIPTOR] ?? examineFunctionForParameterDescriptor(fn);
        let requiredCount = (parameterDescriptor >> 8) & 0xfff;
        let evalCount = parameterDescriptor >> 19 >>> 1;  // restores MAX_INTEGER to MAX_INTEGER
        let tag = parameterDescriptor & 0xff;
        // Run through the arg list evaluating args
        let argv = [], argCount = 0;
        while (moreList(args)) {
          let arg = args[FIRST];
          let nextArg = handleParameterMacroIfPresent(argv, arg, args[REST], argCount, evalCount);
          if (nextArg !== undefined) {
            args = nextArg;
            argCount = argv.length;
            continue;
          }
          if (argCount < evalCount) {
            arg = _eval(arg, scope);
            if (scope[RETURN_SYMBOL]) return;
          }
          argv.push(arg), argCount += 1;
          args = args[REST];
        }
        let jitCompiled = fn[JITCOMPILED];
        if (jitCompiled)
          fn = jitCompiled;
        let fName = namedObjects.get(fn) ?? fn.name; // Used for more than just tracing!
        if (argCount >= requiredCount) {
          if (TRACE_INTERPRETER) {
            let logArgs = [ "APPLY (eval)", fName, ...argv ];
            console.log.apply(scope, logArgs);
          }
          let result = fn.apply(scope, argv);
          if (TRACE_INTERPRETER)
            console.log("RETURNS", fName, result);
          return result;
        }
        // Insufficient # of parameters. If there is at least one argument, create a closure.
        // Otherwise, call the function with no arguments. Returning the function is tempting
        // but that would be error-prone and create an asymmetry between functions with zero
        // required parameters, like apropos, and those that have required parameters.
        // The function itself can decide what to do if it receives "undefined" as its first argument.
        if (argCount === 0) {
          if (TRACE_INTERPRETER) {
            let logArgs = [ "APPLY (degenerate)", fName, ...argv ];
            console.log.apply(scope, logArgs);
          }
          let result = fn.apply(scope, argv);
          if (TRACE_INTERPRETER)
            console.log("RETURNS", fName, result);
          return result;
        }

        // OK, now create a closure.
        const boundArgv = argv, boundFunction = fn;
        let bindingClosure = (...args) => boundFunction.call(scope, ...boundArgv, ...args);
        // A closure function leads a double life: as a closure function but also a closure form!
        // Dig out the original function's closure, if it had one.
        let closureBody = fn[REST];
        let closureParams = NIL, closureForms;
        if (closureBody) {
          scope = closureBody[FIRST];
          scope = newScope(scope, "closure-scope");
          let lambdaBody = closureBody[REST];
          if (fn[FIRST] === SCLOSURE_ATOM)  // Skip the evalCount param
            lambdaBody = lambdaBody[REST];
          closureParams = lambdaBody[FIRST];
          closureForms = lambdaBody[REST];
          for (let i = 0; i < argCount; ++i, closureParams = closureParams[REST]) {
            if (!isList(closureParams)) throw new LogicError(`Shouldn't happen`);
            let param = closureParams[FIRST];
            if (isList(param) && typeof param[FIRST] === 'symbol')
              param = [FIRST];
            scope[param] = argv[i];
          }
        } else {
          scope = newScope(scope, "closure-scope");
          let fnInfo = analyzeJSFunction(fn);
          let params = fnInfo.params, restParam = fnInfo.restParam, paramStr = '', sep = '';
          for (let i = 0, lng = params.length; i < lng; ++i)
            paramStr += sep + params[i], sep = ", ";
          if (restParam)
            paramStr += `${sep}...${restParam}`;
          closureForms = new Pair(Atom(`{*js-function-${fName}(${paramStr})*}`), NIL);
          if (restParam)
            closureParams = Atom(restParam);
          for (let i = params.length; i > argCount; --i)
            closureParams = new Pair(Atom(params[i-1]), closureParams);
          for (let i = 0; i < argCount; ++i)
            scope[Atom(params[i])] = argv[i];
        }
        if (evalCount !== MAX_INTEGER) {
          evalCount -= argCount;
          if (evalCount < 0)
            evalCount = 0;
          bindingClosure[FIRST] = SCLOSURE_ATOM;
          bindingClosure[REST] = new Pair(scope, new Pair(evalCount, new Pair(closureParams, closureForms)));
        } else {
          bindingClosure[FIRST] = CLOSURE_ATOM;
          bindingClosure[REST] = new Pair(scope, new Pair(closureParams, closureForms));
        }
        bindingClosure[LIST] = bindingClosure[MORELIST] = bindingClosure[ITERATE_AS_LIST] = true;
        bindingClosure[CLOSURE_ATOM] = true; // marks closure for special "printing"
        requiredCount -= argCount;
        if (requiredCount < 0) throw new LogicError(`Shouldn't happen`);
        bindingClosure[PARAMETER_DESCRIPTOR] = makeParameterDescriptor(requiredCount, evalCount);
        return bindingClosure;
      }

      // Special eval for JS arrays and objects:
      //   Values that are evaluated and placed in
      //   a new Object or Array in correspoding position.
      // TODO: Investigate Symbol.species (also for map, etc.)
      if (form !== null && typeof form === 'object') {
        if (isArray(form)) {
          let res = [];
          while (moreList(form)) {
            let element = form[FIRST];
            let nextArg = handleParameterMacroIfPresent(res, element, form[REST], 0, MAX_INTEGER);
            if (nextArg !== undefined) {
              form = nextArg;
              continue;
            }
            res.push(_eval(element, scope));
            if (scope[RETURN_SYMBOL]) return;
            form = form[REST];
          }
          return res;
        } else {
          let res = {};
          for (let key of [...Object.getOwnPropertyNames(form), ...Object.getOwnPropertySymbols(form)]) {
            let value = form[key];
            let insertion = handleParameterMacroIfPresentInObjectLiteral(key, value);
            if (insertion !== undefined) {
              Object.assign(res, insertion);
              continue;
            }
            if (key === EVALUATE_KEY_VALUE_SYMBOL) {
              key = value[0];
              value = value[1];
              key = _eval(key, scope);
              if (scope[RETURN_SYMBOL]) return;
            }
            value = _eval(value, scope);
            if (scope[RETURN_SYMBOL]) return;
            res[key] = value;
          }
          return res;
        }
      }
      throw new LogicError(`Shouldn't happen. All cases should be handled above`);
    }  // macro loop

    function throwBadForm() {
      throw new SchemeEvalError(`Bad form ${string(form)}`);
    }

    function handleParameterMacroIfPresent(argv, arg, args, argCount, evalCount) {
      if (typeof arg === 'symbol') {
        let symVal = scope[arg];
        if (typeof symVal === 'function') {
          let parameterDescriptor = symVal[PARAMETER_DESCRIPTOR];
          if (parameterDescriptor != null) {
            let tag = parameterDescriptor & 0xff;
            if (tag === PARAMETER_MACRO_TAG || (argCount < evalCount && tag === EVALUATED_PARAMETER_MACRO_TAG)) {
              let macroResult = symVal.call(scope, args);
              if (!isList(macroResult))
                throw new SchemeEvalError(`bad parameter macro result ${string(macroResult)}`);
              return macroResult;
            }
          }
        }
      }
      return undefined;
    }

    function handleParameterMacroIfPresentInObjectLiteral(key, value) {
      if (typeof key === 'symbol') {
        let symVal = scope[key];
        if (typeof symVal === 'function') {
          let parameterDescriptor = symVal[PARAMETER_DESCRIPTOR];
          if (parameterDescriptor != null) {
            let tag = parameterDescriptor & 0xff;
            if (tag === EVALUATED_PARAMETER_MACRO_TAG) {
              let macroResult = symVal.call(scope, new Pair(value, NIL));
              if (!moreList(macroResult))
                throw new SchemeEvalError(`bad parameter macro result ${string(macroResult)}`);
              let insert = macroResult[FIRST];
              // macroResult[REST] isn't used in this case
              return _eval(insert, scope);
            }
          }
        }
      }
      return undefined;
    }
  }

  function checkUndefinedInScope(sym, scope) {
    // We got an "undefined" result and it's probably an unresolved reference, but maybe not!
    // This is a bit costly but it doesn't happen often and the alternative is to just throw
    // an error. The advantage is that the compiler and interpreter have the same semantics
    // around "undefined."
    while (scope) {
      if (scope.hasOwnProperty(sym)) return;
      scope = Object.getPrototypeOf(scope);
    }
    throw new SchemeEvalError(`Undefined ${string(sym)}`);
  }

  function examineFunctionForParameterDescriptor(fn, evalCount = MAX_INTEGER, tag = 0) {
    let { requiredCount } = analyzeJSFunction(fn);
    return fn[PARAMETER_DESCRIPTOR] = makeParameterDescriptor(requiredCount, evalCount, tag);
  }

  function makeParameterDescriptor(requiredCount, evalCount = MAX_INTEGER, tag = 0) {
    if (requiredCount < 0 || requiredCount >= 0xfff)
      throw new LogicError(`RequiredCount out of range`);
    if (evalCount < 0 || (evalCount !== MAX_INTEGER && evalCount >= 0xfff))
      throw new LogicError(`Too many evaluated paramaters`);
    if (tag < 0 || (tag >= 0xff))
      throw new LogicError(`Too many evaluated paramaters`);
    return (evalCount << 20) | (requiredCount << 8) | tag;
  }

  exportAPI("apply", apply, { requiresScope: true, dontInline: true });
  function apply(fn, args, scope = this) {
    let argv = args;
    if (!isArray(argv)) {
      argv = [];
      for ( ; moreList(args); args = args[REST])
        argv.push(args[FIRST])
    }
    let fName;
    if (TRACE_INTERPRETER) {
      fName = namedObjects.get(fn) ?? fn.name;
      let logArgs = [ "APPLY (api)", fName, ...argv ];
      console.log.apply(scope, logArgs);
    }
    let result = fn.apply(scope, argv);
    if (TRACE_INTERPRETER)
      console.log("RETURNS", fName, result);
    return result;
  }

  // (\ (params) (form1) (form2) ...)
  exportAPI("lambda", lambda, { requiresScope: true, evalCount: 0, dontInline: true });
  function lambda(params, ...forms) {
    let lambda = new Pair(LAMBDA_ATOM, new Pair(params, forms));
    let scope = this;
    let schemeClosure = new Pair(CLOSURE_ATOM, new Pair(scope, lambda[REST]));
    return makeLambdaClosure(scope, params, lambda, forms, schemeClosure);
  }

  // (\# evalCount (params) (body1) (body2) ...)
  exportAPI("special_lambda", special_lambda, { requiresScope: true, evalCount: 0, dontInline: true });
  function special_lambda(evalCount, params, ...forms) {
    let lambda = new Pair(SLAMBDA_ATOM, new Pair(evalCount, new Pair(params, forms)));
    let scope = this;
    let schemeClosure = new Pair(SCLOSURE_ATOM, new Pair(scope, lambda[REST]));
    return makeLambdaClosure(scope, params, lambda, forms, schemeClosure, evalCount);
  }

  //
  // Beware that compileClosure closely parallels this function. If you make a change
  // here, you almost certainly need to make a change there. "string" also
  // has special handling for "printing" closures. In particular, closures are
  // decorated with a CLOSURE_ATOM symbol to clue the string function to print
  // it as a closure. Closures are also decorated with FIRST, REST, LIST and LIST
  // so that they look exactly like lists to the SchemeJS runtime.
  //
  function makeLambdaClosure(scope, lambdaParams, lambda, forms, schemeClosure, evalCount = MAX_INTEGER) {
    // Examine property list and throw any errors now rather than later.
    // In general, do more work here so the closure can do less work when executed.
    let params = lambdaParams, paramCount = 0, requiredCount;
    let paramv = [], restParam;
    for ( ; moreList(params); params = params[REST]) {
      let param = params[FIRST];
      if (isList(param)) {
        if (!isAtom(param[FIRST]) )
          throwBadLambda(lambda, `what's this?  ${string(param)}`);
        if (!requiredCount)
          requiredCount = paramCount;
        param = param[FIRST];
      } else if (!isAtom(param)) {
        throwBadLambda(lambda, `parameter ${string(param)} not an atom`);
      }
      if (param === REST_PARAM_ATOM) {
        if (!isList(params[REST]) || !isNil(params[REST][REST]))
          throwBadLambda(lambda, `bad "rest" parameter ${string(param)}`);
        params = params[REST];
        restParam = params[FIRST];
        params = params[REST];
        break;
      }
      paramCount += 1;
      paramv.push(param.description);
    }
    if (!isNil(params))
      throwBadLambda(`bad parameter list ${string(params)}`);
    if (!requiredCount)
      requiredCount = paramCount;
    let jitCount = JIT_THRESHOLD != null ? JIT_THRESHOLD|0 : undefined;
    function lambdaClosure(...args) {
      let jitFn = lambdaClosure[JITCOMPILED];
      if (jitFn)
        return jitFn.apply(this, args);
      if (JIT_THRESHOLD != null) {  // Disable by optioning jitThreshold as undefined
        // SchemeJS will almost always call the jitFn directly, but external JS will still call this closure.
        if (--jitCount < 0) {
          jitCount = JIT_THRESHOLD;
          jitFn = lambdaClosure[JITCOMPILED] = compile_lambda.call(scope, undefined, namedObjects.get(lambdaClosure), lambda, lambdaClosure);
          return jitFn.apply(this, args);
        }
      }
      scope = newScope(scope, "lambda-scope");
      let params = lambdaParams, i = 0, argLength = args.length;
      for ( ; moreList(params); ++i, params = params[REST]) {
        let param = params[FIRST], optionalForms, arg;
        if (isList(param) && typeof param[FIRST] === 'symbol') {
          optionalForms = param[REST];
          param = param[FIRST];
        }
        if (param === REST_PARAM_ATOM) {
          let param = params[REST][FIRST], val = args.slice(i);
          scope[param] = val;
          break;
        }
        arg = args[i];  // undefined if i >= length OR if deliberately undefined
        if (arg === undefined) {
          arg = NIL;
          if (optionalForms) {
          for ( ; moreList(optionalForms); optionalForms = optionalForms[REST])
            arg = _eval(optionalForms[FIRST], scope);
            if (scope[RETURN_SYMBOL]) return;
          }
        }
        scope[param] = arg;
      }
      scope[RETURN_SCOPE_SYMBOL] = scope;
      let result = BOTTOM;
      for (let form of forms) {
        result = _eval(form, scope);
        if (scope[RETURN_SYMBOL])
          return scope[RETURNS_VALUE_SYMBOL];
      }
      return result;
    }
    lambdaClosure[PARAMETER_DESCRIPTOR] = makeParameterDescriptor(requiredCount, evalCount);
    lambdaClosure[FIRST] = schemeClosure[FIRST];
    lambdaClosure[REST] = schemeClosure[REST];
    lambdaClosure[LIST] = lambdaClosure[ITERATE_AS_LIST] = lambdaClosure[MORELIST] = true;
    lambdaClosure[CLOSURE_ATOM] = true; // marks closure for special "printing"
    // Because the closure has a generic (...args) parameter, the compiler needs more info
    // to be able to create binding closures over it.
    lambdaClosure[COMPILE_INFO] = { params: paramv, restParam, requiredCount, evalCount };
    return lambdaClosure;
  }

  function throwBadLambda(lambda, msg) { throw new SchemeEvalError(`Bad lambda ${lambda}` + (msg ? `, ${msg}` : '')) }

  exportAPI("return", _return, { requiresScope: false, compileHook: return_hook })
  function _return(...values) {
    let scope = this, value = BOTTOM;
    if (values.length > 0)
      value = values[values.length-1];
    let returnScope = scope[RETURN_SCOPE_SYMBOL];
    returnScope[RETURNS_VALUE_SYMBOL] = value;
    returnScope[RETURN_SYMBOL] = true;
    return undefined;  // just to be clear... this does NOT return the value
  }
  function return_hook(args, ssaScope, tools) {
    let ssaValue = `${BOTTOM}`;
    for (let arg of args)
      ssaValue = compileEval(arg, ssaScope, tools);
    tools.emit(`return ${ssaValue};`);
    return ssaValue;
  }

  exportAPI("is_closure", is_closure, { evalCount: 1, compileHook: closureP_hook })
  function is_closure(a, t = true, f = false) {
    if (isClosure(a)) return isPrimitive(t) ? t : _eval(t, this);
    else return isPrimitive(f) ? f : eval(f, this);
  }
  function closureP_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_closure', `is_closure(*)`);
  }
  exportAPI("isClosure", isClosure, { dontInline: true });
  function isClosure(obj) {
    return isList(obj) && (obj[FIRST] === CLOSURE_ATOM || obj[FIRST] === SCLOSURE_ATOM);
  }

  const ESCAPE_STRINGS = { '0': '\0', "'": "'", '"': '"', '\\': '\\', 
                           n: '\n', r: '\r', v: '\v', t: '\t', b: '\b', f: '\f' };
  const STRING_ESCAPES = (() => {
    let res = {};
    for (let [key, value] of Object.entries(ESCAPE_STRINGS))
      res[value] = '\\' + key;
    return res;
  })();
  exportAPI("ESCAPE_STRINGS", ESCAPE_STRINGS);

  //
  // Implements "toString()" for SchemeJS objects.
  // We can't just implement toString() because it needs to work for
  // non-Object types too, but Cons.toString() calls this.
  // "string" prints anything interesting to the SchemeJS runtime, so it's super
  // helpful to use it for "watch" expressions in the debugger. For instance when
  // debugging "_eval", watch "string(form)".
  // I generally refer to what this function does as "printing," which isn't quite
  // accurate, though it is used when printing things. "Stringifying" sounds silly
  // and I can't think of a suitable verb that doesn't. So "printing" it is.
  //
  // In general, to see the SchemeJS view of an object, invoke (string obj) and
  // to see the JavaScript view of an obect, invoke (String obj). This is
  // particularly true of functions (closures) since a SchemeJS closure is
  // simultaneously a JavaScript function and a SchemeJS closure.
  //
  exportAPI("to_string", a => string(a));
  exportAPI("string", string, { dontInline: true });
  function string(obj, opts = {}) {
    opts = { ...schemeOpts, ...opts };
    let stringWrap = opts.stringWrap ?? 100;
    let wrapChar = opts.wrapChar ?? "\n";
    let maxCarDepth = opts.maxCarDepth ?? 100;
    let maxCdrDepth = opts.maxCdrDepth ?? 10000;
    let indentMore = opts.indentMorw ?? "  ";
    let line = "", lines = [], sep = "", prefix = "", indent = "";
    toString(obj, maxCarDepth, maxCdrDepth);
    if (lines.length === 0) return line;
    if (line)
      lines.push(line);
    return lines.join(wrapChar);
    function toString(obj, maxCarDepth, maxCdrDepth) {
      if (maxCarDepth < 0 || maxCdrDepth < 0) return put("....");
      if (obj === undefined) return put("undefined");
      if (obj === null) return put( "null");   // remember: typeof null === 'object'!
      let objType = typeof obj;
      let saveIndent = indent;
      if (obj[CLOSURE_ATOM] || objType === 'object') {
        // MUST check SUPERLAZY before the isNil test, which will cause eager evaluation of
        // a LazyIteratorList, cause it to call next() and mutate into something else.
        if (obj[SUPERLAZY] && displayAsList(obj))
          return put("{?}");
        if (obj[SCOPE_TYPE_SYMBOL]) {
          let symStrs = "";
          if (obj !== globalScope) {
            for (let sym of Object.getOwnPropertySymbols(obj)) {
              if (!isAtom(sym)) continue; // Not an atom (e.g. SCOPE_TYPE_SYMBOL)
              let desc = sym.description;
              let value = string(obj[sym]);
              let descVal = ` ${desc}=${value}`;
              if (symStrs.length + descVal.length > 30) {
                symStrs += "...";  // ... signifies more to this list but maxed out
                break;
              }
              symStrs += descVal;
            }
          }
          return put(`{*${obj[SCOPE_TYPE_SYMBOL]}*${symStrs}}`);
        }
        if (isNil(obj)) return put("()"); // Be careful about []!
        if (obj[LAZYREST]) {
          put("(");
          sep = "";
          if (obj[LAZYFIRST])
            put("{?}");
          else
            toString(obj[FIRST], maxCarDepth-1, maxCdrDepth);
          sep = " ";
          return put("...)", true);
        }
        if (displayAsList(obj)) {
          put("(");
          indent += indentMore;
          sep = "";
          if (!obj[LAZYFIRST]) {
            let objCar = obj[FIRST];
            if ((objCar === LAMBDA_ATOM || objCar === SLAMBDA_ATOM ||
                objCar === CLOSURE_ATOM || objCar === SCLOSURE_ATOM)) {
              if (objCar === LAMBDA_ATOM)
                put(lambdaStr)
              else if (objCar === SLAMBDA_ATOM)
                put(lambdaStr + "#");
              else
                toString(objCar); //%%closure or %%%closure
              sep = " ";
              obj = obj[REST];
              if (isList(obj) && (objCar === CLOSURE_ATOM || objCar === SCLOSURE_ATOM)) {
                toString(obj[FIRST]);  // scope
                sep = " ";
                obj = obj[REST];
              }
              if (isList(obj) && (objCar === SLAMBDA_ATOM || objCar === SCLOSURE_ATOM)) {
                toString(obj[FIRST]);  // evalCount
                sep = " ";
                obj = obj[REST];
              }
              if (isList(obj)) {
                if (typeof obj[FIRST] === 'symbol')
                  put(`(. ${obj[FIRST].description})`); // Special printing for initial "rest" param
                else
                  toString(obj[FIRST])
                sep = " ";
                obj = obj[REST];
              }
            }
          }
          // displayAsList here so that hybrid lists will print as "(a b . [c d])"
          while (moreList(obj) && displayAsList(obj)) {
            if (obj[LAZYFIRST]) {
              put("{?}");
            } else if (obj[COMPILED]) {
              toString(obj[FIRST])
              sep = "";
              put(`{*compiled-${obj[COMPILED]}*}`);
              sep = " ";
            } else {
              toString(obj[FIRST], maxCarDepth-1, maxCdrDepth);
            }
            sep = " ";
            if (obj[LAZYREST])
              return put(". {?})", true);
            obj = obj[REST];
            maxCdrDepth -= 1;
            if (maxCdrDepth < 0)
              return put("....)", true);
            if (obj != null && obj[SUPERLAZY])
              return put(". {?})", true);
          }
          if (!isNil(obj)) {
            put(".");
            sep = " ";
            toString(obj, maxCarDepth, maxCdrDepth-1);
          }
          sep = "";
          put(")", true);
          indent = saveIndent;
          return;
        }
        if (isArray(obj)) {
          put("[");
          indent += indentMore;
          sep = "";
          for (let item of obj) {
            toString(item, maxCarDepth-1, maxCdrDepth);
            maxCdrDepth -= 1;
            if (maxCdrDepth < 0)
              return put("....]", true);
            sep = ", ";
          }
          sep = "";
          put("]", true);
          indent = saveIndent;
          return;
        }
        {
          // Plain object
          if (obj === globalThis) return put("{*globalThis*}");
          put("{");
          indent += indentMore;
          sep = "";
          for (let name of [ ...Object.getOwnPropertyNames(obj), ...Object.getOwnPropertySymbols(obj) ]) {
            let item = obj[name];
            if (name === EVALUATE_KEY_VALUE_SYMBOL) {
              prefix = "[";
              toString(item[0], maxCdrDepth, maxCdrDepth);
              prefix = "]: ";
              toString(item[1], maxCarDepth, maxCdrDepth);
            } else {
              prefix = `${string(name)}: `;
              toString(item, maxCarDepth-1, maxCdrDepth);
              maxCdrDepth -= 1;
              if (maxCdrDepth < 0)
                return put("....]", true);
            }
            sep = ", ";
          }
          sep = "";
          put("}", true);
          indent = saveIndent;
          return;
        }
      }
      if (typeof obj === 'function' && !obj[LIST]) {
        let fnDesc = analyzeJSFunction(obj);
        let parameterDescriptor = obj[PARAMETER_DESCRIPTOR] ?? (MAX_INTEGER << 16);
        let evalCount = parameterDescriptor >> 19 >>> 1;  // restores MAX_INTEGER to MAX_INTEGER
        let tag = parameterDescriptor & 0xff;
        let name = namedObjects.get(obj) ?? fnDesc.name ?? obj.name;
        let params = fnDesc.printParams;
        let printBody = fnDesc.printBody;
        if (fnDesc.valueTemplate && !fnDesc.bodyTemplate && !printBody)
          return put(`{${params} => ${fnDesc.valueTemplate}}`);
        if (printBody && (printBody.length > 80 || printBody.includes('\n')))
          printBody = '';
        let deets = evalCount === MAX_INTEGER ? '' : `# ${evalCount}`;
        if (tag === MACRO_TAG) deets = '-macro';
        else if (tag === EVALUATED_PARAMETER_MACRO_TAG) deets = '-parameter-macro';
        put(`{function${deets} ${name}${params}${printBody}`);
        sep = "";
        return put("}", true);
      }
      if (objType === 'symbol') {
        if (obj === LAMBDA_ATOM) return put(lambdaStr);
        if (obj === SLAMBDA_ATOM) return put(lambdaStr+"#");
        if (isAtom(obj)) return put(obj.description);
        return put(`Symbol(${obj.description})`);
      }
      if (objType === 'string') {
        let str = '"';
        for (let ch of obj) {
          let replace = STRING_ESCAPES[ch];
          if (replace) {
            ch = replace;
          } else {
            let charCode = ch.charCodeAt(0);
            if (charCode < 0x20 || charCode >= 0x7f) {  // XXX TODO: include Latin-1?
              ch = jsChar(charCode);
            }
          }
          str += ch;
        }
        str += '"';
        return put(str);
      }
      if (objType === 'bigint') {
        return put(`${String(obj)}n`);
      }
      return put(String(obj));
    }
    function put(str, nobreak) {
      if (!nobreak && line.length > 0 && line.length + str.length > stringWrap) {
        line += sep;
        lines.push(line);
        line = prefix + str;
      } else {
        line += sep + prefix + str;
      }
      sep = prefix = "";
    }
  }

  function jsChar(charCode) { return `\\u{${charCode.toString(16)}}` }

  exportAPI("analyzeJSFunction", analyzeJSFunction, { dontInline: true });
  function analyzeJSFunction(fn) {
    // The idea here is to use the intrinsic functions themselves as code generation
    // templates. That works as long as the functions don't call _eval. In that case
    // we need specialized compile hooks to generate code.
    //
    // Super janky passer. It only has to recoginze things that fn.toSting() can return.
    // But it's conservative in that it doesn't recognize any functions that
    // don't end in a particular sequence of tokens or that use "return" more than once.
    let analyzed = analyzedFunctions.get(fn);
    if (analyzed)
      return analyzed;
    let str = fn.toString(), pos = 0, token = "", paramPos = 0, requiredCount;
    let name = fn.name, params = [], restParam, valueTemplate, bodyTemplate, native = false, printParams, printBody;
    parse: {
      if (nextToken() === 'function') {
        paramPos = pos;
        if (nextToken() !== '(') {
          name = token;
          paramPos = pos;
          nextToken();
        }
        if (token !== '(') break parse;
        parseParams();
        let printBodyPos = pos;
        nextToken();
        parseBody();
        printBody = str.substr(printBodyPos);
      } else { // Arrow functions
        if (token === '(') {
          parseParams();
        } else {
          params.push(token);
          printParams = str.substring(paramPos, pos);
        }
        if (nextToken() !== '=>') break parse;
        while (WSNL[str[pos]]) ++pos;
        let possibleValue = str.substr(pos);
        if (nextToken() === '{')
          parseBody();
        else
          valueTemplate = possibleValue;
      }
    }
    if (requiredCount === undefined)
      requiredCount = params.length;
    if (native)
      requiredCount = 0;
    let res = { name, params, restParam, valueTemplate, bodyTemplate, printBody, printParams, native, requiredCount };
    analyzedFunctions.set(fn, res);
    return res;

    function parseParams() {
      while (nextToken() && token !==')') {
        scarfParamDefault();
        if (token === ',') nextToken();
        if (token === ')') break;
        if (token === '...')
          restParam = nextToken();
        else
          params.push(token);
      }
      printParams = str.substring(paramPos, pos);
    }

    function scarfParamDefault() {
      if (token === '=') {
        if (requiredCount === undefined)
          requiredCount = params.length-1;
        let delimCount = 0;
        nextToken();
        while (token) {
          if (delimCount === 0 && (token === ',' || token === ')')) break;
          if ('({['.includes(token)) delimCount += 1;
          if (')}]'.includes(token)) delimCount -= 1;
          nextToken();
        }
      }
    }

    function parseBody() {
      if (token === '{') {
        while (WSNL[str[pos]]) ++pos;
        let bodyPos = pos, returnPos = pos, nTok = 0;
        while (nextToken() && token !== 'return') {
          returnPos = pos;
          if (nTok++ === 0 && token === '[') {
            if (nextToken() === 'native' && nextToken() === 'code' &&
                nextToken() === ']' && nextToken() === '}' && nextToken() === '') {
              native = true;
              return;
            }
          }
        }
        if (token === 'return') {
          // If there's a return, it has to be followed by a variable,
          // an optional semicolon, an "}", and NOTHING FURTHER.
          // We capture the name of the return variable and clip the body
          // prior to the return.
          let possibleValue = nextToken();
          while (nextToken() === ';');
          if (token !== '}') return;
          if (nextToken() !== '') return;
          valueTemplate = possibleValue;
          bodyTemplate = str.substring(bodyPos, returnPos);
        }
      }
    }

    function nextToken() {
      // Super janky tokenizer.
      // Most of what it returns is garbage, but it correctly returns anything we actually care about.
      // The assumption is that what JS returns is well-formed, so it takes a lot of liberties.
      if (pos >= str.length) return token = '';
      let ch = str[pos]; // ch is always str[pos]
      do {
        while (WSNL[ch]) ch = str[++pos];
        if (ch === '/' && str[pos+1] === '/') {
          ch = str[pos += 2];
          while (ch && ch !== '\n') ch = str[pos++];
          continue;
        }
        if (ch === '/' && str[pos+1] === '*') {
          ch = str[pos += 2];
          while (ch && !(ch === '*' && str[pos+1] === '/')) ch = str[pos++];
          ch = str[pos += 2];
          continue;
        }
      } while (false);
      if (JSIDENT1[ch]) {
        let tok = ch;
        ch = str[++pos];
        while (JSIDENT2[ch]) {
          tok += ch;
          ch = str[++pos];
        }
        return token = tok;
      }
      if (ch === '=' && str[pos+1] === '>') {
        pos += 2;
        return token = "=>";
      }
      if (ch === '.' && str[pos+1] === '.' && str[pos+2] === '.') {
        pos += 3;
        return token = "...";
      }
      if (ch === '"' || ch === "'" || ch === '`') {
        let qc = ch;
        ch = str[++pos];
        while (ch && ch !== qc) {
          if (ch === '\\') ++pos;
          ch = str[++pos]
        }
        ++pos;
        return token = 'quoted';
      }
      return token = str[pos++] ?? '';
    }
  }

  // (compile (fn args) forms) -- defines a compiled function
  // (compile lambda) -- returns a compiled lambda expression
  exportAPI("compile", compile, { requiresScope: true, evalCount: 0, dontInline: true });
  function compile(nameAndParams, ...forms) {
    if (!isList(nameAndParams)) new TypeError(`First parameter must be a list ${forms}`);
    let name = Atom(nameAndParams[FIRST]);
    let params = nameAndParams[REST];
    if (typeof name !== 'symbol') new TypeError(`Function name must be an atom or string ${forms}`)    
    let lambda = new Pair(LAMBDA_ATOM, new Pair(params, forms));
    let compiledFunction = compile_lambda.call(this, name, name.description, lambda);
    namedObjects.set(compiledFunction, name.description);
    globalScope[name] = compiledFunction;
    // Make available to JavaScript as well
    let jsName = normalizeExportToJavaScriptName(name);
    if (jsName)
      globalScope[jsName] = compiledFunction;
    globalScope[name] = compiledFunction;
    return name;
  }

  exportAPI("compile_lambda", compile_lambda, { dontInline: true });
  function compile_lambda(name, displayName, lambdaForm, jitFunction = optional) {
    let scope = this;
    if (!isList(lambdaForm) && typeof lambdaForm === 'symbol' ) throw new SchemeEvalError(`Bad lambda ${string(lambda)}`);
    let { code, bindSymToObj } = lambda_compiler.call(this, name, displayName, lambdaForm, jitFunction);
    if (TRACE_COMPILER || TRACE_COMPILER_CODE)
      console.log("COMPILED", name, code, bindSymToObj);
    let binder = new Function("bound", "resolveUnbound", "invokeUnbound", code);
    let compiledFunction = binder.call(this, bindSymToObj, resolveUnbound, invokeUnbound);
    return compiledFunction;
    function resolveUnbound(symbol) {
      let val = scope[symbol];
      if (val === undefined) checkUndefinedInScope(symbol, scope);
      return val;
    }
    function invokeUnbound(fn, args) {
      let list = new Pair(fn, args);
      return _eval(list, scope);
    }
  }

  function lambda_compiler(nameAtom, displayName, lambdaForm, jitFunction) {
    // Prevent a tragic mistake that's easy to make by accident. (Ask me how I know!)
    if (nameAtom === QUOTE_ATOM) throw new SchemeEvalError(`Can't redefine quote ${lambda}`);
    let scope = this;
    let bindSymToObj = {}, guardedSymbols = {}, bindObjToSym = new Map(), functionDescriptors = {};
    let tempNames = {}, varNum = 0, emitted = [], usedSsaValues = {};
    let tools = { emit, bind, use, newTemp, scope, deleteEmitted, indent: '', evalLimit: 100000000,
      bindLiterally, functionDescriptors, compileEval };
    let ssaScope = new Object();
    ssaScope[SCOPE_TYPE_SYMBOL] = "compile-scope";
    // Well-known names
    bindLiterally(string, "string");
    bindLiterally(NIL, "NIL");
    bindLiterally(schemeTrue, "schemeTrue");
    bindLiterally(isList, "isList");
    bindLiterally(Pair, "Pair");
    bindLiterally(Atom, "Atom");
    bindLiterally(newScope, "newScope");
    bindLiterally(FIRST, "FIRST");
    bindLiterally(REST, "REST");
    bindLiterally(LIST, "LIST");
    bindLiterally(ITERATE_AS_LIST, "ITERATE_AS_LIST");
    bindLiterally(MORELIST, "MORELIST");
    bindLiterally(COMPILED, "COMPILED");
    bindLiterally(PARAMETER_DESCRIPTOR, "PARAMETER_DESCRIPTOR");
    bindLiterally(COMPILE_INFO, "COMPILE_INFO");
    bindLiterally(CLOSURE_ATOM, "CLOSURE_ATOM");
    let ssaFunction = compileLambda(nameAtom, displayName, lambdaForm, ssaScope, tools);
    if (jitFunction && Object.getOwnPropertyNames(guardedSymbols).length > 0) {
      let ssaGuardFunction = newTemp(displayName + '_guard');
      emit(`function ${ssaGuardFunction}(...params) {`);
      emit(`  if (false`)
      for (let sym in guardedSymbols) {
        use(sym);
        let ssaAtom = use(bind(guardedSymbols[sym]));
        emit(`      || ${sym} !== scope[${ssaAtom}]`);
      }
      emit(`      ) {`);
      let ssaBoundJittedFn = use(bind(jitFunction));
      let ssaJITCOMPILED = use(bind(JITCOMPILED));
      emit(`    ${ssaBoundJittedFn}[${ssaJITCOMPILED}] = undefined;`);
      emit(`    return ${ssaBoundJittedFn}(...params);`)
      emit(`  }`)
      emit(`  return ${ssaFunction}(...params);`)
      emit(`}`);
      redecorateCompiledClosure(ssaGuardFunction, ssaFunction, emit);
      emit(`return ${ssaGuardFunction};`)
    } else {
      emit(`return ${ssaFunction};`);
    }
    let saveEmitted = emitted;
    emitted = [];
    emit('"use strict";')
    emit(`// params: (bound, resolveUnbound, invokeUnbound)`);
    emit(`let scope = this;`);
    for (let bindingName in bindSymToObj)
      if (usedSsaValues[bindingName])
        emit(`let ${bindingName} = bound[${string(bindingName)}];`);
    emitted = emitted.concat(saveEmitted);
    let code = '';
    for (let emittedLine of emitted)
      if (emittedLine)
        code += emittedLine;
    return { code, bindSymToObj };

    // Binds a JavaScript object into the closure
    function bind(obj, name, guardSym) {
      if (obj === undefined) return "undefined";
      if (obj === null) return "null";
      if (typeof obj === 'number' || typeof obj === 'bigint' || typeof obj === 'string')
        return string(obj);
      if (obj === true) return "true";
      if (obj === false) return "false";
      if (obj === null) return "null";
      let boundSym = bindObjToSym.get(obj);
      if (boundSym) return boundSym;
      if (name) {
        if (typeof name === 'symbol')
          name = newTemp(name.description);
        if (bindSymToObj[name]) { // collision
          name = newTemp(name);
        }
      } else {
        if (typeof obj === 'symbol') {
          name = newTemp(obj.description+'_atom');
        } else {
          name = namedObjects.get(obj);
          if (!name && typeof obj === 'function')
            name = obj.name;
          name = newTemp(name);
        }
      }
      bindSymToObj[name] = obj;
      bindObjToSym.set(obj, name);
      if (guardSym)
        guardedSymbols[name] = guardSym;
      return name;
    }
    function bindLiterally(obj, name) {
      // These symbols are referred to literally in compiled code and must retain the given name,
      // and are never subject to guard checks.
      // Since generated names begin with _ and $ collisions are not possible;
      // nevertheless guard against errors.
      let boundName = use(bind(obj, name));
      if (boundName !== name)
        throw new LogicError(`Unguarded sybols must not be renamed ${string(name)} = ${string(obj)}`);
    }
    function emit(str) {
      emitted.push(tools.indent + str + '\n');
      return emitted.length-1;
    }
    function deleteEmitted(lines) {
      for (let line of lines)
        emitted[line] = undefined;
    }
    function newTemp(name) {
      if (!name)
        name = 'tmp';
      name = toJavaScriptIdentifier(name);
      if (tempNames[name]) {
        for (;;) {
          let nameVariation = `${name}${varNum++}`;
          if (!tempNames[nameVariation]) {
            name = nameVariation;
            break;
          }
        }
      }
      tempNames[name] = true;
      return name;
    }
    // Functions that end up being used as templates are bound because
    // we don't know in advance whether they'll be used as values.
    // "use" is called on any bound value that is ultimately used for its value.
    // This keeps compileed templates from being bound unnecessarily at runtime,
    // so long as their value isn't used for any other reason.
    function use(ssaValue) {
      usedSsaValues[ssaValue] = true;
      return ssaValue;
    }
  }

  //
  // This function parallels "_eval" as closely as possible. If you make a change there
  // you almost certainly have to make a corresponding one here.
  //
  exportAPI("compileEval", compileEval, { dontInline: true });
  function compileEval(form, ssaScope, tools) {
    let emit = tools.emit, use = tools.use, bind = tools.bind, scope = tools.scope, newTemp = tools.newTemp;
    for (;;) { // for macros
      // TODO: This is for debugging, mostly. Should eventually be removed.
      if (--tools.evalLimit < 0)
        throw new SchemeCompileError(`Too complex ${string(form)}`);
      if (form === undefined) return "undefined";
      if (form === null) return "null";
      if (typeof form === 'number' || typeof form === 'bigint' || typeof form === 'string')
        return string(form);
      if (form === true) return "true";
      if (form === false) return "false";
      if (isNil(form)) {
        if (form === NIL) return "NIL";
        return bind(form, "nil");
      }
      if (typeof form === 'function') { // a naked function in the form
        let ssaValue = bind(form);  // no guard sym!
        registerFunctionDescriptor(form, ssaValue);
        return ssaValue;
      }
      if (isPrimitive(form))
        throw new LogicError(`All primitives should be handled by now`)
      if (typeof form === 'symbol') {
        let sym = form;
        let ssaValue = ssaScope[sym];
        if (ssaValue)
          return ssaValue;
        // For now, only bind _functions_ from outside scope
        let scopedVal = scope[sym];
        if (scopedVal && typeof scopedVal === 'function') {
          let fn = scopedVal;
          let guardSym = sym;
          ssaValue = bind(fn, null, guardSym);
          registerFunctionDescriptor(fn, ssaValue);
          return ssaValue;
        }
        tools.dynamicScopeUsed = true;
        return `resolveUnbound(${use(bind(sym))})`;
      }
      if (TRACE_COMPILER)  // too noisy and not very informative to trace the above
        console.log("COMPILE EVAL", string(form));
      if (isList(form) && !isArray(form)) {
        let fn = form[FIRST];
        if (fn === QUOTE_ATOM) {
          if (!isList(form)) throwBadForm();
          let quotedVal = form[REST][FIRST];
          if (isAtom(quotedVal))
            return bind(quotedVal);
          return bind(quotedVal, 'quoted');
        }
        if (fn === LAMBDA_ATOM || fn === SLAMBDA_ATOM)
          return compileLambda(null, fn.description, form, ssaScope, tools);
        let ssaFunction;
        if (typeof fn === 'symbol') {  // check for a macro _without evaluating_
          let symVal = scope[fn];
          if (typeof symVal === 'function') {
            let parameterDescriptor = symVal[PARAMETER_DESCRIPTOR];
            if (parameterDescriptor != null) {
              let tag = parameterDescriptor & 0xff;
              if (tag === MACRO_TAG) {
                let saveMacroCompiled = tools.macroCompiled;
                tools.macroCompiled = false;
                form = symVal.call(scope, form[REST], ssaScope, tools);
                let macroCompiled = tools.macroCompiled;
                tools.macroCompiled = saveMacroCompiled;
                if (!macroCompiled)
                  continue;
                // If the macro flags that it compiled the function; it returns the SSA variable
                // representing the compiled function. I don't expect this to be used much
                // since macros are powerful enough as it is.
                ssaFunction = form;
              }
            }
          }
        }
        if (!ssaFunction)
          ssaFunction = compileEval(fn, ssaScope, tools);
        let args = form[REST];
        let functionDescriptor = tools.functionDescriptors[ssaFunction];
        if (!functionDescriptor) {
          use(ssaFunction);
          let fName = typeof fn === 'symbol' ? fn.description : 'unbound';
          let ssaResult = newTemp(`${fName}_result`);
          let ssaArgList = use(bind(args));
          emit(`let ${ssaResult} = invokeUnbound(${ssaFunction}, ${ssaArgList});`);
          return ssaResult;
        }
        let requiredCount = functionDescriptor.requiredCount;
        let evalCount = functionDescriptor.evalCount;
        let fName = functionDescriptor.name ?? "anon";
        let params = functionDescriptor.params;
        let tag = functionDescriptor.tag;
        let restParam = functionDescriptor.restParam;
        let compileHook = functionDescriptor.compileHook;
        let valueTemplate = functionDescriptor.valueTemplate;
        let bodyTemplate = functionDescriptor.bodyTemplate;
        let requiresScope = functionDescriptor.requiresScope;

        // A compile hook decides for itself whether or not to set ssaScope.dynamicScopeUsed
        if (requiresScope && !compileHook) 
          ssaScope.dynamicScopeUsed = true;

        // Run through the arg list evaluating args
        let ssaArgv = [], ssaArgStr = '', sep = '', argCount = 0, usesDynamicArgv = false;
        while (moreList(args)) {
          let arg = args[FIRST];
          let res = handleParameterMacroIfPresent(arg, args[REST], argCount, evalCount);
          if (res !== undefined) {
            let ssaInsert = res[FIRST];
            args = res[REST];
            if (ssaInsert) {
              usesDynamicArgv = true;
              use(ssaInsert);
              ssaArgStr += `${sep} ...${ssaInsert}`;
              sep = ', ';
            }
            continue;
          }
          let ssaArg;
          if (argCount < evalCount)
            arg = ssaArg  = use(compileEval(arg, ssaScope, tools));
          else if (!compileHook)  // hooks get unbound unevaluated args
            arg = ssaArg = use(bind(arg, `arg_${argCount}`));
          ssaArgv.push(arg);
          argCount += 1;
          if (ssaArg) {
            ssaArgStr += `${sep}${ssaArg}`;
            sep = ', ';
          }
          args = args[REST];
        }
        if (!usesDynamicArgv) {
          // Cases where we simply invoke the function:
          //  - we have at least required number of arguments
          //  - we have no arguments
          // Otherwise, return a closure.
          if (argCount >= requiredCount || argCount === 0) {
            if (compileHook) {
              let ssaResult = compileHook(ssaArgv, ssaScope, tools);
              use(ssaResult);
              return ssaResult;
            }
            if (valueTemplate) {
              let ssaResult = newTemp(fName+'_result');
              emit(`let ${ssaResult}; {`);
              let saveIndent = tools.indent;
              tools.indent += '  ';
              for (let i = 0; i < params.length; ++i) {
                let ssaVal = i < ssaArgv.length ? ssaArgv[i] : 'undefined';
                emit(`let ${params[i]} = ${ssaVal};`);
              }
              if (restParam) {
                let str = '';
                for (let i = params.length; i < ssaArgv.length; ++i)
                  str += `${ssaArgv[i]}, `;
                emit(`let ${restParam} = [${str}];`);
              }
              if (bodyTemplate)
                emit(bodyTemplate); 
              emit(`${ssaResult} = (${valueTemplate});`);
              tools.indent = saveIndent;
              emit(`}`);
              return ssaResult;
            }
            let ssaResult = newTemp(fName+'_result');
            if (TRACE_COMPILER)
              console.log("COMPILE APPLY (eval)", fName, ssaResult, ssaFunction, ...ssaArgv);
            use(ssaFunction);
            if (requiresScope) {
              if (ssaArgStr) ssaArgStr = `, ${ssaArgStr}`;
              emit(`let ${ssaResult} = ${ssaFunction}.call(scope${ssaArgStr});`);
            } else {
              emit(`let ${ssaResult} = ${ssaFunction}(${ssaArgStr});`);
            }
            return ssaResult;
          }
          //
          // Generate closure (see "_eval", I ain't gonna splain it agin)
          //
          if (compileHook) {
            // If there were compile hooks, ssaArgv is a lie after evalCount. Rectify that.
            for (let i = evalCount; i < ssaArgv.length; ++i) {
              ssaArgv[i] = use(bind(ssaArgv[i]));
              ssaArgStr += `, ${ssaArgv[i]}`;
            }
          }
          let name = fName + '_closure';
          let ssaResult = newTemp(name), ssaTmpScope = newTemp("tmp_scope")
          ssaScope = newScope(ssaScope, "compiler-closure-scope");
          emit(`let ${ssaTmpScope} = scope;`);
          emit(`let ${ssaResult}; {`);
          let saveIndent = tools.indent;
          tools.indent += '  ';
          emit(`let scope = newScope(${ssaTmpScope}, "compiled-closure-scope");`);
          // First, cons up the closure S-expr.
          let closureBody;
          let innerParams = NIL, innerForms = NIL;
          requiredCount -= argCount;
          if (requiredCount < 0) throw new LogicError(`Shouldn't happen`);
          if (fn[FIRST] === CLOSURE_ATOM || fn[FIRST] === SCLOSURE_ATOM) {
            closureBody = fn[REST];
            if (fn[FIRST] === SCLOSURE_ATOM) // Skip the evalCount param
              closureBody = closureBody[REST];
            innerParams = closureBody[FIRST];
            innerForms = closureBody[REST];
          } else {
            if (restParam)
              innerParams = Atom(restParam);
            for (let i = params.length; i > 0; --i)
              innerParams = new Pair(Atom(params[i-1]), innerParams);
            innerForms = new Pair(new Pair(fn, innerParams), NIL);
          }
          // Peel off the number of parameters we have arguments for
          let capturedParams = NIL, last;
          for (let i = argCount; i > 0; --i, innerParams = innerParams[REST]) {
            if (!isList(innerParams))
              throw new LogicError(`There should be enough params`);
            let item = new Pair(innerParams[FIRST], NIL);
            if (last) last = last[REST] = item;
            else capturedParams = last = item;
          }
          if (!last) throw new LogicError(`There should be at least one param`);
          closureBody = new Pair(innerParams, innerForms);
          let closureForm = new Pair();
          if (evalCount !== MAX_INTEGER) {
            evalCount -= argCount;
            if (evalCount < 0)
              evalCount = 0;
            closureForm[FIRST] = SCLOSURE_ATOM;
            closureForm[REST] = new Pair("PATCH", new Pair(evalCount, closureBody));
          } else {
            closureForm[FIRST] = CLOSURE_ATOM;
            closureForm[REST] = new Pair("PATCH", closureBody);
          }
          // Now go through the arguments, matching to capturedParams, adding to both the ssaScope
          // and to the scope.
          let closedArgStr = '';
          sep = '';
          for (let i = 0, tmp = capturedParams; i < ssaArgv.length; ++i, tmp = tmp[REST]) {
            let arg = ssaArgv[i];
            if (isList(tmp)) {
              let param = tmp[FIRST];
              let ssaParam = newTemp(param);
              ssaScope[params[i]] = ssaParam;
              let ssaParamName = use(bind((param)));
              emit(`let ${ssaParam} = scope[${ssaParamName}] = ${arg};`)
              closedArgStr += `${sep}${ssaParam}`;
              sep = ', ';
            }
          }
          let paramStr = '';
          let ssaParamv = [], ssaRestParam;
          sep = '';
          for (; moreList(innerParams); innerParams = innerParams[REST]) {
            let param = innerParams[FIRST];
            let ssaParam = newTemp(param);
            ssaParamv.push(ssaParam);
            paramStr += sep + ssaParam;
            sep = ', ';
          }
          if (typeof innerParams === 'symbol') {
            let ssaRest = newTemp(innerParams);
            paramStr += `${sep}...${ssaRest}`;
            ssaRestParam = ssaRest;
          }
          use(ssaFunction);
          ssaScope.dynamicScopeUsed = true;
          emit(`${ssaResult} = (${paramStr}) => ${ssaFunction}.call(scope, ${closedArgStr}, ${paramStr});`);
          let displayName = `(${paramStr}) => ${ssaFunction}.call(scope${closedArgStr}, ${paramStr})`;
          // closures do not need a scope!
          let fnInfo = { requiredCount, evalCount, params: ssaParamv, restParam: ssaRestParam, requiresScope: false };
          decorateCompiledClosure(ssaResult, displayName, closureForm, fnInfo, tools);
          tools.indent = saveIndent;
          emit(`}`);
          tools.functionDescriptors[ssaResult] = { requiredCount, evalCount, name, noScope: true };
          return ssaResult;
        }
        // We have a dynamic number of arguments, all evaluated.
        // But we don't know how many at compile time except that there's a minimum
        let ssaResult = newTemp(`${fName}_result`);
        use(ssaFunction);
        let ssaDynamicArgv = newTemp('dynamic_argv');
        if (argCount >= requiredCount) {
          emit(`let ${ssaResult} = ${ssaFunction}.apply(scope, ${ssaArgStr});`);
          return ssaResult;
        }
        // Here, we have to determine at runtime time whether a closure is required
        emit(`let ${ssaResult};`);
        emit(`if (${ssaDynamicArgv}.length >= ${requiredCount})`);
        emit(`  ${ssaResult} = ${ssaFunction}.apply(scope, ${ssaArgStr});`);
        emit(`else`);
        if (requiresScope)
          emit (`  ${ssaResult} = function vaBound(...args) { return ${ssaFunction}.call(scope, ${ssaArgStr}, ...args);  } `);
        else
          emit (`  ${ssaResult} = function vaBound(...args) { return ${ssaFunction}(...${ssaArgStr}, ...args);  } `);
        return ssaRessult;
      }
      // Special eval for JS Arrays and Objects
      if (form !== null && typeof form === 'object') {
        if (isArray(form )) {
          let evalledSsaValues = [];
          while (moreList(form)) {
            let element = form[FIRST];
            let res = handleParameterMacroIfPresent(element, form[REST], 0, MAX_INTEGER);
            if (res !== undefined) {
              let ssaInsert = res[FIRST];
              form = res[REST];
              if (ssaInsert) {
                use(ssaInsert);
                evalledSsaValues.push(`...${ssaInsert}`);
              }
              continue;
            }
            let ssaValue = compileEval(element, ssaScope, tools);
            use(ssaValue);
            evalledSsaValues.push(ssaValue);
            form = form[REST];
          }
          let ssaArrayLiteral = newTemp("arrayliteral");
          emit(`let ${ssaArrayLiteral} = [`);
          for (let ssaElement of evalledSsaValues) {
            emit(`  ${ssaElement},`);
          }
          emit(`];`);
          return ssaArrayLiteral;
        }
        let ssaObjectLiteral = newTemp("objectliteral");
        emit(`let ${ssaObjectLiteral} = {};`);
        for (let key of [ ...Object.getOwnPropertyNames(form), ...Object.getOwnPropertySymbols(form) ]) {
          let value = form[key];
          let ssaInsertObj = handleParameterMacroIfPresentInObjectLiteral(key, value);
          if (ssaInsertObj) {
            emit(`Object.assign( ${ssaObjectLiteral}, (${ssaInsertObj});`)
            continue;
          }
          let ssaKey;
          if (key === EVALUATE_KEY_VALUE_SYMBOL) {
            ssaKey = compileEval(value[0], ssaScope, tools);
            value = value[1];
          } else {
            ssaKey = bind(key)
          }
          let ssaValue = compileEval(value, ssaScope, tools);
          use(ssaKey);
          use(ssaValue);
          emit(`${ssaObjectLiteral}[${ssaKey}] = ${ssaValue};`);
        }
        return ssaObjectLiteral;
      }
      throw new LogicError(`Shouldn't happen. All cases should be handled above`);
    } // macro loop

    function registerFunctionDescriptor(fn, ssaValue) {
      if (!tools.functionDescriptors[ssaValue]) {
        let fnInfo = fn[COMPILE_INFO];
        if (!fnInfo) {  // Neither a builtin nor a lambdaClosure
          let parameterDescriptor = fn[PARAMETER_DESCRIPTOR] ?? examineFunctionForParameterDescriptor(fn);
          let requiredCount = (parameterDescriptor >> 8) & 0xfff;
          let evalCount = parameterDescriptor >> 19 >>> 1;  // restores MAX_INTEGER to MAX_INTEGER
          let tag = parameterDescriptor & 0xff;
          fnInfo = analyzeJSFunction(fn);
          fnInfo.requiredCount = requiredCount;
          fnInfo.evalCount = evalCount;
          fnInfo.valueTemplate = fnInfo.bodyTemplate = undefined;
        }
        // Everything you need to know about invoking a JS function is right here
        tools.functionDescriptors[ssaValue] = fnInfo;
      }
    }

    function throwBadForm() {
      throw new SchemeCompileError(`BadForm ${string(form)}`);
    }

    function handleParameterMacroIfPresent(arg, args, argCount, evalCount) {  
      if (typeof arg === 'symbol') {
        let symVal = scope[arg];
        if (typeof symVal === 'function') {
          let parameterDescriptor = symVal[PARAMETER_DESCRIPTOR];
          if (parameterDescriptor != null) {
            let tag = parameterDescriptor & 0xff;
            if (tag === PARAMETER_MACRO_TAG || (argCount < evalCount && tag === EVALUATED_PARAMETER_MACRO_TAG)) {
              let saveMacroCompiled = tools.macroCompiled;
              tools.macroCompiled = false;
              let macroResult = symVal.call(scope, args, ssaScope, tools);
              let macroCompiled = tools.macroCompiled;
              tools.macroCompiled = saveMacroCompiled;
              if (!isList(macroResult))
                throw new SchemeCompileError(`bad parameter macro result ${string(macroResult)}`);
              let insert = macroResult[FIRST], ssaInsertValues;
              let nextArg = macroResult[REST];
              if (macroCompiled) {
                ssaInsertValues = insert;
              } else {
                if (tag === PARAMETER_MACRO_TAG)
                  return new Pair(undefined, insert);
                if (insert && insert.length > 0) {
                  let ssaInsert = use(bind(insert));
                  ssaInsertValues = newTemp("macro_insert");
                  emit(`let ${ssaInsertValues} = Array(${insert.length});`);
                  emit(`for (let i = 0; i < ${insert.length}; ++i) {`);
                  emit(`  let arg = ${ssaInsert}[i];`);
                  if (argCount < evalCount && evalCount !== MAX_INTEGER) {
                    ssaScope.dynamicScopeUsed = true;
                    tools.bindLiterally(_eval, "_eval");
                    emit(`if (i < ${evalCount-argCount}) arg = _eval(arg, scope);`);
                  } else if (argCount >= evalCount || evalCount === MAX_INTEGER) {
                    ssaScope.dynamicScopeUsed = true;
                    tools.bindLiterally(_eval, "_eval");
                    emit(`arg = _eval(arg, scope);`);
                  }
                  emit(`  ${ssaInsertValues}[i] = arg;`);
                }
              }
              return new Pair(ssaInsertValues, nextArg);
            }
          }
        }
      }
    }

    function handleParameterMacroIfPresentInObjectLiteral(key, value) {
      if (typeof key === 'symbol') {
        let symVal = scope[key];
        if (typeof symVal === 'function') {
          let parameterDescriptor = symVal[PARAMETER_DESCRIPTOR];
          if (parameterDescriptor != null) {
            let tag = parameterDescriptor & 0xff;
            if (tag === EVALUATED_PARAMETER_MACRO_TAG) {
              let saveMacroCompiled = tools.macroCompiled;
              tools.macroCompiled = false;
              let macroResult = symVal.call(scope, new Pair(value, NIL), ssaScope, tools);
              let macroCompiled = tools.macroCompiled;
              tools.macroCompiled = saveMacroCompiled;
              if (!isList(macroResult))
                throw new SchemeCompileError(`bad parameter macro result ${string(macroResult)}`);
              let insert = macroResult[FIRST], ssaInsertObj;
              // macroResult[REST] isn't used in this case
              if (macroCompiled) {
                ssaInsertObj = insert;
              } else {
                let ssaInsert = use(bind(insert));
                ssaScope.dynamicScopeUsed = true;
                ssaInsertObj = newTemp("macro_insert");
                tools.bindLiterally(_eval, "_eval");
                emit(`let ${ssaInsertObj} = _eval(${ssaInsert}, scope);`)
              }
              return ssaInsert;
            }
          }
        }
      }
      return undefined
    }

  }

  //
  // This function parallels makeLambdaClosure as closely as possible. If you make a change
  // there, you almost certainly have to make a corresponding change here.
  //
  function compileLambda(nameAtom, displayName, lambda, ssaScope, tools) {
    let emit = tools.emit, use = tools.use, bind = tools.bind, scope = tools.scope, newTemp = tools.newTemp;
    let saveUsesDynamicScope = tools.usesDymanicScope;
    tools.requiresScope = false;
    let saveScopeLines = tools.scopeLines, scopeLines = [];
    tools.scopeLines = scopeLines;
    if (!isList(lambda)) throwBadCompiledLambda(lambda);
    let body = lambda[REST];
    let evalCount = MAX_INTEGER;
    if (lambda[FIRST] === SLAMBDA_ATOM) {
      if (!isList(lambda)) throwBadCompiledLambda(lambda);
      evalCount = body[FIRST];
      if (typeof evalCount !== 'number') throwBadCompiledLambda(lambda);
      body = body[REST];
    }
    if (!displayName) displayName = nameAtom ? nameAtom.description : evalCount === MAX_INTEGER ? `lambda` : `slambda#${evalCount}`;
    let ssaFunction = newTemp(displayName);
    if (!isList(body)) throwBadCompiledLambda(lambda);
    let params = body[FIRST];
    let forms = body[REST];
    let ssaParamv = [], ssaRestParam, paramv = [], restParam;
    let originalSsaScope = ssaScope;
    ssaScope = newScope(ssaScope, "compiler-lambda-scope");
    let paramCount = 0, requiredCount, optionalFormsVec = [];
    for (; moreList(params); ++paramCount, params = params[REST]) {
      let param = params[FIRST], ssaParam;
      if (isList(param)) {
        if (!isAtom(param[FIRST]))
          throwBadCompiledLambda(lambda, `what's this? ${string(param)}`);
        optionalFormsVec.push(param[REST]);
        param = param[FIRST];
        ssaParam = newTemp(param);
        if (requiredCount === undefined)
          requiredCount = paramCount;
      } else if (!isAtom(param)) {
        throwBadLambda(lambda, `parameter ${string(param)} not an atom`);
      } else {
        if (param === REST_PARAM_ATOM) {
          if (!isList(params[REST]) || !isNil(params[REST][REST]))
            throwBadLambda(lambda, `bad "rest" parameter ${string(param)}`);
          params = params[REST];
          restParam = params[FIRST];
          params = params[REST];
          ssaRestParam = newTemp(restParam);
          ssaScope[restParam] = ssaRestParam;
          break;
        }
        ssaParam = newTemp(param);
        optionalFormsVec.push(undefined);
      }
      ssaParamv.push(ssaParam);
      ssaScope[param] = ssaParam;
      paramv.push(param);
    }
    if (!isNil(params))
      throw new throwBadCompiledLambda(lambda,`bad parameter list ${string(params)}`);
    if (requiredCount === undefined)
      requiredCount = paramCount;  // paramCount does NOT contain rest param
    tools.functionDescriptors[ssaFunction] = { requiredCount, evalCount, name: displayName, noScope: true };
    if (nameAtom)
      ssaScope[nameAtom] = ssaFunction;
    let delim = '', ssaParamStr = '';
    for (let param of ssaParamv) {
      ssaParamStr += delim + param;
      delim = ', ';
    }
    if (ssaRestParam)
      ssaParamStr += `${delim}...${ssaRestParam}`;
    let ssaScopeTmp = newTemp("tmp_scope");
    scopeLines.push(emit(`let ${ssaScopeTmp} = scope;`));
    emit(`function ${ssaFunction}(${ssaParamStr}) { // COMPILED ${displayName}, req: ${requiredCount}, eval: ${evalCount === MAX_INTEGER ? '*' : evalCount}`);
    let saveIndent = tools.indent;
    tools.indent += '  ';
    let lambdaStrs = string(lambda).split('\n');
    for (let str of lambdaStrs) emit(`// ${str}`);
    scopeLines.push(emit(`let scope = newScope(${ssaScopeTmp}, "compiled-lambda-scope");`));
    for (let i = 0; i < paramCount; ++i) {
      let ssaParam = ssaParamv[i];
      let optionalForms = optionalFormsVec[i];
      if (optionalForms !== undefined) {
        emit(`if (${ssaParam} === undefined) {`);
        let saveIndent = tools.indent;
        tools.indent += '  ';
        let ssaVal = `${BOTTOM}`;
        for (let form of optionalForms)
          ssaVal = compileEval(form, ssaScope, tools);
        emit(`${ssaParam} = ${ssaVal};`);
        tools.indent = saveIndent;
        emit('}');
      }
      let ssaParamName = use(bind(paramv[i]));
      scopeLines.push(emit(`scope[${ssaParamName}] = ${ssaParam};`));
    }
    if (restParam) {
      let ssaParamName = use(bind(restParam));
      scopeLines.push(emit(`scope[${ssaParamName}] = ${ssaRestParam};`));
    }
    let ssaResult = `${BOTTOM}`;
    for ( ; moreList(forms); forms = forms[REST])
      ssaResult = compileEval(forms[FIRST], ssaScope, tools);
    if (ssaScope.dynamicScopeUsed)
      originalSsaScope.dynamicScopeUsed = true;
    if (!ssaScope.dynamicScopeUsed)
      tools.deleteEmitted(scopeLines);
    use(ssaResult);
    emit(`return ${ssaResult};`);
    tools.indent = saveIndent;
    emit(`}`);
    let closureAtom = CLOSURE_ATOM;
    if (evalCount !== MAX_INTEGER) {
      closureAtom = SCLOSURE_ATOM;
      body = new Pair(evalCount, body);
    }
    let closureForm = new Pair(closureAtom, new Pair("PATCH", body));
    // NOTE: The compiler is misssing an opportunity her that I want to return to
    // later. If the compiled lambda (and in compileEval, closure)
    // references unbound parameters (requiresScope here), it should
    // *also* set a compileHook in foNofo so that if the compiler sees the
    // function (or closure) again, it will try to compile it again
    // in a new scope which coul potentially satisfy those references).
    // It's trivial to write but hard to test so I'm saiving it for a
    // rainy day. More important things to do right now.
    // OR, perhaps better, the compiler should just recompile any
    // closures it sees, having the potential there to materialize the closed
    // values in the downstream compiled code.
    let fnInfo = { requiresScope: tools.usesDymanicScope, requiredCount, evalCount, params: ssaParamv, restParam: ssaRestParam };
    decorateCompiledClosure(ssaFunction, displayName, closureForm, fnInfo, tools);
    tools.usesDymanicScope = saveUsesDynamicScope;
    tools.scopeLines = saveScopeLines;
    return ssaFunction;
  }

  function throwBadCompiledLambda(lambda, msg) { throw new SchemeCompileError(`Bad lambda ${lambda}` + (msg ? `, ${msg}` : '')) }
  
  function decorateCompiledClosure(ssaClosure, displayName, closureForm, fnInfo, tools) {
    let emit = tools.emit, use = tools.use, bind = tools.bind;
    let requiredCount = fnInfo.requiredCount, evalCount = fnInfo.evalCount;
    let ssaClosureForm = use(bind(closureForm, "closureForm"));
    let parameterDescriptor = makeParameterDescriptor(requiredCount, evalCount);
    let evalCountStr = evalCount === MAX_INTEGER ? "MAX_INTEGER" : String(evalCount);
    emit(`// evalCount: ${evalCountStr}, requiredCount: ${requiredCount}`)
    emit(`${ssaClosure}[PARAMETER_DESCRIPTOR] = ${parameterDescriptor};`);
    emit(`${ssaClosure}[COMPILE_INFO] = ${use(bind(fnInfo, "COMPILE_INFO"))};`);
    // The function is simultaneously a Scheme closure object
    let closureStr = string(closureForm);
    for (let str of closureStr.split('\n'))
      emit(`// ${str}`);
    emit(`${ssaClosure}[FIRST] = ${ssaClosureForm}[FIRST];`);
    emit(`${ssaClosure}[REST] = new Pair(scope, ${ssaClosureForm}[REST][REST]);`);
    emit(`${ssaClosure}[COMPILED] = ${string(displayName)}`);
    // Mark object as a list, a pair, and a closure.
    emit(`${ssaClosure}[LIST] = ${ssaClosure}[ITERATE_AS_LIST] = ${ssaClosure}[MORELIST] = ${ssaClosure}[CLOSURE_ATOM] = true;`);
  }

  function redecorateCompiledClosure(ssaToFn, ssaFromFn, emit) {
    emit(`${ssaToFn}[COMPILE_INFO] = ${ssaFromFn}[COMPILE_INFO];`);
    emit(`${ssaToFn}[PARAMETER_DESCRIPTOR] = ${ssaFromFn}[PARAMETER_DESCRIPTOR];`);
    emit(`${ssaToFn}[FIRST] = ${ssaFromFn}[FIRST];`);
    emit(`${ssaToFn}[REST] = ${ssaFromFn}[REST];`);
    emit(`${ssaToFn}[COMPILED] = ${ssaFromFn}[COMPILED];`);
    // Mark object as a list, a pair, and a closure.
    emit(`${ssaToFn}[LIST] = ${ssaToFn}[ITERATE_AS_LIST] = ${ssaToFn}[MORELIST]  = ${ssaToFn}[CLOSURE_ATOM] = true;`);
  }
  
  const JS_IDENT_REPLACEMENTS = {
    '~': '$tilde', '!': '$bang', '@': '$at', '#': '$hash', '$': '$cash', '%': '$pct', '^': '$hat',
    '&': '$and', '|': '$or', '*': '$star', '+': '$plus', '-': '$dash', '=': '$eq', '<': '$lt',
    '>': '$gt', '/': '$stroke', '\\': '$bs', '?': '$q', MUL: '$mul', DIV: '$div', ELIPSIS: "$elipsis",
  };

  //
  // Preserves "normal" JavaScript names as much as possible; they generally just get an _ prefix.
  // Lisp symbols with special characters get translated using $symid from the replacement
  // table above or $x<codepoint> otherwise. So any unicode name is represented.
  //
  exportAPI("toJavaScriptIdentifier", toJavaScriptIdentifier);
  function toJavaScriptIdentifier(name) {
    if (typeof name === 'symbol')
      name = name.description;
    let newName = "", fragment = "";
    for (let ch of name) {
      if (JS_IDENT_REPLACEMENTS[ch]) {
        newName += `${fragment}${JS_IDENT_REPLACEMENTS[ch]}`;
        fragment = "";
      } else if (JSIDENT2[ch]) {
        if (!fragment) fragment = "_";
        fragment += ch;
      } else {
        newName += `${fragment}$x${ch.codePointAt(0).toString(16)}`;
        fragment = "";
      }
    }
    newName += fragment;
    return newName;
  }

  exportAPI("_setGlobalScope_test_hook_", _setGlobalScope_test_hook_)
  function _setGlobalScope_test_hook_(scope) {
    let previousGlobalScope = globalScope;
    globalScope = scope;
    globalScope.globalScope = globalScope;
    globalScope[Atom('globalScope')] = globalScope;
    return previousGlobalScope;
  }

  return globalScope;
} // Whew!