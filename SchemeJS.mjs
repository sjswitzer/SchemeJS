//
// SchemeJS: Scheme in JavaScript
//
// Copyright 2021 Stan Switzer
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

export const VERSION = "1.1";

//
// Creates a SchemeJS instance, independent of any others.
// Instances are distinct to the bones; they do not even recognize each other's
// Cons cells or NIL values. This is by design. People should be able
// to poke things into class definitions to experiment with different ideas
// but that should only affect that specific SchemeJS instance; others should
// be unaffected.
//
export function createInstance(schemeOpts = {}) {
  let readFile = schemeOpts.readFile;
  let _reportError = schemeOpts.reportError = error => console.log(error); // Don't call this one
  let reportSchemeError = schemeOpts.reportSchemeError ?? _reportError; // Call these instead
  let reportSystemError = schemeOpts.reportError ?? _reportError;
  let reportLoadResult = schemeOpts.reportLoadResult ?? (result => console.log(string(result)));
  let lambdaStr = schemeOpts.lambdaStr ?? "\\";
  let slambdaStr = schemeOpts.lsambdaStr ?? "\\\\";

  // Creating a Cons should be as cheap as possible, so no subclassing
  // or calls to super. But I want people to be able to define their
  // own specialized or tricky Cons cells.
  // This means that identifying the "class" of Cons
  // cells can't use "instanceof AbstractCons" or whatever.
  //
  // Instead, the method is:
  //    obj != null && obj[PAIR] === true
  //
  // Fetching properties is something JITs really optimize.
  // It's probably as fast as or faster than "instanceof".
  //
  // Beware when traversing lists. Things purported to be lists might not be
  // and although lists are conventionally NIL-terminated, the final "cdr"
  // could be anything at all.

  const CAR = Symbol("CAR"), CDR = Symbol("CDR"), PAIR = Symbol("PAIR"), LAZY = Symbol("LAZY");

  // Trust the JIT to inline this
  const is_cons = obj => obj != null && obj[PAIR] === true;

  class Cons {
    [CAR]; [CDR];
    constructor(car, cdr) {
      this[CAR] = car;
      this[CDR] = cdr;
    }
    toString() {
      return string(this, { maxDepth: 4 });
    }
    [Symbol.iterator]() {
      let current = this;
      return {
        next() {
          if (!is_cons(current)) return { done: true };
          let value = current[CAR];
          current = current[CDR];
          return { done: false, value };
        }
      }
    }
    // static [PAIR] = true;  // Hmm; Shouldn't this work?
  }
  Cons.prototype[PAIR] = true;
  
  // Hide the NIL class because there's never any reason to
  // reference it or to instantiate it it more than once. Leaving it visible
  // just invites errors. But it's good to have a distinct class for NIL
  // for various reasons including that it looks better in a JS debugger
  // and provides a way to trap attempts to get or set [CAR] and [CDR].
  const NIL = new ((_ => {
    return class NIL {
      [Symbol.iterator]() { return { next: () => { done: true } } }
      get [CAR]() { throw new EvalError("car of nil") }
      set [CAR](_) { throw new EvalError("set car of nil") }
      get [CDR]() { throw new EvalError("cdr of nil") }
      set [CDR](_) { throw new EvalError("set cdr of nil") }
    }
  })());
  
  // Create a new scope with newScope(oldScope, "description").
  // A new scope's prototype is the enclosing scope.
  // This way, a scope chain is a prototype chain and resolving
  // a symbol is as simple as "scope[sym]"!
  const SCOPE_IS_SYMBOL = Symbol("SCOPE_IS");
  class Scope {
    // Be careful defining methods or properties here; they
    // automatically become part of the API.
  };
  Scope.prototype[SCOPE_IS_SYMBOL] = "global-scope";

  let globalScope = new Scope();

  exportAPI("newScope", newScope);
  function newScope(enclosingScope, scope_is) {
    let scope = Object.create(enclosingScope);
    scope[SCOPE_IS_SYMBOL] = scope_is;
    return scope;
  }

  //
  // Atoms are Symbols that are in the ATOMS object
  //
  const ATOMS = {};

  const is_atom = obj => typeof obj === 'symbol' && ATOMS[obj.description] === obj;

  function Atom(name) {
    // If they pass in an atom, just return it
    if (is_atom(name)) return name;
    if (typeof name !== 'string') throw new EvalError(`Not a string ${name}`);
    let atom = ATOMS[name];
    if (atom !== undefined) return atom;
    atom = Symbol(name);
    ATOMS[name] = atom;
    return atom;
  }

  const LAMBDA_ATOM = Atom("lambda"), LAMBDA_CHAR = "\u03BB";
  ATOMS["\\"] = ATOMS[LAMBDA_CHAR] = LAMBDA_ATOM;  // Some aliases for Lambda
  const SLAMBDA_ATOM = Atom("special-lambda");
  ATOMS["\\\\"] = SLAMBDA_ATOM;
  const CLOSURE_ATOM = Atom("%%closure");
  const SCLOSURE_ATOM = Atom("%%%closure");

  const is_iterable = obj => obj != null && typeof obj[Symbol.iterator] === 'function';

  // Character clases for parsing
  const TOKS = {}, DIGITS = {}, IDENT1 = {}, IDENT2 = {},
  NUM1 = {}, NUM2 = {}, OPERATORS = {}, WS = {}, NL = {}, WSNL = {}, JSIDENT = {};
  for (let ch of `()[]{},':`) TOKS[ch] = true;
  for (let ch of ` \t`) WS[ch] = WSNL[ch] = true;
  for (let ch of `\n\r`) NL[ch] = WSNL[ch] = true;
  for (let ch of `abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$`)
    IDENT1[ch] = IDENT2[ch] = JSIDENT[ch] = true;
  for (let ch of `0123456789`)
    DIGITS[ch] = IDENT2[ch] = NUM1[ch] = NUM2[ch] = JSIDENT[ch] = true;
  for (let ch of `+-.`)
    NUM1[ch] = NUM2[ch] = true;
  for (let ch of `eEoOxXbBn`)
    NUM2[ch] = true;
  for (let ch of `~!@#$%^&|*_-+-=|\\<>?/`)
    OPERATORS[ch] = IDENT1[ch] = IDENT2[ch] = true;

  //
  // Everything is encapsulated in a function scope because JITs
  // should be able to resolve lexically-scoped references most easily.
  // Since "this" is used as the current scope, a SchemeJS instance
  // is the global scope itself!
  //
  function exportAPI(name, value, ...aliases) {
    globalScope[name] = value;
    for (let alias in aliases)
      globalScope[alias] = value;
  }

  //
  // Unlike exportAPI, which exports an API to clients, defineGlobalSymbol
  // defines a symbol for the SchemeJS environment AND exports it as an API.
  // Be careful which one you use!
  //
  // Some of the short exported functions are all on one line. This is intentional.
  // Those function's bodies are included in the string representation used for dispay.
  // 
  const FUNCTION_DESCRIPTOR_SYMBOL = Symbol("*schemeJS-function-descriptor*");
  const COMPILE_HOOK = Symbol("*schemeJS-compile-hook*");
  const MAX_INTEGER = 2**31-1;  // Presumably allows JIT to do small-int optimizations
  const analyzedFunctions = new Map();

  exportAPI("defineGlobalSymbol", defineGlobalSymbol);
  function defineGlobalSymbol(name, value, ...aliases) {
    let opts = {};
    if (typeof aliases[0] === 'object')
      opts = aliases.shift();
    if (typeof value === 'function') {
      let evalCount = opts.evalArgs ?? MAX_INTEGER;
      let fnInfo = analyzeJSFunction(value);
      let paramCount = fnInfo.params.length;
      // If this function doesn't evaluate all of its parameters, the last parameter
      // recieves the unevaluated forms, so don't count that as a normal one.
      if (evalCount !== MAX_INTEGER)
        paramCount -= 1;
      if (paramCount >= 0xff) throw new LogicError("Too many params");
      if (fnInfo.native) paramCount = 0;
      // Encoding chosen so that small values mean eval everything and lift that many.
      let functionDescriptor = (~evalCount << 8) | paramCount&0xff;
      value[FUNCTION_DESCRIPTOR_SYMBOL] = functionDescriptor;
      // XXX TODO: make sure that every defn with evalCount !== MAX_INTEGER has a compile hook.
      if (opts.compileHook) value[COMPILE_HOOK] = opts.compileHook;
    }
    let atom;
    ({ atom, name } = normalize(name));
    globalScope[atom] = value;
    if (!opts.schemeOnly)
      globalScope[name] = value;
    for (let alias of aliases) {
      ({ atom, name } = normalize(alias));
        globalScope[atom] = value;
      if (!opts.schemeOnly)
        globalScope[name] = value;
    }
    return atom;

    function normalize(name) {
      if (typeof name === 'symbol')
        name = name.description;
      let atom = Atom(name);
      name = name.replace("->", "_to_");
      name = name.replace("-", "_");
      name = name.replace("@", "_at_");
      name = name.replace("*", "_star_");
      name = name.replace("?", "P");
      return { atom, name };
    }
  }

  defineGlobalSymbol("SchemeJS-version", VERSION);
  defineGlobalSymbol("is-atom", is_atom, "atom?");
  defineGlobalSymbol("Atom", Atom);

  class SchemeJSError extends Error {};
  SchemeJSError.prototype.name = "SchemeJSError";
  defineGlobalSymbol("SchemeJSError", SchemeJSError);

  class EvalError extends SchemeJSError {};
  EvalError.prototype.name = "EvalError";
  defineGlobalSymbol("EvalError", EvalError);

  class CompileError extends SchemeJSError {};
  CompileError.prototype.name = "CompileError";
  defineGlobalSymbol("CompileError", CompileError);

  class ParseError extends SchemeJSError {};
  ParseError.prototype.name = "ParseError";
  defineGlobalSymbol("ParseError", ParseError);

  class ParseExtraTokens extends ParseError {};
  ParseExtraTokens.prototype.name = "ParseExtraTokens";
  defineGlobalSymbol("ParseExtraTokens", ParseExtraTokens);

  class ParseIncomplete extends ParseError {};
  ParseIncomplete.prototype.name = "ParseIncomplete";
  defineGlobalSymbol("ParseIncomplete", ParseIncomplete);

  const LogicError = Error;
  defineGlobalSymbol("LogicError", LogicError);

  class EvaluateKeyValue {
    key; value;
    constructor(key, value) {
      this.key = key;
      this.value = value;
    }
  }

  //
  // SchemeJS strives to maintain JavaScript consistency wherever possibe but enough is enough.
  // In SchemeJS, NIL, null, undefined, and false are false and everything else is true.
  //
  exportAPI("bool", bool);
  function bool(val) {
    // Give priority to actual true and false values
    if (typeof val === 'boolean') return val;
    if (val === NIL || val == null) // The val == null is _intended_ as a nullish test
      return false;
    return true;
  }

  const cons = (car, cdr) => new Cons(car, cdr);
  const car = a => a[CAR];
  const cdr = a => a[CDR];
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

  const quote = quoted => quoted[CAR];
  const QUOTE_ATOM = defineGlobalSymbol("quote", quote, { evalArgs: 0 }, "'");
  defineGlobalSymbol("scope", function() { return this });

  exportAPI("NIL", NIL);
  defineGlobalSymbol("nil", NIL);
  defineGlobalSymbol("null", null);
  defineGlobalSymbol("true", true);
  defineGlobalSymbol("false", false);
  defineGlobalSymbol("is-cons", is_cons, "pair?");
  defineGlobalSymbol("cons", cons);
  defineGlobalSymbol("car", car, "first");
  defineGlobalSymbol("cdr", cdr, "rest");
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
  defineGlobalSymbol("typeof", a => typeof a);
  defineGlobalSymbol("undefined?", a => a === undefined);
  defineGlobalSymbol("null?", a => a === NIL);  // SIOD clained it first. Maybe rethink the naming here.
  defineGlobalSymbol("jsnull?", a => a === null);
  defineGlobalSymbol("nullish?", a => a == null);
  defineGlobalSymbol("boolean?", a => typeof a === 'boolean');
  defineGlobalSymbol("number?", a => typeof a === 'number');
  defineGlobalSymbol("bigint?", a => typeof a === 'bigint');
  defineGlobalSymbol("numeric?", a => typeof a === 'number' || typeof a === 'bigint');
  defineGlobalSymbol("string?", a => typeof a === 'string');
  defineGlobalSymbol("symbol?", a => typeof a === 'symbol');
  defineGlobalSymbol("function?", a => typeof a === 'function');
  defineGlobalSymbol("object?", a => typeof a === 'object');
  defineGlobalSymbol("array?", a => Array.isArray(a));
  defineGlobalSymbol("NaN", NaN, { schemeOnly: true });
  defineGlobalSymbol("isNaN", isNaN, { schemeOnly: true } , "NaN?", "nan?");
  defineGlobalSymbol("Infinity", Infinity, { schemeOnly: true });
  defineGlobalSymbol("is_finite", isFinite, { schemeOnly: true } , "finite?");
  defineGlobalSymbol("globalThis", globalThis);
  defineGlobalSymbol("Math", Math, { schemeOnly: true });
  defineGlobalSymbol("Atomics", Atomics, { schemeOnly: true });
  defineGlobalSymbol("JSON", JSON, { schemeOnly: true });
  defineGlobalSymbol("Reflect", Reflect, { schemeOnly: true } );
  defineGlobalSymbol("Intl", Intl, { schemeOnly: true } );
  for (let fn of [
      Object, Boolean, Symbol, Number, String, BigInt, Array,
      encodeURI, encodeURIComponent, decodeURI, decodeURIComponent,
      Error, EvalError, RangeError, ReferenceError,
      SyntaxError, TypeError, URIError,
      Date, RegExp, parseFloat, parseInt,
      Map, Set, WeakMap, WeakSet,
      Int8Array, Uint8Array, Uint8ClampedArray,
      Int16Array, Uint16Array, Int32Array, Uint32Array, Float32Array,
      Float64Array, BigInt64Array, BigUint64Array,
      ArrayBuffer, SharedArrayBuffer, DataView,
      Function, Promise, Proxy
    ]) {
    defineGlobalSymbol(fn.name, fn, { schemeOnly: true });
  }
  if (typeof XMLHttpRequest !== 'undefined')
    defineGlobalSymbol("XMLHttpRequest", XMLHttpRequest, { schemeOnly: true });
  if (typeof navigator !== 'undefined')
    defineGlobalSymbol("navigator", navigator, { schemeOnly: true });
  if (typeof window !== 'undefined')
    defineGlobalSymbol("window", window, { schemeOnly: true });

  // Stuff the whole Math class in there!
  for (let [name, {value}] of Object.entries(Object.getOwnPropertyDescriptors(Math))) {
    // SIOD defines *pi* so I'll just define them all like that
    if (typeof value === 'number')
      name = `*${name.toLowerCase()}*`;
    // SIOD defines sin, cos, asin, etc. so I'll just define them all like that
    if (typeof value === 'function')
      defineGlobalSymbol(name, value, { schemeOnly: true });
  }
  defineGlobalSymbol("abs", a => a < 0 ? -a : a);  // Overwrite Math.abs; this deals with BigInt too

  defineGlobalSymbol("intern", a => Atom(a));
  defineGlobalSymbol("Symbol", a => Symbol(a));  // XXX name?
  defineGlobalSymbol("Number", a => Number(a));
  defineGlobalSymbol("BigInt", a => BigInt(a));

  defineGlobalSymbol("eval", eval_);
  function eval_(expr, scope) { // Javascript practically treats "eval" as a keyword
    if (scope == null) scope = this;  // Default is the current scope
    else if (scope === NIL) scope = globalScope; // NIL, specifically, means use the global scope
    return _eval(expr, scope);
  }

  defineGlobalSymbol("eval-string", evalString, "evalString");
  function evalString(str, scope) {
    let expr = parseSExpr(str);
    return eval_.call(this, expr, scope);
  }

  defineGlobalSymbol("globalScope", globalScope);

  defineGlobalSymbol("apply", apply);
  function apply(fn, args, ...rest) {
    let scope = this;
    if (rest.length > 0)
      scope = rest[0];
    if (!(scope instanceof Scope)) scope = this;
    return _apply(fn, args, scope);
  }

  // Pokemon gotta catch 'em' all!
  defineGlobalSymbol("!", a => !bool(a), "not");
  defineGlobalSymbol("~", a => ~a, "bit-not");
  defineGlobalSymbol("**", (a,b) => a ** b, "exp");
  defineGlobalSymbol("%", (a,b) => a % b, "rem");
  defineGlobalSymbol("<<", (a,b) => a << b, "bit-shl");
  defineGlobalSymbol(">>", (a,b) => a >> b, "bit-shr");
  defineGlobalSymbol(">>>", (a,b) => a >>> b, "bit-ushr");
  const ash = (a, b) => b < 0 ? a >>> -b : a << b;
  defineGlobalSymbol("ash", ash);  // SIOD
  defineGlobalSymbol("in", (a,b) => a in b);
  defineGlobalSymbol("new", (cls, ...args) => new cls(...args));
  defineGlobalSymbol("instanceof", (a,b) => a instanceof b);
  defineGlobalSymbol("@", (a, b) => a[b]);  // indexing and member access
  defineGlobalSymbol("@?", (a, b) => a?.[b]);  // conditional indexing and member access
  defineGlobalSymbol("@!", (a, b, ...params) => a[b](...params));
  defineGlobalSymbol("@?!", (a, b, ...params) => a?.[b](...params), "@!?");
  defineGlobalSymbol("@=", (a, b, c) => a[b] = c);
  defineGlobalSymbol("delete", (a, b) => delete a[b]);
  defineGlobalSymbol("void", _ => undefined);

  //
  // Variable args definitions
  //
  defineGlobalSymbol("+", add, { compileHook: add_hook }, "add");
  function add(a, b, ...rest) {
    a += b;
    for (b of rest)
      a += b;
    return a;
  }
  function add_hook(args, compileScope, tools) {
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` + ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("-", sub, { compileHook: sub_hook}, "sub");
  function sub(a, ...rest) {
    if (rest.length === 0) return -a;
    for (let b of rest)
      a -= b;
    return a;
  }
  function sub_hook(args, compileScope, tools) {
    if (args.length == 1)
      return `(-${args[0]})`;
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` - ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("*", mul, { compileHook: mul_hook}, "mul");
  function mul(a, b, ...rest) {
    a *= b;
    for (let b of rest)
      a *= b;
    return a;
  }
  function mul_hook(args, compileScope, tools) {
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` * ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol('/', div, { compileHook: div_hook }, "div");
  function div(a, ...rest) {
    if (rest.length === 0) return 1/a;
    for (let b of rest)
      a /= b;
    return a;
  }
  function div_hook(args, compileScope, tools) {
    if (args.length == 1)
      return `(1 / ${args[0]})`;
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` / ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("&", bit_and, { compileHook: bit_and_hook }, "bit-and");
  function bit_and(a, b, ...rest) {
    a &= b;
    for (b of rest)
      a &= b;
    return a;
  }
  function bit_and_hook(args, compileScope, tools) {
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` & ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("|", bit_or, { compileHook: bit_or_hook }, "bit-or");
  function bit_or(a, b, ...rest) {
    a |= b;
    for (let b of rest)
      a |= b;
    return a;
  }
  function bit_or_hook(args, compileScope, tools) {
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` | ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("^", bit_xor, { compileHook: bit_xor_hook}, "bit-xor");
  function bit_xor(a, b, ...rest) {
    a ^= b;
    for (let b of rest)
      a ^= b;
    return a;
  }
  function bit_xor_hook(args, compileScope, tools) {
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` ^ ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("<", lt, { evalArgs: 2, compileHook: lt_hook }, "lt");
  function lt(a, b, forms) {
    if (forms === undefined) return a < b;
    if (!(a < b)) return false;
    a = b;
    while (is_cons(forms)) {
      let b = _eval(forms[CAR], this);
      if (!(a < b)) return false;
      a = b;
      forms = forms[CDR];
    }
    return true;
  }
  function lt_hook(args, compileScope, tools) {
    return compare_hooks(args, compileScope, tools, '<', 'lt');
  }

  function compare_hooks(args, compileScope, tools, op, name) {
    if (args.length < 2)
      return 'false';
    if (args.length == 2)
      return `(${args[0]} ${op} ${args[1]})`;
    let result = tools.newTemp(name);
    tools.emit(`let ${result} = false; ${result}: {`);
    let saveIndent = tools.indent;
    tools.indent = saveIndent + "  ";
    tools.emit(`let a = ${args[0]}, b = ${args[1]};`);
    tools.emit(`if (!(a ${op} b)) break ${result};`);
    for (let i = 2; i < args.length; ++i) {
      emit(`a = b;`);
      let b = compileEval(args[i], compileScope, tools, tools.newTemp);
      emit(`b = ${b};`);
      tools.emit(`if (!(a ${op} b)) break ${result};`);
    }
    tools.emit(`${result} = true;`);
    tools.indent = saveIndent;
    tools.emit(`}`);
    return result;
  }

  defineGlobalSymbol("<=", le, { evalArgs: 2, compileHook: le_hook }, "le");
  function le(a, b, forms) {
    if (forms === undefined) return a <= b;
    if (!(a <= b)) return false;
    a = b;
    while (is_cons(forms)) {
      let b = _eval(forms[CAR], this);
      if (!(a <= b)) return false;
      a = b;
      forms = forms[CDR];
    }
    return true;
  }
  function le_hook(args, compileScope, tools) {
    return compare_hooks(args, compileScope, tools, '<=', 'le');
  }

  defineGlobalSymbol(">", gt, { evalArgs: 2, compileHook: gt_hook }, "gt");
  function gt(a, b, forms) {
    if (forms === undefined) return a > b;
    if (!(a > b)) return false;
    a = b;
    while (is_cons(forms)) {
      let b = _eval(forms[CAR], this);
      if (!(a > b)) return false;
      a = b;
      forms = forms[CDR];
    }
    return true;
  }
  function gt_hook(args, compileScope, tools) {
    return compare_hooks(args, compileScope, tools, '>', 'gt');
  }

  defineGlobalSymbol(">=", ge, { evalArgs: 2, compileHook: ge_hook }, "ge");
  function ge(a, b, forms) {
    if (forms === undefined) return a >= b;
    if (!(a >= b)) return false;
    a = b;
    while (is_cons(forms)) {
      let b = _eval(forms[CAR], this);
      if (!(a >= b)) return false;
      a = b;
      forms = forms[CDR];
    }
    return true;
  }
  function ge_hook(args, compileScope, tools) {
    return compare_hooks(args, compileScope, tools, '>=', 'ge');
  }

  defineGlobalSymbol("==", eq, { evalArgs: 2, compileHook: eq_hook }, "eq?");
  function eq(a, b, forms) {
    if (forms === undefined) return a == b;
    if (!(a == b)) return false;
    a = b;
    while (is_cons(forms)) {
      let b = _eval(forms[CAR], this);
      if (!(a === b)) return false;
      forms = forms[CDR];
    }
    return true;
  }
  function eq_hook(args, compileScope, tools) {
    return compare_hooks(args, compileScope, tools, '==', 'eq');
  }

  // XXX define === and !==

  // Sorry, "equal?"" does not get the variadic treatment at this time
  const equalp = (a, b) =>  deep_eq(a, b);
  defineGlobalSymbol("equal?", equalp, "equal?");

  defineGlobalSymbol("!=", ne, { evalArgs: 2, compileHook: ne_hook }, "ne");
  function ne(a, b, forms) {
    return !eq.call(this, a, b, forms);
  }
  function ne_hook(args, compileScope, tools) {
    let eq = compare_hooks(args, compileScope, tools, '==', 'eq');
    return `(!${eq})`;
  }


  defineGlobalSymbol("max", max);
  function max(a, b, ...rest) {
    let val = a;
    if (val < b) val = b;
    for (b of rest)
      if (val < b) val = b;
    return val;
  }

  defineGlobalSymbol("min", min);
  function min(a, b, ...rest) {
    let val = a;
    if (val > b) val = b;
    for (b of rest)
      if (val > b) val = b;
    return val;
  }

 // logical & conditional

  defineGlobalSymbol("&&", and, { evalArgs: 1 }, "and");
  function and(val, forms) {
    while (bool(val) && is_cons(forms)) {
      val = _eval(forms[CAR], this);
      forms = forms[CDR];
    }
    return val;
  }

  defineGlobalSymbol("||", or, { evalArgs: 1 }, "or");
  function or(val, forms) {
    while (!bool(val) && is_cons(forms)) {
      val = _eval(forms[CAR], this);
      forms = forms[CDR];
    }
    return val;
  }

  defineGlobalSymbol("?", ifelse, { evalArgs: 1, compileHook: ifelse_hook }, "if");
  function ifelse(p, t, f, _) { return bool(p) ? _eval(t, this): _eval(f, this) }
  function ifelse_hook(args, compileScope, tools) {
    let p = args[0], t = args[1], f = args[2];
    let result;
    if (args.length < 3) {
      result = 'undefined';
    } else {
      let p = args[0], t = args[1], f = args[2];
      result = tools.newTemp("if");
      tools.emit(`let ${result};`);
      tools.emit(`if (bool(${p})) {`);
      let saveIndent = tools.indent;
      tools.indent = saveIndent + "  ";
      let tResult = compileEval(t, compileScope, tools);
      tools.emit(`${result} = ${tResult};`);
      tools.indent = saveIndent;
      tools.emit(`} else {`);
      tools.indent = saveIndent + "  ";
      let fResult = compileEval(f, compileScope, tools);
      tools.emit(`${result} = ${fResult};`);
      tools.indent = saveIndent;
      tools.emit(`}`);
    }
    return result;
  }


  // (begin form1 form2 ...)
  defineGlobalSymbol("begin", begin, { evalArgs: 1 });
  function begin(res, forms) {
    while (is_cons(forms)) {
      res = _eval(forms[CAR], this);
      forms = forms[CDR];
    }
    return res;
  }

  // (prog1 form1 form2 form3 ...)
  defineGlobalSymbol("prog1", prog1, { evalArgs: 1 });
  function prog1(val, forms) {
    while (is_cons(forms)) {
       _eval(forms[CAR], this);
      forms = forms[CDR];
    }
    return val;
  }

  // (cond clause1 clause2 ...)  -- clause is (predicate-expression form1 form2 ...)
  defineGlobalSymbol("cond", cond, { evalArgs: 0 });
  function cond(clauses) {
    while (is_cons(clauses)) {
      let clause = clauses[CAR];
      if (!is_cons(clause))
        throw new EvalError(`Bad clause in "cond" ${string(clause)}`);
      let pe = clause[CAR], forms = clause[CDR];
      let evaled = _eval(pe, this);
      if (bool(evaled)) {
        let res = NIL;
        while (is_cons(forms)) {
          res = _eval(forms[CAR], this);
          forms = forms[CDR];
        }
        return res;
      }
      clauses = clauses[CDR];
    }
    return NIL;
  }

  defineGlobalSymbol("require", require_);
  function require_(path) {
    let sym = Atom(`*${path}-loaded*`);
    if (!bool(globalScope[sym])) {
      load.call(this, path);
      globalScope[sym] = true;
      return sym;
    }
    return NIL;
  }

  // (load fname noeval-flag)
  //   If the neval-flag is true then a list of the forms is returned otherwise the forms are evaluated.
  defineGlobalSymbol("load", load);
  function load(path, ...rest) {
    let scope = this, result = NIL, last;
    let noEval = rest.length > 0 &&bool(rest[0]);
    let fileContent;
    try {
      if (!readFile) throw new EvalError("No file reader defined");
      fileContent = readFile(path);
    } catch (error) {
      let loadError = new EvalError(`Load failed ${string(path)}`);
      loadError.cause = error;
      loadError.path = path;
      return false;
    }
    let tokenGenerator = schemeTokenGenerator(fileContent);
    for(;;) {
      try {
        let expr = parseSExpr(tokenGenerator);
        if (!expr) break;
        if (noEval) {
          if (last) last = last[CDR] = cons(expr, NIL);
          else result = last = cons(expr, NIL);
        } else {
          let evaluated = _eval(expr, scope);
          reportLoadResult(evaluated);
        }
      } catch (error) {
        if (error instanceof SchemeJSError)
          reportSchemeError(error);
        else
          reportSystemError(error);
      }
    }
    return result;
  }

  defineGlobalSymbol("append", append);
  function append(...lists) {
    let res = NIL, last;
    for (let list of lists) {
      if (list === NIL || is_cons(list)) {
        // Could handle as iterable, but faster not to
        while (is_cons(list)) {
          if (last) last = last[CDR] = cons(list[CAR], NIL);
          else res = last = cons(list[CAR], NIL);
          list = list[CDR];
        }
      } else if (is_iterable(list)) {  // other iterables
        for (let element of list)
          if (last) last = last[CDR] = cons(element, NIL);
          else res = last = cons(element, NIL);
      }
    }
    return res;
  }

  defineGlobalSymbol("last", last);
  function last(list) {
    let res = NIL;
    if (list === NIL || is_cons(list)) {
      while (is_cons(list)) {
        res = list[CAR];
        list = list[CDR];
      }
    } else {
      // Don't special-case string. Its iterator returns code points by combining surrogate pairs
      if (Array.isArray(list)) {
        if (list.length > 0)
          return list[list.length-1];
        return NIL;
      }
      if (is_iterable(list)) {
        for (let item of list)
          res = item;
      }
    }
    return res;
  }

  defineGlobalSymbol("butlast", butlast);
  function butlast(list) {
    let res = NIL, last;
    if (list === NIL || is_cons(list)) {
      while (is_cons(list) && is_cons(list[CDR])) {
        if (last) last = last[CDR] = cons(list[CAR], NIL);
        else res = last = cons(list[CAR], NIL);
        list = list[CDR];
      }
    } else if (is_iterable(list)) {
      let prev, first = true;
      for (item of list) {
        if (!first)
          if (last) last = last[CDR] = cons(prev, NIL);
          else res = last = cons(prev, NIL);
        prev = item;
        first = false;
      }
    }
    return res;
  }

  defineGlobalSymbol("length", length);
  function length(list) {
    let n = 0;
    if (list === NIL || is_cons(list)) {
      while (is_cons(list)) {
        n += 1;
        list = list[CDR];
      }
    } else {
      // Don't special-case string. Its iterator returns code points by combining surrogate pairs
      if (Array.isArray(list) && list.length > 0)
        return list.length;
      if (is_iterable(list)) {
        for (let item of list)
          n += 1;
      }
    }
    return n;
  }

  defineGlobalSymbol("list", list);
  function list(...elements) {  // easy list builder
    let val = NIL;
    for (let i = elements.length; i > 0; --i)
      val = cons(elements[i-1], val);
    return val;
  }

  defineGlobalSymbol("reverse", reverse);
  function reverse(list) {
    let res = NIL;
    while (is_cons(list)) {
      res = cons(list[CAR], res)
      list = list[CDR];
    }
    return res;
  }

  // (member key list)
  //     Returns the portion of the list where the car is equal to the key, or () if none found.
  defineGlobalSymbol("member", member);
  function member(key, list) {
    while (is_cons(list)) {
      if (key == list[CAR])
        return list;
      list = list[CDR];
    }
    return NIL;
  }

  // (memq key list)
  //     Returns the portion of the list where the car is eq to the key, or () if none found.
  defineGlobalSymbol("memq", memq);
  function memq(key, list) {
    while (is_cons(list)) {
      if (key === list[CAR])
        return list;
      list = list[CDR];
    }
    return NIL;
  }

  // (nth index list)
  //     Reference the list using index, with the first element being index 0.
  defineGlobalSymbol("nth", nth);
  function nth(index, list) {
    if (typeof index !== 'number' || Math.trunc(index) !== index)
      throw new EvalError(`Not an integer ${string(index)}`);
    if (index < 0) return NIL;
    if (list === NIL || is_cons(list)) {
      while (index > 0 && is_cons(list)) {
        index -= 1;
        list = list[CDR];
      }
      if (is_cons(list))
        return list[CAR];
  ``} else if (Array.isArray(list)) {
      if (index < list.length)
        return list[index];
    } else if (is_iterable(list)) {
      for (let item of list) {
        if (index <= 0)
          return item;
        index -= 1;
      }
    }
    return NIL;
  }

  // (apropos substring) -- Returns a list of all symbols containing the given substring
  defineGlobalSymbol("apropos", apropos);
  function apropos(substring) {
    if (!substring) substring = "";
    substring = substring.toLowerCase();
    let matches = NIL, scope = this;
    while (scope && scope !== Object) {
      let symbols = Object.getOwnPropertySymbols(scope);
      for (let symbol of symbols) {
        if (!is_atom(symbol)) continue;
        let name = string(symbol);
        if (name.toLowerCase().includes(substring))
          matches = cons(symbol, matches);
      }
      scope = Object.getPrototypeOf(scope);
    }
    return this.sort(matches,
      (a,b) => a.description.toLowerCase() < b.description.toLowerCase() ? -1 :
               b.description.toLowerCase() < a.description.toLowerCase() ? 1 : 0);
  }

  // (mapcar fn list1 list2 ...)
  defineGlobalSymbol("mapcar", mapcar), "map";
  function mapcar(fn, ...lists) {
    if (!bool(fn)) return NIL;
    // Actually, this will work for any iterables and lists are iterable.
    let res = NIL, last;
    for (let list of lists) {
      if (list === NIL || is_cons(list)) {
        // Could just let the list iterator handle it but might as well just follow the Cons chain
        // and not have to manufacture an iterator.
        while (is_cons(list)) {
          let item = list[CAR];
          item = _apply(fn, cons(item, NIL), this);
          if (last) last = last[CDR] = cons(item, NIL);
          else res = last = cons(item, NIL);
            list = list[CDR];
        }
      } else if (is_iterable(list)) {
        for (let item of list) {
          item = _apply(fn, cons(item, NIL), this);
          if (last) last = last[CDR] = cons(item, NIL);
          else res = last = cons(item, NIL);
        }
      }
    }
    return res;
  }

  // Same as mapcar but results in an Array
  defineGlobalSymbol("map->array", map_to_array);  // XXX name
  function map_to_array(fn, ...lists) {
    let res = [];
    for (let list of lists) {
      for (let item of list) {
        item = _apply(fn, cons(item, NIL), this);
        res.push(item);
      }
    }
    return res;
  }

  // (let (binding1 binding2 ...) form1 form2 ...) -- let* behavior
  //     (let ((x 10)
  //           (y 20))
  //       (+ x y))
  // Because of this implementation uses a scope chain instead
  // of an environment, each kind of let is as piwerful as "letrec".
  //
  // TODO: Make a shape-shifting Scope object that mimics a Cons
  // and transforms into an environment on demand, conversely
  // functions that take an "env" param should transform it back into
  // a Scope. That will maintain Scheme API compatibility while
  // still benefiting from Scopes internally.
  defineGlobalSymbol("letrec", letrec, { evalArgs: 0 }, "let", "let*");
  function letrec(forms) {
    if (!is_cons(forms)) throw new EvalError(`No bindings`);
    let bindings = forms[CAR];
    forms = forms[CDR];
    let scope = newScope(this, "letrec-scope");
    while (is_cons(bindings)) {
      let binding = bindings[CAR];
      if (!is_cons(binding))
        throw new EvalError(`Bad binding ${string(binding)}`);
      let boundVar = binding[CAR], bindingForms = binding[CDR];
      if (typeof boundVar !== 'symbol')
        throw new EvalError(`Bad binding ${string(binding)}`);
      let val = NIL;
      while (is_cons(bindingForms)) {
        val = _eval(bindingForms[CAR], scope);
        bindingForms = bindingForms[CDR];
      }
      scope[boundVar] = val;
      bindings = bindings[CDR];
    }
    let res = NIL;
    while (is_cons(forms)) {
      res = _eval(forms[CAR], scope);
      forms = forms[CDR];
    }
    return res;
  }

  // (qsort list predicate-fcn access-fcn)
  //   "qsort" is a lie for API compatibility with SIOD, but this sort has
  //   comparable performance and is excellent with partially-sorted lists.
  defineGlobalSymbol("sort", mergesort, "qsort");
  function mergesort(list, ...rest) {
    let predicateFn = rest[0], accessFn = rest[1];
    let before = predicateFn;
    if (bool(predicateFn)) {
      if (bool(accessFn)) {
        before = function(a, b) {
          return predicateFn.call(this, accessFn.call(this, a), accessFn.call(this, b));
        }
      } else if (typeof predicateFn !== 'function') {
        // Make sure it's a JS function, not a Scheme function
        before = function(a, b) {
          return predicateFn.call(this, predicateFn);
        }
      }
    } else {
      if (bool(accessFn)) {
        before = function(a, b) {
          a = accessFn.call(this, a);
          b = accessFn.call(this, b);
          if (typeof a === 'symbol') a = a.description;
          if (typeof b === 'symbol') b = b.description;
          return a < b;
        }
      } else {
        before = function(a, b) {
          if (typeof a === 'symbol') a = a.description;
          if (typeof b === 'symbol') b = b.description;
          return a < b;
        }
      }
    }
    if (!bool(list)) return NIL;  // includes NIL
    if (is_cons(list)) {
      // llsort is in-place, so first copy the list.
      // There are no new Cons cells after this, so it's a bargain.
      let copied = NIL, tail;
      while (is_cons(list)) {
        let next = list[CDR];
        if (tail) tail = tail[CDR] = list;
        else copied = tail = list;
        list = next;
      }
      return llsort(copied);
    }
    if (!accessFn && is_iterable(list)) { // Bail out to JavaScript sort
      // This expands iterables into an array. It also copies arrays,
      // which is good because JavaScript sort is in-place.
      // The JavaScript sort algorithm sorts as if keys were were strings;
      // this is very bad for numbers.
      // That's not what we did for lists, so we override that.
      let array = [ ...list ];
      array.sort((a,b) => before.call(this, a, b) ? -1 : 1);
      return array;
    }
    throw new EvalError(`Not a list or iterable ${string(list)}`);
  
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
    function llsort(list) {
      let stack = [];
      while (is_cons(list)) {
        // Accumulate a run that's already sorted.
        let run = list, runTail = list;
        list = list[CDR];
        while (is_cons(list)) {
          let listNext = list[CDR];
          runTail[CDR] = NIL;
          if (before.call(this, list[CAR], run[CAR])) {
            list[CDR] = run;
            run = list;
          } else {
            if (!before.call(this, list[CAR], runTail[CAR])) {
              runTail[CDR] = list;
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
        // It's helpful put a breakpoint right here and "watch" these expressions:
        //     string(list)
        //     string(run)
        //     string(stack[0])
        //     string(stack[1])
        //     etc.
        let i = 0;
        for ( ; i < stack.length; ++i) {
          if (stack[i] === NIL) {
            stack[i] = run;
            run = NIL;
            break;
          };
          run = merge(stack[i], run);
          stack[i] = NIL;
        }
        if (run !== NIL)
          stack.push(run);
      }
      // Merge all remaining stack elements
      let run = NIL;
      for (let i = 0; i < stack.length; ++i)
        run = merge(stack[i], run);
      return run;
    }

    function merge(left, right) {
      // When equal, left goes before right
      let merged = NIL, last;
      while (is_cons(left) && is_cons(right)) {
        if (before.call(this, right[CAR], left[CAR])) {
          let next = right[CDR];
          if (last) last[CDR] = right;
          else merged = right;
          last = right;
          right = next;
        } else {
          let next = left[CDR];
          if (last) last[CDR] = left;
          else merged = left;
          last = left;
          left = next;
        }
        last[CDR] = NIL;
      }
      // Can't both be Cons cells; the loop above ensures it
      if (is_cons(left)) {
        if (last) last[CDR] = left;
        else merged = left;
      } else if (is_cons(right)) {
        if (last) last[CDR] = right;
        else merged = right;
      }
      return merged;
    }
  }

  exportAPI("deep_eq", deep_eq);
  function deep_eq(a, b, maxDepth, report) {
    if (!bool(maxDepth)) maxDepth = 10000; // Protection from circular lists
    return deep_eq(a, b, maxDepth);
    function deep_eq(a, b, maxDepth) {
      if (a === b)
        return true;
      if (typeof a !== typeof b) {
        if (report) report.a = a, report.b = b;
        return false;
      }
      // Normally NaNs are not equal to anything, including NaNs, but for
      // the purposes of this routine they are
      if (typeof a === 'number' && isNaN(a) && isNaN(b))
        return true;
      if (a == null || b == null) { // nullish and we already know thay aren't equal
        if (report) report.a = a, report.b = b;
        return false;
      }
      if (maxDepth <= 0) {
        if (report) report.depth = true;
        return false;
      }
      maxDepth -= 1;

      if (is_cons(a)) {
        if (!is_cons(b)) {
          if (report) report.a = a, report.b = b;
          return false;
        }
        return deep_eq(a[CAR], b[CAR]) && deep_eq(a[CDR], b[CDR], maxDepth);
      }
      if (is_cons(b)) {
        if (report) report.a = a, report.b = b;
        return false;
      }
      
      if (Array.isArray(a)) {
        if (!Array.isArray(b) || a.length !== b.length) {
          if (report) report.a = a, report.b = b;
          return false;
        }
        for (let i = 0; i < a.length; ++i)
          if (!deep_eq(a[i], b[i], maxDepth)) {
            if (report) {
              report.a = a, report.b = b, report.index = i;
              report = undefined;  // Don't let this report get overwritten!
            }
            return false;
          }
        return true;
      }
      if (Array.isArray(b)) {
        if (report) report.a = a, report.b = b;
        return false;
      }
      
      if (typeof a === 'object') {
        for (let prop of Object.getOwnPropertyNames(a).concat(Object.getOwnPropertySymbols(a)))
          if (!b.hasOwnProperty(prop)) {
            if (report) {
              report.a = a, report.b = b, report.prop = prop;
              report.aVal = a[prop], report.bVal = b[prop];
              report = undefined;  // Don't let this report get overwritten!
            }
            return false;
          }
        for (let prop of Object.getOwnPropertyNames(b).concat(Object.getOwnPropertySymbols(b)))
          if (!(a.hasOwnProperty(prop) && deep_eq(a[prop], b[prop], maxDepth))) {
            if (report) {
              report.a = a, report.b = b, report.prop = prop;
              report.aVal = a[prop], report.bVal = b[prop];
              report.hasProp = a.hasOwnProperty(prop);
              report = undefined;  // Don't let this report get overwritten!
            }
            return false;
          }
        return true;
      }

      if (report) report.a = a, report.b = b;
      return false;
    }
  }

  // SIOD compatibility checklist:
  //
  // TODO benchmark fns -- http://people.delphiforums.com/gjc//siod.html#builtin
  // (realtime)
  //      Returns a double precision floating point value representation of the current realtime number of seconds. Usually precise to about a thousandth of a second.
  // errobj, (error message object)
  // (number->string x base width precision)
  //     Formats the number according to the base, which may be 8, 10, 16 or the symbol e or f.
  //     The width and precision are both optional.
  // (parse-number str)
  // (print object stream) -- Same as prin1 followed by output of a newline.
  // (rand modulus) -- Computes a random number from 0 to modulus-1. Uses C library rand. (srand seed)
  // (random modulus) -- Computes a random number from 0 to modulus-1. Uses C library random. (srandom seed)
  // (runtime)
  //     Returns a list of containing the current cpu usage in seconds and the subset amount of cpu time
  //     that was spent performing garbage collection during the currently extant read-eval-print loop cycle.
  // (save-forms filename forms how)
  //    Prints the forms to the file, where how can be "w" (default) or "a" to append to the file.
  // (sdatref spec data) -- Used as the %%closure-code by mkdatref.
  // (set! variable value)
  //    A special form that evaluates the value subform to get a value, and then assigns the variable
  //    to the value.  ??? where ???
  // (set-symbol-value! symbol value env)
  //    Finds the location of the value cell for the specified symbol in the environment env and sets the value.
  // (strbreakup string sep)
  //    Return a list of the portions of string indicated by the separator.
  // (unbreakupstr list sep) -- The reverse of strbreakup.
  // (string-append str1 str2 str3 ...) -- same as +
  // (string->number str radix)
  // (string-downcase str) -- also string-downcase
  //    Return a new string converting all the characters of str to lowercase.
  // (string-length str)
  //    Returns the active string length of str.
  // (string-lessp str1 str2)
  //    Return true if str1 is alphabetically less than str2.
  // (string-search key str)
  //    Locate the index of the key in the specified string. Returns () if not found.
  // (string-trim str)
  //    Return a new string made by trimming whitespace from the left and right of the specified string.
  // (string-trim-left str)
  //    Like string-trim but only the left hand side.
  // (string-trim-right str)
  //    Like string-trim but only the right hand side.
  // (string-upcase str)
  //    Returns a new string with all the lowercase characters converted to uppercase.
  // (string? x)
  //    Returns true if x is a string.
  // (strspn str indicators)
  //    Returns the location of the first character in str which is not found in the indicators set, returns the length of the str if none found. For example:
  //    (define (string-trim-left x)
  //       (substring x (strspn x " \t")))
  // (subset pred-fcn list) -- aka filter?
  //    Return the subset of the list such that the elements satisify the pred-fcn. For example:
  //    (subset number? '(1 b 2 c)) => (1 2)
  // (substring str start end)
  //    Returns a new string made up of the part of str begining at start and terminating at end. In other words, the new string has a length of end - start.
  // (substring-equal? str str2 start end)
  //    An efficient way to determine if the substring of str2 specified by start and end is equal to str1.
  // (symbol-bound? symbol env)
  //    Returns true of the symbol is bound in the environment.
  // (symbol-value symbol env)
  //    Returns the value of the symbol in the environment.
  // (symbol? x)
  //    Returns true if x is a symbol.
  // (symbolconc arg1 arg2 ...)
  //    Slightly more efficient than calling intern on the result of using string-append on the arguments. This procedure actually predates the availability of the string data type in SIOD.
  // t -- Please do not change the global value of this variable, bound to a true value.
  // (tan x)
  // (the-environment) -- A special form which returns the interpreter environment structure for the current lexical scope.
  // (trunc x) -- Returns the integer portion of x.
  // (typeof x) -- Returns a symbol describing the type of the object x, or the integer type code. Hmmm
  // (unix-ctime x) -- Converts the integer time x into a string. U
  // (unix-time) -- Returns the current number of seconds since 1-JAN-1970 GMT. U
  // (unix-time->strtime x) Returns a string of the form "YYYYMMDDHHmmSSdd" which is useful in some contexts. This predates the availability of the strftime procedure.
  // (url-decode str)
  //    Performs the url decode operation on the str. See chtml.html for example usage.
  // (url-encode str)
  //    Locates characters in the str which should not appear in a url, and returns a new string where they have been converted to the %NN hex representation. Spaces are converted to "+" signs.
  // (verbose arg)
  //    Sets the verbosity level of SIOD to the specified level or returns the current level if not specified.
  //    Verbose Level	Effect on System
  //      0 No messages.
  //      1 Error messages only.
  //      2 Startup messages, prompts, and evaluation timing.
  //      3 File loading and saving messages.
  //      4 (default)	Garbage collection messages.
  //      5 display of data loaded from files and fetched from databases.
  // (while pred-form form1 form2 ...)
  //    If pred-form evaluates true it will evaluate all the other forms and then loop.

  // Maybe a non-recursive evaluator with explicit stack?
  // Promises and async functions?

  // (\ (params) (body1) (body2) ...)
  defineGlobalSymbol(LAMBDA_ATOM, lambda, { evalArgs: 0 });
  function lambda(body) { return cons(CLOSURE_ATOM, cons(this, body)) }

  // (\\ (params) (body1) (body2) ...)
  // (\\ param . form) -- Curry notation
  defineGlobalSymbol(SLAMBDA_ATOM, special_lambda, { evalArgs: 0 });
  function special_lambda(body) { return cons(SCLOSURE_ATOM, cons(0, cons(this, body))) }

  exportAPI("is_closure", is_closure)
  defineGlobalSymbol("closure?", is_closure);
  function is_closure(form) {
    return is_cons(form) && (form[CAR] === CLOSURE_ATOM || form[CAR] === SCLOSURE_ATOM);
  }

  //
  // try/catch/filnally.
  //
  class SchemeJSThrow extends SchemeJSError {
    constructor(tag, value, msg) {
      value;
      super(msg);
      this.tag = tag;
      this.value = value;
    }
    toString() {
      return `${super.toString()} ${this.tag} ${string(this.value)}`;
    }
  };
  SchemeJSThrow.prototype.name = "SchemeJSThrow";

  // (*throw tag value) -- SIOD style
  defineGlobalSymbol("*throw", schemeThrow);
  function schemeThrow(tag, value) { throw new SchemeJSThrow(tag, value)}

  // (*catch tag form ...) -- SIOD style
  defineGlobalSymbol("*catch", schemeCatch, { evalArgs: 1 });
  function schemeCatch(tag, forms) {  // XXX order of args?
    let val = NIL;
    try {
      while (is_cons(forms)) {
        val = _eval(forms[CAR], this);
        forms = forms[CDR];
      }
    } catch (e) {
      if (!(e instanceof SchemeJSThrow)) throw e;  // rethrow
      if (e.tag !== tag) throw e;
      val = e.value;
    }
    return val;
  }

  // (throw value) -- Java/JavaScript style
  defineGlobalSymbol("throw", jsThrow);
  function jsThrow(value) { throw value }

  // (catch (var [type] forms) forms) -- Java/JavaScript style
  defineGlobalSymbol("catch", jsCatch, { evalArgs: 0 });
  function jsCatch(catchClause, forms) {
    if (!is_cons(catchClause))
      throw new EvalError(`Bad catch clause ${string(catchClause)}`);
    let catchVar = catchClause[CAR], catchForms = catchClause[CDR];
    if (!is_cons(catchForms))
      throw new EvalError(`Bad catch clause ${string(catchClause)}`);
    var typeMatch;
    if (typeof catchForms[CAR] === 'string' || typeof catchForms[CAR] === 'function') {
      typeMatch = catchForms[CAR];
      catchForms = catchForms[CDR];
    }
    if (!is_cons(catchForms))
      throw new EvalError(`Bad catch clause ${string(catchClause)}`);
    let val = NIL;
    try {
      while (is_cons(forms)) {
        val = _eval(forms[CAR], this);
        forms = forms[CDR];
      }
    } catch (e) {
      if (!typeMatch || (typeof typeMatch === 'string' && typeof e === typeMatch)
          || e instanceof typeMatch) {
        let scope = newScope(this, "catch-scope");
        scope[catchVar] = e;
        while (is_cons(catchForms)) {
          val = _eval(catchForms[CAR], scope);
          catchForms = catchForms[CDR];
        }
      } else {
        throw e; // rethrow
      }
    }
    return val;
  }

  // (define variable value)
  // (define (fn args) forms)
  defineGlobalSymbol("define", define, { evalArgs: 0 });
  function define(forms) {
    if (!(is_cons(forms) && is_cons(forms[CDR])))
      throw new EvalError(`Define requires two parameters`);
    let defined = forms[CAR], value = forms[CDR][CAR];
    let scope = this, name = defined;
    if (is_cons(defined)) {
      name = defined[CAR];
      let args = defined[CDR];
      value = list(LAMBDA_ATOM, args, value);
    } else {
      value = _eval(value, scope);
    }
    if (typeof name === 'string') name = Atom(name);
    // Prevent a tragic mistake that's easy to make by accident. (Ask me how I know.)
    if (name === QUOTE_ATOM) throw new EvalError("Can't redefine quote");
    if (typeof name !== 'symbol')
      throw new EvalError(`must define symbol or string ${string(defined)}`);
    globalScope[name] = value;
    return name;
  }

  //
  // This is where the magic happens
  //

  function _eval(form, scope) {
    if (form === NIL) return form;
    if (typeof form === 'symbol') {
      let val = scope[form];
      if (val === undefined) throw new EvalError(`Undefined symbol ${string(form)}`);
      return val;
    }
    if (is_cons(form)) {
      let fn = form[CAR], args = form[CDR];
      if (fn === QUOTE_ATOM) // QUOTE is a function that will do this, but catch it here anyway.
        return form[CDR][CAR];
      if (is_cons(fn)) {
        let fnCar = fn[CAR];
        if (!(fnCar === LAMBDA_ATOM || fnCar === SLAMBDA_ATOM))
          fn = _eval(fn, scope);
      } else {
        fn = _eval(fn, scope);
      }
      // Unconventionally shifting the job of evaluating the args to the _apply function
      // because it has better visibility on the function's attributes.
      // It won't evaluate the arguments unless passed an evalCount.
      let evaluateParams = true;
      return _apply(fn, args, scope, evaluateParams);
    }
    // Special eval for JS arrays and objects:
    //   Values that are evaluated and placed in
    //   a new Object or Array in correspoding position.
    // XXX Investigate Symbol.species (also for mapcar, etc.)
    if (form !== null && typeof form === 'object') {
      if (form instanceof Array) {
        let res = [];
        for (let item of form) {
          let val = _eval(item, scope);
          res.push(val);
        }
        return res;
      } else {
        let res = {};
        for (let key of [ ...Object.getOwnPropertyNames(form), ...Object.getOwnPropertySymbols(form) ]) {
          let value = form[key];
          if (value instanceof EvaluateKeyValue) {
            key = value.key;
            value = value.val;
            key = _eval(key, scope);
          }
          value = _eval(value, scope);
          res[key] = value;
        }
        return res;
      }
    }
    return form;
  }

  // Invocation of JavaScript functions:

  // When defined, a function can be annotated with the number of arguments
  // to be evaluated. The definition is also examined to determine the number of
  // non-rest parameters. Every evaluated parameter is passed as a javascript argument.
  // If there are not as many arguments as parameters, a closure is returned that
  // binds a partial application of that function.
  // Functions with unevaluated arguments get those forms as a final parameter, after
  // the evaluated ones.

  function _apply(form, args, scope, evaluateArguments) {
    // Typically, it would be eval's job to evaluate the arguments but in the case of JS
    // functions we don't know how many arguments to evaluate until we've read the
    // function descriptor and I want the logic for that all in one place. Here, in fact.
    // By default apply doesn't evaluate its args, but if evaluateArguments is set
    // (as it is by eval) it does.
    let paramCount = 0, evalCount = MAX_INTEGER;
    if (is_cons(form)) {
      let opSym = form[CAR];
      if (opSym === SLAMBDA_ATOM) {
        evalCount = 0;
      } else if (opSym === SCLOSURE_ATOM) {
        if (!is_cons(form[CDR])) throw new EvalError(`Bad form ${string(form)}`);
        evalCount = form[CDR][CAR];
      }
    } else if (typeof form === 'function') {
      let fn = form;
      // The function descriptor is encoded as: (~evalCount << 8) | paramCount&0xff;
      // If there's no function descriptor the default is to eval every argument
      // which, by no accident, is zero.
      let functionDescriptor = fn[FUNCTION_DESCRIPTOR_SYMBOL] ?? 0;
      // Turns paramCounts and evalCounts that were MAX_INTEGER back into MAX_INTEGER, without branches
      evalCount = ~functionDescriptor >> 7 >>> 1;
      paramCount = functionDescriptor & 0xff;;
    }
    if (evaluateArguments) {
      let evalledArgs = NIL, last = undefined;
      for (let i = 0; i < evalCount && is_cons(args); ++i) {
        let evalledArgCons = cons(_eval(args[CAR], scope), evalledArgs);
        if (last) last = last[CDR] = evalledArgCons;
        else evalledArgs = last = evalledArgCons;
        args = args[CDR];
      }
      if (last) {
        last[CDR] = args;
        args = evalledArgs;
      }
    }
    let lift = evalCount > paramCount ? evalCount : paramCount;
    if (typeof form === 'function') {
      let jsArgs = [];
      for (let i = 0; i < lift; ++i) {
        if (is_cons(args)) {
          jsArgs.push(args[CAR]);
          args = args[CDR];
        } else { 
          if (i < paramCount) {
            if (i < 1) {
              // We can't partially apply without any arguments, but the function wants
              // parameters anyway so we supply undefined. This will allow the
              // "forms" parameter to go into the correct parameter.
              while (i++ < paramCount)
                jsArgs.push(undefined);
              break;
            }
            // Partial application of built-in functions
            let paramList = NIL;
            for (let pnum = paramCount; pnum > i; --pnum)
              paramList = cons(Atom(`p${(pnum)}`), paramList);
            let argList = paramList;
            for (let anum = i; anum > 0; --anum)
              argList = cons(jsArgs[anum-1], argList);
            let forms = cons(cons(form, argList), NIL);
            if (i < evalCount)
              return cons(CLOSURE_ATOM, cons(scope, cons(paramList, forms)));
            return cons(SCLOSURE_ATOM, cons(i-evalCount, cons(scope, cons(paramList, forms))));
          }
          break;
        }
      }
      if (evalCount !== MAX_INTEGER)
        jsArgs.push(args);
      return form.apply(scope, jsArgs);
    }
    if (is_cons(form)) {
      let opSym = form[CAR];
      let body = form[CDR];
      if (!is_cons(body)) throw new EvalError(`Bad form ${string(form)}`);
      if (opSym === CLOSURE_ATOM) {
        if (!is_cons(body)) throw new EvalError(`Bad closure ${string(form)}`);
        scope = body[CAR];
        body = body[CDR];
        opSym = LAMBDA_ATOM;
      }
      if (opSym === SCLOSURE_ATOM) {
        if (!is_cons(body)) throw new EvalError(`Bad closure ${string(form)}`);
        scope = body[CDR][CAR];
        body = body[CDR][CDR];
        opSym = LAMBDA_ATOM;
      }
      if (opSym === LAMBDA_ATOM || opSym === SLAMBDA_ATOM) {
        if (!is_cons(body)) throw new EvalError(`Bad lambda ${string(form)}`);
        let params = body[CAR];
        let forms = body[CDR];
        if (typeof params === 'symbol') { // Curry notation :)
          params = cons(params, NIL);
          forms = cons(forms, NIL);
        }
        scope = newScope(scope, "lambda-scope");
        let origFormalParams = params;
        while (is_cons(params)) {
          let param = params[CAR];
          if (typeof param !== 'symbol') throw new EvalError(`Param must be a symbol ${param}`);
          if (args !== NIL) {
            scope[param] = args[CAR];
            if (is_cons(args)) args = args[CDR];
          } else {
            // Curry up now! (partial application)
            if (opSym === LAMBDA_ATOM)
              return cons(CLOSURE_ATOM, cons(scope, cons(params, forms)));
            else
              return cons(CSLOSURE_ATOM, cons(scope, cons(params, forms)));
          }
          params = params[CDR];
        }
        if (typeof params === 'symbol')  // Neat trick for 'rest' params!
          scope[params] = args;
        else if (params !== NIL)
          throw new EvalError(`Bad parameter list ${string(origFormalParams)}`);
        let res = NIL;
        while (is_cons(forms)) {
          res = _eval(forms[CAR], scope);
          forms = forms[CDR];
        }
        return res;
      }
    }
    throw new EvalError(`Can't apply ${string(form)}`);
  }

  const ESCAPE_STRINGS = { t: '\t', n: '\n', r: '\r', '"': '"', '\\': '\\', '\n': '' };
  const STRING_ESCAPES = (() => {
    let res = {};
    for (let [key, value] of Object.entries(ESCAPE_STRINGS))
      res[value] = '\\' + key;
    return res;
  })();

  // Implements "toString()" for SchemeJS objects.
  // We can't just implement toString() because it needs to work for
  // non-Object types too, but Cons.toString() calls this.
  defineGlobalSymbol("to-string", string);
  function string(obj, opts = {}) {
    opts = { ...schemeOpts, ...opts };
    let stringWrap = opts.stringWrap ?? 100;
    let wrapChar = opts.wrapChar ?? "\n";
    let maxDepth = opts.maxDepth ?? 100;
    let indentMore = opts.indentMorw ?? "  ";
    let line = "", lines = [], sep = "", prefix = "", indent = "";
    toString(obj);
    if (lines.length === 0) return line;
    if (line)
      lines.push(line);
    return lines.join(wrapChar);
    function toString(obj, maxDepth) {
      if (maxDepth <= 0) return put("...");
      maxDepth -= 1;
      if (obj === NIL) return put("()");
      if (obj === undefined) return put("undefined");
      if (obj === null) return put( "null");   // remember: typeof null === 'object'!
      let objType = typeof obj;
      let saveIndent = indent;
      if (typeof obj === 'function') {
        let fnDesc = analyzeJSFunction(obj);
        let name = fnDesc.name ? ` ${fnDesc.name}` : '';
        let params = "";
        for (let param of fnDesc.params) {
          if (params) params += ', ';
          params += param;
        }
        if (fnDesc.restParam) {
          if (params) params += ', ';
          params = `${params}...${fnDesc.restParam}`;
        }
        params = `(${params})`;
        let printBody = fnDesc.printBody;
        if (fnDesc.value && !fnDesc.body && !printBody)
          return put(`{${params} => ${fnDesc.value}}`);
        if (printBody.length > 60 || printBody.includes('\n'))
          printBody = '';
        return put(`{function${name}${params}${printBody}}`);
      }
      if (objType === 'object') {
        if (obj instanceof Scope) {
          let symStrs = "";
          if (obj !== globalScope) {
            for (let sym of Object.getOwnPropertySymbols(obj)) {
              if (!is_atom(sym)) continue; // Not an atom (e.g. SCOPE_IS_SYMBOL)
              let desc = sym.description;
              if (symStrs.length + desc.length > 20) {
                symStrs += "...";
                break;
              }
              symStrs += ` ${desc}`;
            }
          }
          return put(`{*${obj[SCOPE_IS_SYMBOL]}*${symStrs}}`);
        }
        if (is_cons(obj)) {
          if (obj[LAZY])
            return put(`(${string(obj[CAR])} ...)`);
          let objCar = obj[CAR];
          if ((objCar === LAMBDA_ATOM || objCar === SLAMBDA_ATOM ||
               objCar == CLOSURE_ATOM || objCar === SCLOSURE_ATOM)
                && is_cons(obj[CDR])) {
            // Specal treatment of lambdas and closures with curry notation
            if (objCar == CLOSURE_ATOM|| objCar === SCLOSURE_ATOM) {
              if (is_cons(obj[CDR][CDR])) {
                let evalCount, scopeCons = obj[CDR];
                if (objCar === SCLOSURE_ATOM) {
                  evalCount = obj[CDR][CAR];
                  scopeCons = obj[CDR][CDR];
                }
                let params = scopeCons[CDR][CAR];
                if (typeof params === 'symbol') {
                  put("(");
                  indent += indentMore;
                  sep = "";
                  if (objCar === SCLOSURE_ATOM) {
                    toString(evalCount, maxDepth);
                    sep = " ";
                  }
                  toString(objCar, maxDepth);  // %%closure or %%%closure
                  sep = " ";
                  toString(scopeCons[CAR], maxDepth);  // scope
                  sep = " ";
                  toString(params, maxDepth);  // actually the atom
                  sep = ""; put(".");
                  toString(scopeCons[CDR][CDR], maxDepth);  // the form
                  sep = ""; put(")", true);
                  indent = saveIndent;
                  return
                }
              }
            }
            let str = "(", params = obj[CDR][CAR], forms = obj[CDR][CDR];
            if (objCar === LAMBDA_ATOM) str += lambdaStr;
            if (objCar === SLAMBDA_ATOM) str += slambdaStr;
            if (typeof params === 'symbol') {  // curry notation
              str += `${params.description}.`;
              put(str);
              indent += indentMore;
              toString(forms, maxDepth);
              sep = "";
              put(")", true);
              indent = saveIndent;
              return;
            }
          }
          put("(");
          indent += indentMore;
          sep = "";
          while (is_cons(obj) && !obj[LAZY]) {
            toString(obj[CAR], maxDepth);
            sep = " ";
            obj = obj[CDR];
          }
          if (obj !== NIL) {
            put(".");
            sep = " ";
            toString(obj, maxDepth)
          }
          sep = "";
          put(")", true);
          indent = saveIndent;
          return;
        }
        if (obj instanceof Array) {
          put("[");
          indent += indentMore;
          sep = "";
          for (let item of obj) {
            toString(item, maxDepth);
            sep = ", ";
          }
          sep = "";
          put("]", true);
          indent = saveIndent;
          return;
        }
        {
          // Plain object
          put("{");
          indent += indentMore;
          sep = "";
          for (let name of [ ...Object.getOwnPropertyNames(obj), ...Object.getOwnPropertySymbols(obj) ]) {
            let item = obj[name];
            if (item instanceof EvaluateKeyValue) {
              prefix = "[";
              toString(item.key);
              prefix = "]: ";
              toString(item.value);
            } else {
              prefix = `${string(name)}: `;
              toString(item, maxDepth);
            }
            sep = ", ";
          }
          sep = "";
          put("}", true);
          indent = saveIndent;
          return;
        }
      }
      if (objType === 'symbol') {
        if (obj === LAMBDA_ATOM) return put(lambdaStr);
        if (obj === SLAMBDA_ATOM) return put(slambdaStr);
        return put(obj.description);
      }
      if (objType === 'string') {
        let str = '"';
        for (let ch of obj) {
          let replace = STRING_ESCAPES[ch];
          if (replace) {
            ch = replace;
          } else {
            let charCode = ch.charCodeAt(0);
            if (!(charCode >= 0x20 && charCode < 0x7f)) {
              let s = '0000' + charCode.toString(16);
              ch = '\\u' + s.substr(s.length-4);
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

  // Turns iterable objects like arrays into lists, recursively to "depth" (default 1) deep.
  defineGlobalSymbol("to-list", to_list);
  function to_list(obj, depth) {
    if (!bool(depth)) depth = 1;
    if (depth <= 0) return obj;
    if (obj === NIL || is_cons(obj)) return obj;
    if (typeof obj === 'object') {
      if (is_cons(obj)) return obj;  // Careful; Cons is iterable itself
      if (is_iterable(obj)) {
        let list = NIL, last;
        for (let value of obj) {
          if (depth > 1 && value[Symbol.iterator])
            value = to_list.call(this, value, depth-1);
          if (last) last = last[CDR] = cons(value, NIL);
          else list = last = cons(value, NIL);
        }
        return list;
      }
    }
    return NIL;
  }

  class IteratorList {
    [CAR]; [LAZY]; _cdrVal; _mapper;
    constructor(car, iterator, mapper) {
      this[CAR] = car;
      this[LAZY] = iterator;
      this._mapper = mapper;
    }
    toString() { return string(this) }
    get [CDR]() {
      let iterator = this[LAZY];
      if (!iterator)
        return this._cdrVal;
      let { done, value } = iterator.next();
      if (done) {
        this[LAZY] = undefined;
        return this._cdrVal = NIL;
      }
      let mapper = this._mapper;
      if (mapper)
        value = mapper(value);
      let cdr = new IteratorList(value, iterator, mapper);
      this[LAZY] = undefined;
      return this._cdrVal = cdr;
    }
    set [CDR](val) {
      this._iterator = undefined;
      this._cdrVal = val;
    }
  }
  IteratorList.prototype[PAIR] = true;

  defineGlobalSymbol("lazy-list", lazy_list);
  function lazy_list(obj) {
    let iterator = obj;
    if (is_iterable(obj))
      iterator = obj[Symbol.iterator]();
    if (iterator == null || typeof iterator.next !== 'function')
      throw new EvalError(`Not an iterable or iterator ${obj}`);
    let { done, value } = iterator.next();
    if (done) return NIL;
    return new IteratorList(value, iterator);
  }

  defineGlobalSymbol("lazy-map", lazy_map);
  function lazy_map(fn, obj) {
    if (!bool(fn)) return NIL; // XXX probably should throw; also mapcar
    let iterator = obj;
    if (is_iterable(obj))
      iterator = obj[Symbol.iterator]();
    if (iterator == null || typeof iterator.next !== 'function')
      throw new EvalError(`Not an iterable or iterator ${obj}`);
    let { done, value } = iterator.next();
    if (done) return NIL;
    let mapper = value => _apply(fn, cons(value, NIL), this);
    value = mapper(value);
    return new IteratorList(value, iterator, mapper);
  }

  // Turns iterable objects like lists into arrays, recursively to "depth" (default 1) deep.
  defineGlobalSymbol("to-array", to_array);
  function to_array(obj, ...rest) {
    let depth = 1;
    if (rest.length > 0 ) {
      let maybeDepth = rest[0];
      if (typeof maybeDepth === 'number')
        depth = maybeDepth;
    }
    if (depth <= 0) return obj;
    res = [];
    for (let item of obj) {
      if (depth > 1 && is_iterable(obj))
        value = to_array.call(this, item, depth-1);
      res.push(item);
    }
    return res;
  }

  //
  // S-epression parser
  //

  function* schemeTokenGenerator(characterGenerator) {
    if (!(typeof characterGenerator.next === 'function')) {
      if (is_iterable(characterGenerator)) {
        let generator = characterGenerator[Symbol.iterator]();
        if (!(typeof generator.next === 'function'))
          throw new LogicError(`Not an iterator or iterable ${characterGenerator}`);
        characterGenerator = generator;
      }
    }
    let ch = "", _peek = [], _done = false;
    function nextc() {
      if (_peek.length > 0)
        return ch = _peek.shift();
      if (_done) return ch = "";
      let next = characterGenerator.next();
      if (next.done) {
        _done = true;
        return ch = "";
      }
      return ch = next.value;
    }
    function peekc(n = 0) {
      for (let get = n - _peek.length + 1; get > 0; --get) {
        let next = characterGenerator.next();
        if (next.done) {
          _done = true;
          return "";
        }
        _peek.push(next.value);
      }
      return _peek[n];
    }
    nextc();

    while (ch) {
      while (WS[ch])
        nextc();

      if (NL[ch]) {
        yield { type: 'newline' };
        nextc();
        continue;
      }

      if (ch === ';' && peekc() === ';') {  // ;; begins a comment
        yield { type: 'newline' };
        while (ch && !NL[ch])
          nextc();
        continue;
      }

      if (ch === '"') {
        let str = "";
        nextc();
        while (ch && ch != '"' && !NL[ch]) {
          if (ch === '\\') {
            nextc();
            ch = ESCAPE_STRINGS[ch] ?? `\\${ch}`;
          }
          str += ch;
          nextc();
        }
        if (ch === '"') {
          yield { type: 'string', value: str };
          nextc();
        } else {
          yield { type: 'garbage', value: '"'+str };
          // Don't consume the newline or the REPL will prompt for another line of input
        }
        continue;
      }

      if (TOKS[ch]) {
        yield { type: ch };
        nextc();
        continue;
      }

      if (ch === "." && !DIGITS[peekc()]) {
        yield { type: ch };
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
            yield { type: 'number', value};
            continue;
          }
        }
      }

      if (IDENT1[ch]) {
        let str = "", operatorPrefix = OPERATORS[ch];
        while (ch && IDENT2[ch]) {
          // a prefix of operators breaks at a digit so we can write (*1 2)
          if (operatorPrefix && DIGITS[ch])
            break;
          // lambda symbols are special so we can parse \x as \ x
          if ((str[0] === '\\' || str[0] === LAMBDA_CHAR) && JSIDENT[ch])
            break;
          if (!OPERATORS[ch]) operatorPrefix = false;
          str += ch, nextc();
        }
        yield { type: 'ident', value: Atom(str) };
        continue;
      }

      if (!ch) break;
      yield { type: 'garbage', value: ch };
      nextc();
    }
    yield { type: 'end' };
  }

  let gensym_count = 0;
  const gensym = () => Symbol(`*gensym-${gensym_count++}*`);
  defineGlobalSymbol("gensym", gensym);

  defineGlobalSymbol("parse", parseSExpr);
  function parseSExpr(tokenGenerator, opts = {}) {
    opts = { ...schemeOpts, ...opts };
    let replHints = opts.replHints ?? {};
    if (typeof tokenGenerator === 'string')
      tokenGenerator = schemeTokenGenerator(tokenGenerator);
    if (!(typeof tokenGenerator.next === 'function')) {
      if (is_iterable(tokenGenerator)) {
        let generator = tokenGenerator[Symbol.iterator]();
        if (!(typeof generator.next === 'function'))
          throw new LogicError(`Not an iterator or iterable ${tokenGenerator}`);
        tokenGenerator = generator;
      }
    }
    let _toks = [], _done = false;
    let expr = parseExpr(0);
    let unparsed = unParesedInput();
    if (!unparsed)
      return expr;
    throw new ParseExtraTokens(unparsed);

    function parseExpr(parseDepth) {
      replHints.parseDepth = parseDepth;
      if (token().type === 'string' || token().type === 'number') {
        let thisToken = consumeToken();
        return thisToken.value;
      }

      if (token().type === 'ident') {
        let thisToken = consumeToken();
        return thisToken.value;
      }

      if (token().type === '(') {
        replHints.parseDepth = parseDepth + 1;
        consumeToken();
        return parseListBody();
        function parseListBody() {
          if (token().type === ')') {
            replHints.parseDepth = parseDepth;
            consumeToken();
            return NIL;
          } else if (token().type === '.') {
            consumeToken();
            let val = parseExpr(parseDepth + 1);
            if (token().type !== ')')
              throw new ParseExtraTokens(unParesedInput());
            consumeToken();
            return val;
          }
          if (token().type === 'end') throw new ParseIncomplete();
          if (token().type === 'garbage') throw new ParseExtraTokens(unParesedInput());
          let first = parseExpr(parseDepth + 1);
          let rest = parseListBody();
          return cons(first, rest);
        }
      }

      if (token().type === '[') {  // JavaScript Array
        let res = [];
        replHints.parseDepth = parseDepth + 1;
        consumeToken();
        for (;;) {
          if (token().type === ']') {
            consumeToken();
            return res;
          }
          let item = parseExpr(parseDepth + 1);
          res.push(item);
          if (token().type === ',')  // Comma might as well be optional for now
            consumeToken();
          if (token().type === 'end') throw new ParseIncomplete();
        }
      }

      if (token().type === '{') {  // JavaScript Object
        let res = {}, evalCount = 0;
        replHints.parseDepth = parseDepth + 1;
        consumeToken();
        for (;;) {
          if (token().type === '}') {
            consumeToken();
            break;
          }
          let gotIt = false;
          if (token().type === 'ident' || token().type === 'string'
                || token().type === 'number' || token().type === '[') {
            let evaluatedKey = false, sym;
            if (token().type === '[') {
              consumeToken();
              sym = parseExpr(parseDepth + 1);
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
              consumeToken();
              let val = parseExpr(parseDepth + 1);
              if (evaluatedKey)
                res[gensym] = new EvaluateKeyValue(sym, val);
              else
                res[sym] = val;
            }
            gotIt = true;
            if (token().type === ',')  // Comma might as well be optional for now
              consumeToken();
            if (token().type === 'end') throw new ParseIncomplete();
          }
          if (!gotIt)
            throw new ParseExtraTokens(unParesedInput());
        }
        return res;
      }

      if (token().type === "'") {
        consumeToken();
        let quoted = parseExpr(parseDepth + 1);
        replHints.parseDepth = parseDepth;
        return cons(QUOTE_ATOM, cons(quoted, NIL));
      }

      if (token(1).type === 'end')
        return null;
      throw new ParseExtraTokens(unParesedInput());
    }

    function token(n = 0) {
      // Two main ideas here, mostly in support of the REPL:
      // (1) Don't read anything until absolutely necessary.
      // (2) Foil attempts to peek for tokens across a line boundary by leaving
      //     'newline' tokens in the peek buffer. But to simplify the parser,
      //      token(0) skips past any newline tokens.
      for (;;) {
        for (let get = n - _toks.length; get >= 0 && !_done; --get) {
          let next = tokenGenerator.next();
          if (next.done)
            _done = true;
          else
            _toks.push(next.value);
        }
        if (n >= _toks.length)
          return { type: 'end'};
        let res = _toks[n];
        if (n > 0 || res.type !== 'newline')
          return res;
        // token(0) never returns a newline token
        while (_toks.length > 0 && _toks[0].type === 'newline')
          _toks.shift();
      }
    }

    function consumeToken() {
      return _toks.shift();
    }

    function unParesedInput() {
      let str = "", sep = "";
      while (_toks.length > 0) {
        let tok = _toks.shift();
        if (tok.type === 'newline' || tok.type === 'end')
          return str === '' ? null : str;
        str += sep + (tok.value !== undefined ? string(tok.value) : tok.type);
        sep = " ";
      }
      for (;;) {
        let { done, value: tok } = tokenGenerator.next();
        if (done) return str;
        if (tok.type === 'newline' || tok.type === 'end')
          return str;
        let val = tok.value;
        str += sep + (tok.value !== undefined ? string(tok.value) : tok.type);
        sep = " ";
      }
    }
  }

  exportAPI("analyzeJSFunction", analyzeJSFunction);
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
    let str = fn.toString(), pos = 0, token = "", pushbackToken = "";
    let name = fn.name, params = [], restParam, value, body, native = false, printBody;
    parse: {
      if (nextToken() === 'function') {
        if (nextToken() !== '(') {
          name = token;
          nextToken();
        }
        if (token !== '(') break parse;
        while (nextToken() && token !==')') {
          scarfOptional();
          if (token === ",") nextToken();
          if (token === "...")
            restParam = nextToken();
          else
            params.push(token);
        }
        let printBodyPos = pos;
        nextToken();
        parseBody();
        printBody = str.substr(printBodyPos);
      } else { // Arrow functions
        if (token === '(') {
          while (nextToken() && token !==')') {
            scarfOptional()
            if (token === ",") nextToken();
            if (token === "...")
              restParam = nextToken();
            else
              params.push(token);
          }
        } else {
          params.push(token);
        }
        if (nextToken() !== '=>') break parse;
        while (WSNL[str[pos]]) ++pos;
        let possibleValue = str.substr(pos);
        if (nextToken() === '{')
          parseBody();
        else
          value = possibleValue;
      }
    }
    let res = { name, params, restParam, value, body, printBody, native };
    analyzedFunctions.set(fn, res);
    return res;

    function scarfOptional() {
      // TODO: this is in no way adequate.
      if (token === "=") {
        nextToken();
        while (token) {
          if (token === "," || token === ")") break;
          nextToken();
        }
        pushbackToken = token;
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
        if (token === "return") {
          // If there's a return, it has to be followed by a variable,
          // an optional semicolon, an "}", and NOTHING FURTHER.
          // We capture the name of the return variable and clip the body
          // prior to the return.
          let possibleValue = nextToken();
          while (nextToken() === ';');
          if (token !== '}') return;
          if (nextToken() !== '') return;
          value = possibleValue;
          body = str.substring(bodyPos, returnPos);
        }
      }
    }

    function nextToken() {
      // Super janky tokenizer.
      // Most of what it returns is garbage, but it returns anything we actually care about.
      // The assumption is that what JS returns is well-formed, so it takes a lot of liberties.
      if (pushbackToken) {
        let ret = pushbackToken;
        pushbackToken = "";
        return ret;
      }
      if (pos >= str.length) return token = '';
      let ch = str[pos]; // ch is always str[pos]
      while (WSNL[ch]) ch = str[++pos];
      if (JSIDENT[ch]) {
        let tok = ch;
        ch = str[++pos];
        while (JSIDENT[ch]) {
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
        while (ch && ch != qc) {
          if (ch === '\\') ++pos;
          ch = str[++pos]
        }
        ++pos;
        return token = 'quoted';
      }
      return token = str[pos++] ?? '';
    }
  }

  const COMPILE_SENTINEL = Symbol("SENTINEL");

  // (compile (fn args) forms) -- defines a compiled function
  // (compile lambda) -- returns a compiled lambda expression
  defineGlobalSymbol("compile", compile, { evalArgs: 0 });
  function compile(nameAndParams, forms, _) {
    if (!is_cons(nameAndParams)) new EvalError(`First parameter must be a list`);
    let name = Atom(nameAndParams[CAR]);
    let args = nameAndParams[CDR];
    if (typeof name !== 'symbol') new EvalError(`Function name must be an atom or string`)    
    let form = list(LAMBDA_ATOM, args, forms);
    let compiledFunction = compile_lambda.call(this, name, form);
    globalScope[name] = compiledFunction;
    return name;
  }

  defineGlobalSymbol("compile-lambda", compile_lambda);
  function compile_lambda(name, lambda) {
    let compiled = lambda_compiler.call(this, name, lambda);
    let code = car(compiled), bindSymToObj = cdr(compiled);
    let binder = new Function("bound", code);
    let compiledFunction = binder(bindSymToObj);
    return compiledFunction;
  }

  defineGlobalSymbol("lambda-compiler", lambda_compiler);
  function lambda_compiler(name, lambda) {
    name = Atom(name);
    // Prevent a tragic mistake that's easy to make by accident. (Ask me how I know.)
    if (name === QUOTE_ATOM) throw new EvalError("Can't redefine quote");

    let scope = newScope(this);
    // If you're compiling a dunction that's already been defined, this prevents
    // the symbol from resolving to the old definition. It also serves as a
    // sentinel to compileApply that it's gotten back around to where it started.
    scope[name] = COMPILE_SENTINEL;

    let bindSymToObj = {}, bindObjToSym = new Map(), tempNames = {}, varNum = 0, emitted = [];
    let tools = { bind, boundVal, emit, newTemp, scope, indent: '', evalLimit: 100 };

    let compileScope = new Scope();
    let nameStr = newTemp(name);
    tools.functionName = nameStr;
    let stringStr = bind(string, "string");
    let evalErrorStr = bind(EvalError, "EvalError");
    bind(NIL, "NIL");
    bind(bool, "bool");
    bind(cons, "cons");
    bind(car, "car");
    bind(cdr, "cdr");
    emit(`function outsideScope(this_, x) {`);
    emit(`  let val = this_[x];`);
    emit(`  if (val === undefined) throw new ${evalErrorStr}("Undefined symbol " + ${stringStr}(x));`);
    emit(`  return val;`);
    emit(`}`);
    compileLambda(nameStr, lambda, compileScope, tools);
    emit(`return ${nameStr};`);
    let saveEmitted = emitted;
    emitted = [];
    for (let bindingName of Object.keys(bindSymToObj))
      emit(`let ${bindingName} = bound[${string(bindingName)}];`);
    emitted = emitted.concat(saveEmitted);
    let code = emitted.join('');
    console.log("COMPILED", code);
    return cons(code, bindSymToObj);

    function bind(obj, name) {
      if (obj === undefined) return "undefined";
      if (obj === null) return "null";
      let boundSym = bindObjToSym.get(obj);
      if (boundSym) return boundSym;
      if (name) {
        if (typeof name === 'symbol')
          name = newTemp(name.description);
        if (bindSymToObj[name])  // collision
          name = newTemp(name);
      } else {
        if (typeof obj === 'symbol')
          name = newTemp(obj.description);
        else if (typeof obj == 'function')
          name = newTemp(obj.name);
        else
          name = newTemp();
      }
      bindSymToObj[name] = obj;
      bindObjToSym.set(obj, name);
      return name;
    }
    function emit(str) {
      emitted.push(tools.indent + str + '\n');
    }
    function boundVal(name) {
      return typeof name === 'string' && bindSymToObj[name];
    }
    function newTemp(name) {
      if (!name || name === '')
        name = 'tmp';
      name = toJSname(name);
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
  }
  function compileEval(form, compileScope, tools) {
    if (--tools.evalLimit < 0)
      throw new CompileError(`Too comlpex ${string(form)}`);
    let result;
    if (form === undefined) {
      result = "undefined";
    } else if (form === null) {
      result = "null";
    } else if (typeof form === 'number' || typeof form === 'bigint' || typeof form === 'string') {
      result = string(form);
    } else if (form === NIL) {
      result = "NIL";
    } else if (typeof form === 'symbol') {
      let sym = form;
      result = compileScope[sym];
      if (!result) {
        let scopedVal = tools.scope[sym];
        if (scopedVal === COMPILE_SENTINEL) {
          result = scopedVal;
        } else if (scopedVal) {
          result = tools.bind(scopedVal, sym);
        } else {
          let bound = tools.bind(sym);
          result = `outsideScope(this, ${bound})`;
        }
      }
    } else if (is_cons(form)) {
      let fn = form[CAR], args = form[CDR];
      if (fn === QUOTE_ATOM) {
        result = tools.bind(form[CDR][CAR]);
      } {
        if (is_cons(fn)) {
          let fnCar = fn[CAR];
          if (!(fnCar === LAMBDA_ATOM || fnCar === SLAMBDA_ATOM)) {
            ({ result, code } = compileApply(form, compileScope, tools));
          }
        } else {
          let evalResult = compileEval(fn, compileScope, tools);
          result = compileApply(evalResult, args, compileScope, tools);
        }
      }
    } else {
      // TODO: handle array and object literals
      result = tools.bind(form);
    }
    return result;
  }
  
  function compileApply(form, args, compileScope, tools) {
    let paramCount = 0, evalCount = MAX_INTEGER;
    let name, params, restParam, value, body, hook;
    let saveIndent = tools.indent
    let boundVal = tools.boundVal(form);
    if (boundVal) form = boundVal;
    if (typeof form === 'function') { // form equals function :)
      hook = form[COMPILE_HOOK];
      ({ name, params, restParam, value, body } = analyzeJSFunction(form));
      if (restParam)  // rest-param functions need a hook
        value = body = undefined;
      let functionDescriptor = form[FUNCTION_DESCRIPTOR_SYMBOL] ?? 0;
      evalCount = ~functionDescriptor >> 7 >>> 1;
      paramCount = params.length;
      if (evalCount !== MAX_INTEGER)
        paramCount -= 1;
      if (name === '') name = undefined;
    } else if (is_cons(form)) {
      let opSym = form[CAR];
      if (opSym === SLAMBDA_ATOM) {
        evalCount = 0;
      }
      // Compile closures?
      params = [];
      if (!is_cons(form[CDR])) throw new EvalError(`Bad lambda ${string(form)}`);
      if (typeof form[CDR] === 'symbol') {
        params.push(form[CDR])
      } else {
        for (let params = form[CDR[CAR]]; is_cons(params); params = params[CDR])
          params.push(params[CAR]);
      }
      paramCount = params.length;
    }
    // Materialize the arguments in an array
    let lift = evalCount > paramCount ? evalCount : paramCount;
    let result = tools.newTemp(name);
    let argv = [];
    for (let i = 0; is_cons(args); ++i) {
      if (i < evalCount) {
        let evalResult = compileEval(args[CAR], compileScope, tools);
        argv.push(evalResult);
      } else {
        argv.push(args[CAR]);
      }
      args = args[CDR];
    }
    if (hook) {
      // TODO: partial application
      result = hook(argv, compileScope, tools);
    } else if (form === COMPILE_SENTINEL || (typeof form === 'function' && !value)) {
      // No template: going to have to call it after all.
      let fname, argvStr = '';
      if (form === COMPILE_SENTINEL)
        fname = tools.functionName;
      else
        fname = tools.bind(form);
      for (let arg of argv)
        argvStr += ', ' + arg;
      tools.emit(`${result} = ${fname}.call(this${argvStr});`);
      return result;
    } else {
      tools.emit(`let ${result}; {`);
      tools.indent = saveIndent + "  ";
      let i = 0;
      while (i < lift) {
        if (i < argv.length) {
          tools.emit(`let ${params[i]} = ${argv[i]};`);
          ++i;
        } else {
          if (i < paramCount) {
            if (i < 1) {
              tools.emit(`let ${params[i]} = undefined;`);
              ++i;
              break;
            }
          }
          // TODO: partial application of builtin; create closure somehow
        }
        break;
      }
      if (evalCount !== MAX_INTEGER) {
        tools.emit(`let ${params[i]} = NIL;`);
        for (let j = args.length; j > i; --j)
          tools.emit(`${params[i]} = cons(${argv[j]}, ${params[i]};`);
      } else if (typeof form === 'function') {
        if (body)
          tools.emit(body); 
        tools.emit(`${result} = (${value});`);
        tools.indent = saveIndent;
        tools.emit(`}`);
        return result;
      } else if (is_cons(form)) {
        let opSym = form[CAR];
        let body = form[CDR];
        if (!is_cons(body)) throw new EvalError(`Bad form ${string(form)}`);
        if (opSym === LAMBDA_ATOM || opSym === SLAMBDA_ATOM) {
          // I don't expect to compile closures but maybe there's a reason to do so?
          let lambda = compileLambda('', form, compileScope, tools);
          let argStr = '';
          for (arg of argv)
            argStr += ', ' + arg;
          tools.emit(`${result} = ${lambda}.call(this${argStr});`);
        }
      }
    }
    return result;
  }

  function compileLambda(name, form, compileScope, tools) {
    if (!is_cons(form)) throw new EvalError(`Bad lambda ${string(form)}`);
    let body = form[CDR];
    if (!is_cons(body)) throw new EvalError(`Bad lambda ${string(form)}`);
    let params = body[CAR];
    let forms = body[CDR];
    let paramv = [];
    let result;
    compileScope = newScope(compileScope, "compile-lambda-scope");
    if (typeof params === 'symbol') { // Curry notation :)
      let paramVar = tools.newTemp(params);
      paramv.push(paramVar);
      compileScope[params] = paramVar;
      forms = cons(forms, NIL);
    }
    let origFormalParams = params;
    while (is_cons(params)) {
      let param = params[CAR];
      if (typeof param !== 'symbol') throw new EvalError(`Param must be a symbol ${param}`);
      let paramVar = tools.newTemp(param);
      paramv.push(paramVar);
      compileScope[param] = paramVar;
      params = params[CDR];
    }
    if (typeof params === 'symbol') {
      // Hmmm... 'rest' params? This is probably not quite right.
      let paramVar = tools.newTemp(params);
      paramv.push(paramVar);
      compileScope[params] = paramVar;
    }
    else if (params !== NIL) {
      throw new EvalError(`Bad parameter list ${string(origFormalParams)}`);
    }
    if (name && name !== '')
      result = name;
    else
      result = tools.newTemp();
    let delim = '', paramStr = '', saveIndent = tools.indent;
    for (let param of paramv) {
      paramStr += delim + param;
      delim = ', ';
    }
    tools.emit(`function ${result}(${paramStr}) {`);
    tools.indent = saveIndent + "  ";
    let res = NIL;
    while (is_cons(forms)) {
      result = compileEval(forms[CAR], compileScope, tools);
      forms = forms[CDR];
    }
    if (result)
      tools.emit(`return ${result};`);
    else
      tools.emit(`return NIL};`);
    tools.indent = saveIndent;
    tools.emit(`}`);
    return result;
  }
  
  const JS_IDENT_REPLACEMENTS  = {
    '~': '$tilde', '!': '$bang', '@': '$at', '#': '$hash', '$': '$cash', '%': '$pct', '^': '$hat',
    '&': '$and', '|': '$or', '*': '$star', '+': '$plus', '-': '$dash', '=': '$eq', '<': '$lt',
    '>': '$gt', '/': '$stroke', '\\': '$bs', '?': '$q'
  };

  exportAPI("toJSname", toJSname);
  function toJSname(name) {
    if (typeof name === 'symbol')
      name = name.description;
    let newName = "", fragment = "";
    for (let ch of name) {
      if (JS_IDENT_REPLACEMENTS[ch]) {
        newName += fragment + JS_IDENT_REPLACEMENTS[ch];
        fragment = "";
      } else if (JSIDENT[ch]) {
        if (!fragment) fragment = "_";
        fragment += ch;
      } else {
        newName += fragment + '$x' + ch.codePointAt(0).toString(16);
        fragment = "";
      }
    }
    newName += fragment;
    return newName;
  }

  let quitRepl = false;
  const quit = _ => quitRepl = true;
  defineGlobalSymbol("quit", quit);

  defineGlobalSymbol("REPL", REPL);
  function REPL(readline, opts = {}) {
    let scope = this;
    opts = { ...schemeOpts, ...opts };
    // readline(parseDepth) => str | nullish
    let name = opts.name ?? "SchemeJS";
    let print = opts.print ?? (x => console.log(string(x)));
    let reportSchemeError = opts.reportSchemeError ?? (x => console.log(String(x)));;
    let reportSystemError = opts.reportSystemError ?? (x => console.log(name + " internal error:", String(x), x));;
    let replHints = { };
    let endTest = opts.endTest ?? (_ => false);
    quitRepl = false;
    function* charStreamPromptInput() {
      for(;;) {
        let line = readline(replHints.parseDepth);
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
    let tokenGenerator = schemeTokenGenerator(charStream);
    while (!quitRepl) {
      try {
        let expr = parseSExpr(tokenGenerator, { ...opts, replHints });
        if (!expr) continue;
        let evaluated = _eval(expr, scope);
        print (evaluated);
      } catch (error) {
        if (error instanceof SchemeJSError)
          reportSchemeError(error);
        else
          reportSystemError(error);
      }
    }
  }

  exportAPI("_setGlobalScope_test_hook_", _setGlobalScope_test_hook_)
  function _setGlobalScope_test_hook_(scope) {
    let previousGlobalScope = globalScope;
    globalScope = scope;
    return previousGlobalScope;
  }

  // Tiny unit test system.
  // I need something special here because the internal functions are not accessible
  // externally. I also just like the idea of the tests being with the code itself.
  // But I'll separate them when I finally make this a JavaScript module.

  class TestFailureError extends SchemeJSError {
    constructor(message, test, result, expected) {
      super(`${string(test)}; ${message}: ${string(result)}, expected: ${string(expected)}`);
      this.test = test;
      this.result = result;
      this.expected = expected;
    }
  }
  TestFailureError.prototype.name = "TestFailureError";

  function testFailed(message, test, result, expected, report) {
    console.info("FAILED", test, result, expected, report);
    throw new TestFailureError(message, test, result, expected);
  }

  function testSucceeded(test, result, expected) {
    console.info("SUCCEEDED", test, result, expected);
  }

  let testScopeStack = [], originalGlobalScope = globalScope;
  
  function SAVESCOPE() {
    // The idea here is to let the following series of steps to share a scope
    // Otherwise, each test gets a fresh scope.
    testScopeStack.push(globalScope);
    globalScope = newScope(globalScope, "test-global-scope");
    globalScope.GlobalScope = globalScope;  // A damn lie
  }

  function RESTORESCOPE() {
    let scope = testScopeStack.pop();
    if (!scope) throw new LogicError("Test scope push and pop not paired");
    globalScope = scope;
  }

  function EXPECT(test, expected) {
    let pushed = false;
    if (globalScope === originalGlobalScope) {
      SAVESCOPE();
      pushed = true;
    }
    try {
      let result, ok, report = {};
      try {
        if (typeof test === 'string') {
        result = globalScope.evalString(test);
          if (typeof expected === 'string')
            expected = globalScope.evalString(expected);
        } else {
          result = test.call(globalScope);
        }
      } catch (error) {
        reportTestFailed("exception", test, error, expected);
        return;
      }
      if (typeof expected === 'function')
        ok = expected.call(globalScope, result);
      else
        ok = deep_eq(result, expected, 100, report);
      if (!ok)
        reportTestFailed("got", test, result, expected, report);
      else
        reportTestSucceeded(test, result, expected);
    } finally {
      if (pushed) RESTORESCOPE();
    }
  }

  function EXPECT_ERROR(test, expected) {
    let result, pushed = false;
    if (globalScope === originalGlobalScope) {
      SAVESCOPE();
      pushed = true;
    }
    try {
      if (typeof test === 'string') {
        result = globalScope.evalString(test);
      } else {
        result = test.call(globalScope);
      }
    } catch (error) {
      if (typeof test === 'string' && typeof expected === 'string')
        expected = globalScope.evalString(expected);
      if (error === expected || (error instanceof expected)) {
        reportTestSucceeded(test, error, expected);
      } else {
        reportTestFailed("wrong exception", test, error, expected);
      }
      return;
    } finally {
      if (pushed) RESTORESCOPE();
    }
    reportTestFailed("expected exception", test, result, expected);
  }

  return globalScope;
} // Whew!