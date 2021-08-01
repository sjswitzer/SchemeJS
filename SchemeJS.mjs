//
// SchemeJS: Scheme in JavaScript
//
// Copyright 2021 Stan Switzer
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

export const VERSION = "1.1 (beta)";

//
// Creates a SchemeJS instance, independent of any others.
//
// Instances are distinct to the bones; they do not even recognize each other's
// Cons cells or NIL values. This is by design. People should be able
// to poke things into class definitions to experiment with different ideas
// but that should only affect that specific SchemeJS instance; others should
// be unaffected.
//
export function createInstance(schemeOpts = {}) {
  let readFile = schemeOpts.readFile;
  let latin1 = schemeOpts.latin1 ?? false;
  let supplemental = schemeOpts.supplemental ?? false;
  let dumpIdentifierMap = schemeOpts.dumpIdentifierMap ?? false;
  let _reportError = schemeOpts.reportError = error => console.log(error); // Don't call this one
  let reportSchemeError = schemeOpts.reportSchemeError ?? _reportError; // Call these instead
  let reportSystemError = schemeOpts.reportError ?? _reportError;
  let reportLoadResult = schemeOpts.reportLoadResult ?? (result => console.log(string(result)));
  let lambdaStr = schemeOpts.lambdaStr ?? "\\";
  let slambdaStr = schemeOpts.lsambdaStr ?? "\\\\";

  // Creating a Cons should be as cheap as possible, so no subclassing
  // or calls to super. But people should be able to be able to define their
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

  const CAR = Symbol("CAR"), CDR = Symbol("CDR");
  const PAIR = Symbol("PAIR"), LIST = Symbol("LIST"), NULLSYM = Symbol("NULLSYM");
  const LAZYCAR = Symbol("LAZYCAR"), LAZYCDR = Symbol("LAZYCDR"), SUPERLAZY = Symbol("SUPERLAZY");
  const COMPILED = Symbol('COMPILED');

  // Trust the JIT to inline this
  const is_cons = obj => obj != null && obj[PAIR] === true;
  const is_null = obj => obj != null && obj[NULLSYM] === true;
  const is_list = obj => obj != null && obj[LIST] === true;

  class Cons {
    [CAR]; [CDR];
    constructor(car, cdr) {
      this[CAR] = car;
      this[CDR] = cdr;
    }
    toString() { return string(this); }
    [Symbol.iterator] = pairIterator;
    // static [PAIR] = true;  // Hmm; Shouldn't this work?
  }
  Cons.prototype[PAIR] = true;
  Cons.prototype[LIST] = true;

  function pairIterator() {
    let current = this;
    return {
      next() {
        if (!is_cons(current))
        return { done: true, value: current };  // value is whatever wasn't a cons cell
        let value = current[CAR];
        current = current[CDR];
        return { done: false, value };
      },
      // So that the iterator itself is iterable, with a fresh iterator at the current position
      [Symbol.iterator]() { return pairIterator.call(current); }
    }
  }
  
  // Hide the NIL class because there's never any reason to
  // reference it or to instantiate it it more than once. Leaving it visible
  // just invites errors. But it's good to have a distinct class for NIL
  // for various reasons including that it looks better in a JS debugger
  // and provides a way to trap attempts to get or set [CAR] and [CDR].
  const NIL = new ((() => {
    let nilClass = class NIL {
      [Symbol.iterator]() { return { next: () => ({ done: true }) } }
      get [CAR]() { throw new SchemeEvalError("car of nil") }
      set [CAR](_) { throw new SchemeEvalError("set car of nil") }
      get [CDR]() { throw new SchemeEvalError("cdr of nil") }
      set [CDR](_) { throw new SchemeEvalError("set cdr of nil") }
      [NULLSYM] = true;  // probably a _tiny_ bit faster if defined here too
      [LIST] = true;
    }
    nilClass.prototype[NULLSYM] = true;
    nilClass.prototype[LIST] = true;
    return nilClass;
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
    if (typeof name !== 'string')
      throw new SchemeEvalError(`Not a string ${name}`);
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

  function iteratorFor(obj, throwException) {
    if (obj != null) {
      if (typeof obj[Symbol.iterator === 'function']) return obj[Symbol.iterator]();
      if (typeof obj.next === 'function') return obj;
    }
    if (throwException) throw new throwException(`Not an iterable or list ${obj}`);
  }

  //
  // Character clases for parsing
  //
  const NBSP = '\u00a0', VTAB = '\u000b', FORMFEED = '\u000c', ELIPSIS = '\u0085', MUL = '\u00d7', DIV = '\u00f7';
  const NL = {}, SINGLE_CHAR_TOKENS = {}, QUOTES = {}, DIGITS = {}, NUM1 = {}, NUM2 = {}, JSIDENT = {};
  // IDENT2 includes IDENT1 by inheritence, as does WSNL WS.
  const IDENT1 = {}, IDENT2 = Object.create(IDENT1), WS = {}, WSNL = Object.create(WS);
  for (let ch of `abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$`)
    JSIDENT[ch] = ch.codePointAt(0);
  for (let ch of `0123456789`)
    DIGITS[ch] = IDENT2[ch] = NUM1[ch] = NUM2[ch] = JSIDENT[ch] = ch.codePointAt(0);
  for (let ch of `+-.`)
    NUM1[ch] = NUM2[ch] = ch.codePointAt(0);
  for (let ch of `eEoOxXbBn`)
    NUM2[ch] = ch.codePointAt(0);
  for (let ch of ` \t${VTAB}${FORMFEED}${NBSP}`) WS[ch] = ch.codePointAt(0);
  for (let ch of `\n\r`) NL[ch] = WSNL[ch] = ch.codePointAt(0);
  for (let ch of `()[]{}'.:`) SINGLE_CHAR_TOKENS[ch] = ch.codePointAt(0);
  for (let ch of `\`'"`) QUOTES[ch] = ch.codePointAt(0);

  // Digs the Unicode character properties out of the RegExp engine
  // This can take a bit of time and a LOT of memory, but people should
  // be able to use their own languages. By default it includes the
  // the Basic Multilingual Plane, but you can option it down to Latin-1
  // or up to include all the supplemental planes.
  // In addition to the memory used by the table I suspect the RegExp engine
  // drags in some libraries dynamically when the "u" flag is specified.
  // And for that metter using RegExp at all probably drags in a dynammic library
  // so, to reduce memory footprint, don't use it for Latin-1.

  // Basic Latin (ASCII)
  for (let codePoint = 0x21; codePoint < 0xff; ++codePoint) {
    let ch = String.fromCodePoint(codePoint)
     // All printable charactes except single-char tokens, digits and quotes
    if (!SINGLE_CHAR_TOKENS[ch] && !DIGITS[ch] && !QUOTES[ch])
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

  //
  // Everything is encapsulated in a function scope because JITs
  // should be able to resolve lexically-scoped references most efficiently.
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
  globalScope._help_ = {};  // For clients that want to implement help.

  exportAPI("defineGlobalSymbol", defineGlobalSymbol);
  function defineGlobalSymbol(name, value, ...aliases) {
    let opts = {};
    if (typeof aliases[0] === 'object')
      opts = aliases.shift();
    if (typeof value === 'function') {
      let evalCount = opts.evalArgs ?? MAX_INTEGER;
      createFunctionDescriptor(value, evalCount);
      if (opts.compileHook) value[COMPILE_HOOK] = opts.compileHook;
    }
    let group = opts.group ?? "builtin";
    let atom;
    ({ atom, name } = normalize(name));
    globalScope[atom] = value;
    if (!opts.schemeOnly)
      globalScope[name] = value;
    let atoms = [ atom ];
    globalScope._help_[atom] = { atoms, value, group };
    for (let alias of aliases) {
      ({ atom, name } = normalize(alias));
      globalScope[atom] = value;
      atoms.push(atom);
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

  function createFunctionDescriptor(fn, evalCount = MAX_INTEGER) {
    let fnInfo = analyzeJSFunction(fn);
    let paramCount = fnInfo.params.length;
    let restParam = 0;
    if (fnInfo.restParam) {
      if (evalCount !== MAX_INTEGER)
        throw new LogicError(`Can't have rest params with special evaluation ${name}`)
        restParam = 0x8000;
    }
    // If this function doesn't evaluate all of its parameters, the last parameter
    // recieves the unevaluated forms, so don't count that as a normal one.
    if (evalCount !== MAX_INTEGER)
      paramCount -= 1;
    if (paramCount >= 0x7fff) throw new LogicError("Too many params");
    if (fnInfo.native) paramCount = MAX_INTEGER;
    // Encoding chosen so that small values mean eval everything and lift that many.
    let functionDescriptor = (~evalCount << 16) | (paramCount & 0x7fff) | restParam;
    fn[FUNCTION_DESCRIPTOR_SYMBOL] = functionDescriptor;
    return functionDescriptor;
  }

  exportAPI("PAIR_SYMBOL", PAIR);
  exportAPI("LIST_SYMBOL", LIST);
  exportAPI("CAR_SYMBOL", CAR);
  exportAPI("CDR_SYMBOL", CDR);
  exportAPI("LAZYCAR_SYMBOL", LAZYCAR);
  exportAPI("LAZYCDR_SYMBOL", LAZYCDR);
  exportAPI("SUPERLAZY_SYMBOL", SUPERLAZY);

  defineGlobalSymbol("VERSION", VERSION);
  defineGlobalSymbol("is-atom", is_atom, "atom?");
  defineGlobalSymbol("intern", Atom);
  exportAPI("Atom", Atom);

  class SchemeError extends Error {};
  SchemeError.prototype.name = "SchemeError";
  defineGlobalSymbol("SchemeError", SchemeError);

  class SchemeEvalError extends SchemeError {};
  SchemeEvalError.prototype.name = "SchemeEvalError";
  defineGlobalSymbol("SchemeEvalError", SchemeEvalError);

  class SchemeCompileError extends SchemeError {};
  SchemeCompileError.prototype.name = "SchemeCompileError";
  defineGlobalSymbol("SchemeCompileError", SchemeCompileError);

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

  class SchemeParseIncomplete extends SchemeParseError {
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
  SchemeParseIncomplete.prototype.name = "SchemeParseIncomplete";
  defineGlobalSymbol("SchemeParseIncomplete", SchemeParseIncomplete);

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
  // Written this way so that actual bools do not need to be compared to NIL; most of this
  // is just a tag-bit compare in the runtime.
  //
  // I'm likely to revisit this. Differet Schemes and Lisps have different policies
  // here. What I'd like to do is define bool in a way that the JIT can trivially evaluate.
  // In particular, treating NIL as false is expensive.
  //
  const bool = a => a === true || (a !== false && a != null && !is_null(a));
  exportAPI("bool", bool);

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
  defineGlobalSymbol("true", true, "t", "#t"); // SIOD: t, MIT Scheme: #t
  defineGlobalSymbol("false", false);
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
  defineGlobalSymbol("pair?", is_cons, "is-cons");
  defineGlobalSymbol("list?", is_list, "is-list");
  defineGlobalSymbol("null?", a => is_null(a));  // SIOD clained it first. Maybe rethink the naming here.
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
  defineGlobalSymbol("nan?", isNaN, { schemeOnly: true } , "isNaN", "NaN?");
  defineGlobalSymbol("finite?", isFinite, { schemeOnly: true } , "isFinite");

  // Hoist a bunch of JavaScript definitions into the global scope
  defineGlobalSymbol("NaN", NaN);
  defineGlobalSymbol("Infinity", Infinity);

  { // (Local scope so we don't hang onto the property descriptors)
    // Import global JavaScript symbols
    let propDescs = Object.getOwnPropertyDescriptors(globalThis);
    for (let name in propDescs) {
      let {value, get} = propDescs[name];
      if (!get && value)
        defineGlobalSymbol(name, value, { schemeOnly: true, group: "imported" });
    }

    // Stuff the whole Math class in there!
    defineGlobalSymbol("Math", Math, { schemeOnly: true });
    propDescs = Object.getOwnPropertyDescriptors(Math);
    for (let name in propDescs) {
      let {value, get} = propDescs[name];
      if (!get && value) {
        // SIOD defines *pi* so I'll just define them all like that
        if (typeof value === 'number')
          defineGlobalSymbol(`*${name.toLowerCase()}*`, value, { schemeOnly: true });
        // SIOD defines sin, cos, asin, etc. so I'll just define them all like that
        if (typeof value === 'function')
          defineGlobalSymbol(name, value, { schemeOnly: true });
      }
    }
    defineGlobalSymbol("abs", a => a < 0 ? -a : a);  // Overwrite Math.abs; this deals with BigInt too
    // This is redundant but I want them defined in the "builtin" group.
    defineGlobalSymbol("globalThis", globalThis);
    for (let obj of [Object, Boolean, Symbol, Number, String, BigInt, Array])
      defineGlobalSymbol(obj.name, obj);
    
  }

  defineGlobalSymbol("intern", a => Atom(a));
  defineGlobalSymbol("eval", eval_);
  function eval_(expr, ...scope) { // Javascript practically treats "eval" as a keyword
    let useScope = scope[0];
    if (!(useScope != null && useScope instanceof Scope)) useScope = this;
    if (is_null(useScope)) scope = globalScope; // NIL, specifically, means use the global scope
    return _eval(expr, useScope);
  }

  defineGlobalSymbol("eval-string", eval_string);
  function eval_string(str, ...scope) {
    let useScope = scope[0];
    if (!(useScope != null && useScope instanceof Scope)) useScope = this;
    if (is_null(useScope)) scope = globalScope; // NIL, specifically, means use the global scope
    let expr = parseSExpr(str);
    return eval_.call(this, expr, useScope);
  }

  defineGlobalSymbol("globalScope", globalScope);

  defineGlobalSymbol("apply", apply);
  function apply(fn, args, ...scope) {
    let useScope = scope[0];
    if (!(useScope != null && useScope instanceof Scope)) useScope = this;
    if (is_null(useScope)) scope = globalScope; // NIL, specifically, means use the global scope
    return _apply(fn, args, useScope);
  }

  // Pokemon gotta catch 'em' all!
  defineGlobalSymbol("not", a => !bool(a), "!");
  defineGlobalSymbol("bit-not", a => ~a, "~");
  defineGlobalSymbol("pow", (a,b) => a ** b, "**");  // overrides Math.pow
  defineGlobalSymbol("rem", (a,b) => a % b, "%");
  defineGlobalSymbol("bit-shl", (a,b) => a << b, "<<");
  defineGlobalSymbol("bit-shr", (a,b) => a >> b, ">>");
  defineGlobalSymbol("bit-ushr", (a,b) => a >>> b, ">>>");
  const ash = (a, b) => b < 0 ? a >>> -b : a << b;
  defineGlobalSymbol("ash", ash);  // SIOD
  defineGlobalSymbol("in", (a,b) => a in b);
  defineGlobalSymbol("new", (cls, ...args) => new cls(...args));
  defineGlobalSymbol("instanceof", (a,b) => a instanceof b);
  defineGlobalSymbol("@", (a, b) => a[b], "aref");  // indexing and member access (SIOD: aref)
  defineGlobalSymbol("@?", (a, b) => a?.[b], "aref?");  // conditional indexing and member access
  defineGlobalSymbol("@!", (a, b, ...params) => a[b](...params), "js-call");
  defineGlobalSymbol("@?!", (a, b, ...params) => a?.[b](...params), "js-call?");
  defineGlobalSymbol("@=", (a, b, c) => a[b] = c, "js-assign");
  defineGlobalSymbol("delete", (a, b) => delete a[b]);
  defineGlobalSymbol("void", _ => undefined);

  //
  // Variable args definitions
  //
  defineGlobalSymbol( "add", add, { compileHook: add_hook }, "+");
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

  defineGlobalSymbol("sub", sub, { compileHook: sub_hook}, "-");
  function sub(a, ...rest) {
    if (rest.length === 0) return -a;
    for (let b of rest)
      a -= b;
    return a;
  }
  function sub_hook(args, compileScope, tools) {
    if (args.length === 1)
      return `(-${args[0]})`;
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` - ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("mul", mul, { compileHook: mul_hook}, "*", MUL);
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

  defineGlobalSymbol("div", div, { compileHook: div_hook }, '/', DIV);
  function div(a, ...rest) {
    if (rest.length === 0) return 1/a;
    for (let b of rest)
      a /= b;
    return a;
  }
  function div_hook(args, compileScope, tools) {
    if (args.length === 1)
      return `(1 / ${args[0]})`;
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` / ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("bit-and", bit_and, { compileHook: bit_and_hook }, "&");
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

  defineGlobalSymbol("bit-or", bit_or, { compileHook: bit_or_hook }, "|");
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

  defineGlobalSymbol("bit-xor", bit_xor, { compileHook: bit_xor_hook}, "^");
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

  defineGlobalSymbol("lt", lt, { evalArgs: 2, compileHook: lt_hook }, "<");
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
    if (args.length === 2)
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

  defineGlobalSymbol("le", le, { evalArgs: 2, compileHook: le_hook }, "<=");
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

  defineGlobalSymbol("gt", gt, { evalArgs: 2, compileHook: gt_hook }, ">");
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

  defineGlobalSymbol("ge", ge, { evalArgs: 2, compileHook: ge_hook }, ">=");
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

  defineGlobalSymbol("eqv", eq, { evalArgs: 2, compileHook: eq_hook }, "=="); // XXX name
  function eq(a, b, forms) {
    if (forms === undefined) return a == b;   // TODO == or ===?
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

  // XXX define === and !==;  "eq?" is ===
  // is equal? really a deep compare? 

  // Sorry, "equal?"" does not get the variadic treatment at this time
  const equalp = (a, b) =>  deep_eq(a, b);
  defineGlobalSymbol("equal?", equalp);

  defineGlobalSymbol("ne", ne, { evalArgs: 2, compileHook: ne_hook }, "!=");
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

  // Logical & Conditional

  defineGlobalSymbol("and", and, { evalArgs: 1 }, "&&");
  function and(val, forms) {
    while (bool(val) && is_cons(forms)) {
      val = _eval(forms[CAR], this);
      forms = forms[CDR];
    }
    return val;
  }

  defineGlobalSymbol("or", or, { evalArgs: 1 }, "||");
  function or(val, forms) {
    while (!bool(val) && is_cons(forms)) {
      val = _eval(forms[CAR], this);
      forms = forms[CDR];
    }
    return val;
  }

  defineGlobalSymbol("if", ifelse, { evalArgs: 1, compileHook: ifelse_hook }, "?");
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
        throw new SchemeEvalError(`Bad clause in "cond" ${string(clause)}`);
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
    let noEval = rest.length > 0 && bool(rest[0]);
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
    for(;;) {
      try {
        let expr = parseSExpr(characterGenerator, { path });
        if (!expr) break;
        if (noEval) {
          if (last) last = last[CDR] = cons(expr, NIL);
          else result = last = cons(expr, NIL);
        } else {
          let evaluated = _eval(expr, scope);
          reportLoadResult(evaluated);
        }
      } catch (error) {
        if (error instanceof SchemeError)
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
      if (is_list(list)) {
        // Could handle as iterable, but faster not to
        while (is_cons(list)) {
          if (last) last = last[CDR] = cons(list[CAR], NIL);
          else res = last = cons(list[CAR], NIL);
          list = list[CDR];
        }
      } else {
        if (!is_iterable(list)) throw new SchemeEvalError(`Not a list or iterable ${list}`);
        for (let value of list) {
          let item = cons(value, NIL);
          if (last) last = last[CDR] = item;
          else res = last = item;
        }
      }
    }
    return res;
  }

  defineGlobalSymbol("last", last);
  function last(list) {
    let res = NIL;
    if (!list || is_null(list)) return NIL; // XXX check this.
    if (is_cons(list)) {
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
      if (!is_iterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
      for (let value of list)
        res = value;
    }
    return res;
  }

  defineGlobalSymbol("butlast", butlast);
  function butlast(list) {
    let res = NIL, last;
    if (is_list(list)) {
      while (is_cons(list) && is_cons(list[CDR])) {
        if (last) last = last[CDR] = cons(list[CAR], NIL);
        else res = last = cons(list[CAR], NIL);
        list = list[CDR];
      }
    } else {
      if (!is_iterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
      for (let value of list) {
        let item = cons(value, NIL);
        if (!first)
          if (last) last = last[CDR] = item;
          else res = last = item;
        first = false;
      }
    }
    return res;
  }

  defineGlobalSymbol("length", length);
  function length(list) {
    let n = 0;
    if (is_list(list)) {
      while (is_cons(list)) {
        n += 1;
        list = list[CDR];
      }
    } else {
      // Don't special-case string. Its iterator returns code points by combining surrogate pairs
      if (Array.isArray(list) && list.length > 0)
        return list.length;
      if (!is_iterable(list)) throw new TypeError(`Not a list or iterable ${string(list)}`);
      for (let _ of list)
        n += 1;
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

  defineGlobalSymbol("nreverse", in_place_reverse);  // Name from SIOD
  function in_place_reverse(list) {
    let res = NIL;
    while (is_cons(list)) {
      let next = list[CDR];
      list[CDR] = res;
      res = list;
      list = next;
    }
    return res;
  }

  defineGlobalSymbol("copy-list", copy_list);  // TODO: unit tests!
  function copy_list(list) {
    let res = NIL, last;
    if (is_null(list)) return NIL;
    if (is_cons(list)) {
      while (is_cons(list)) {
        let item = cons(list[CAR], NIL);
        if (last) last = last[CDR] = item;
        else res = last = item;
        list = list[CDR];
      }
      return res;
    }
    if (is_iterable(list)) {
      for (let item of list) {
        item = cons(item, NIL);
        if (last) last = last[CDR] = item;
        else res = last = item;
        list = list[CDR];
      }
      return res;
    }
    throw new TypeError(`Not a list or iterable ${list}`);
  }

  // (member key list)
  //     Returns the portion of the list where the car is equal to the key, or () if none found.
  defineGlobalSymbol("member", member);
  function member(key, list) {
    while (is_cons(list)) {
      if (key === list[CAR])   // TODO: == or ===?
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
      throw new TypeError(`Not an integer ${string(index)}`);
    if (index < 0) throw new RangeError(`nth`);
    if (is_list(list)) {
      while (index > 0 && is_cons(list)) {
        index -= 1;
        list = list[CDR];
      }
      if (is_cons(list))
        return list[CAR];
  ``} else if (Array.isArray(list)) {
      if (index < list.length)
        return list[index];
    } else {
      if (!is_iterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
      for (let value of list) {
        if (index <= 0)
          return value;
        index -= 1;
      }
    }
    throw new RangeError(`nth`);
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
      (a,b) => a.description.toLowerCase() < b.description.toLowerCase());
  }

  // (mapcar fn list1 list2 ...)
  defineGlobalSymbol("mapcar", mapcar, "map");
  function mapcar(fn, ...lists) {
    if (!bool(fn)) return NIL;
    // Actually, this will work for any iterables, and lists are iterable.
    let res = NIL, last;
    for (let list of lists) {
      if (is_list(list)) {
        // Could just let the list iterator handle it but might as well just follow the Cons chain
        // and not have to manufacture an iterator.
        while (is_cons(list)) {
          let item = list[CAR];
          item = _apply(fn, cons(item, NIL), this);
          if (last) last = last[CDR] = cons(item, NIL);
          else res = last = cons(item, NIL);
            list = list[CDR];
        }
      } else {
        if (!is_iterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
        for (let item of list) {
          item =  _apply(fn, cons(item, NIL), this);
          item = cons(item, NIL);
          if (last) last = last[CDR] = item;
          else res = last = item;
        }
      }
    }
    return res;
  }

  // Same as mapcar but results in an Array
  defineGlobalSymbol("array_map", array_map);
  function array_map(fn, ...lists) {
    let res = [];
    for (let list of lists) {
      for (let item of list) {
        item = _apply(fn, cons(item, NIL), this);
        res.push(item);
      }
    }
    return res;
  }

  // Useful for instantiating a lazy list using "map"
  const identity = x => x;
  defineGlobalSymbol("identity", identity);

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
    if (!is_cons(forms)) throw new SchemeEvalError(`No bindings`);
    let bindings = forms[CAR];
    forms = forms[CDR];
    let scope = newScope(this, "letrec-scope");
    while (is_cons(bindings)) {
      let binding = bindings[CAR];
      if (!is_cons(binding))
        throw new SchemeEvalError(`Bad binding ${string(binding)}`);
      let boundVar = binding[CAR], bindingForms = binding[CDR];
      if (typeof boundVar !== 'symbol')
        throw new SchemeEvalError(`Bad binding ${string(binding)}`);
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

  // Something like this would be nice, but it's not quite right
  //  let setSymWithWith = new Function("symbol", "value", "scope",
  //    "with (scope) { return symbol = value }");

  defineGlobalSymbol("set!", setq, { evalArgs: 1 }, "setq");
  function setq(symbol, value) { return setSym(symbol, value, this) }

  defineGlobalSymbol("set", setq);
  function set(symbol, value) { return setSym(symbol, value, this) }

  function setSym(symbol, value, scope) {
    // Can _almost_ do this using "with." Maybe come back to that.
    // The hell  of it is that JS has such a primitive internally to do
    // the same operation but you can't seem to get at it as a user.
    do {
      if (scope.hasOwnProperty(symbol)) {
        scope[symbol] = value;
        return val;
      }
      scope = Object.getPrototypeOf(scope);
    } while (scope && scope !== globalScope);  // I knew I'd use this someday!
    // XXX TODO: Hmmm, what to do here?
    globalScope[symbol] = val;
    return val;
  }

  // (qsort list predicate-fcn access-fcn)
  //   "qsort" is a lie for API compatibility with SIOD, but this sort has
  //   comparable performance and is excellent with partially-sorted lists.
  defineGlobalSymbol("mergesort", mergesort, "sort", "qsort");
  function mergesort(list, ...rest) {
    if (is_null(list)) return NIL;
    // Sort Arrays as Arrays
    if (Array.isArray(list))
      return in_place_mergesort(list.slice(0), ...rest);
    // Lists and other iterables are sorted as lists
    if (is_cons(list))
      return in_place_mergesort(copy_list(list), ...rest);
    let copied = NIL, last;
    if (!is_iterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
    for (let item of list) {
      item = cons(item, NIL);
      if (last) last = last[CDR] = item;
      else copied = last = item;
    }
    return in_place_mergesort(copied, ...rest);
  }

  defineGlobalSymbol("in-place-mergesort", in_place_mergesort, "in-place-sort", "nsort");
  function in_place_mergesort(list, ...rest) {
    if (is_null(list)) return NIL;
    let predicateFn = rest[0], accessFn = rest[1];
    // Reduce the optional predicete and access function to a single (JavaScript) "before" predicate
    let before = predicateFn, scope = this;
    if (bool(predicateFn)) {
      if (bool(accessFn)) {
        before = (a, b) => predicateFn.call(scope, accessFn.call(scope, a), accessFn.call(scope, b));
      } else if (typeof predicateFn !== 'function') {
        // Make sure it's a JS function, not a Scheme function
        before = (a, b) => _apply(fn, cons(a, cons(b, NIL)), this);
      }
    } else {
      if (bool(accessFn)) {
        before = function(a, b) {
          a = accessFn.call(scope, a);
          b = accessFn.call(scope, b);
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
    if (is_null(list)) return NIL;
    // Sort arrays as arrays
    if (Array.isArray(list)) {
      // ES10 stipulates that it only cares wheter the compare function
      // returns > 0, which means move "b"  before "a," or <= zero,
      // which means leave "a" before "b". There's no ned to distinguish
      // the "equal" case. Which is nice for us because the "before"
      // predicate doesn't distinguish that case (without a second call
      // with reversed arguments.)
      list.sort((a,b) => before.call(scope, a, b) ? -1 : 1);
      return list;
    }
    if (is_cons(list)) {
      return llsort.call(this, list, before);
    }
    throw new TypeError(`Not a list or iterable ${string(list)}`);
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
        if (is_null(stack[i])) {
          stack[i] = run;
          run = NIL;
          break;
        };
        run = merge(stack[i], run);
        stack[i] = NIL;
      }
      if (!is_null(run))
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
  function deep_eq(a, b, ... rest) {
    let maxDepth = rest[0], report = rest[1];
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

  function makeConsImposter(fn, form) {
    fn[PAIR] = true;
    fn[CAR] = form[CAR];
    fn[CDR] = form[CDR];
    return fn;
  }

  // (\ (params) (form1) (form2) ...)
  defineGlobalSymbol(LAMBDA_ATOM, lambda, { evalArgs: 0 });
  function lambda(body) {
    let schemeClosure = cons(CLOSURE_ATOM, cons(this, body));
    let scope = this;
    if (!is_cons(body)) throw TypeError(`Bad special closure ${string(body)}`);
    let lambda = cons(LAMBDA_ATOM, body);
    let jsClosure = args => _apply(lambda, args, scope);
    makeConsImposter(jsClosure, cons(CLOSURE_ATOM, cons(scope, body)));
    jsClosure[CLOSURE_ATOM] = true;
    return jsClosure;
  }

  // (\\ n (params) (body1) (body2) ...)
  defineGlobalSymbol(SLAMBDA_ATOM, special_lambda, { evalArgs: 0 });
  function special_lambda(body) {
    let schemeClosure = cons(SCLOSURE_ATOM, cons(this, body));
    let scope = this;
    if (!is_cons(body) || typeof body[CAR] !== 'number')
      throw TypeError(`Bad special closure ${string(body)}`);
    let lambda = cons(SLAMBDA_ATOM, body);
    let jsClosure = args => _apply(lambda, args, scope);
    makeConsImposter(jsClosure, lambda);
    jsClosure[CLOSURE_ATOM] = true;
    return jsClosure;
  }

  exportAPI("is_closure", is_closure)
  defineGlobalSymbol("closure?", is_closure);
  function is_closure(form) {
    if (form != null && form[CLOSURE_ATOM] === true)
      return true;
    if (is_cons(form))
      return !!(form[CAR] === CLOSURE_ATOM || form[CAR] === SCLOSURE_ATOM);
    return false;
  }

  //
  // try/catch/finally.
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
  defineGlobalSymbol("throw", js_throw);
  function js_throw(value) { throw value }

  // (catch (var [type] forms) forms) -- Java/JavaScript style
  defineGlobalSymbol("catch", js_catch, { evalArgs: 0 });
  function js_catch(catchClause, forms) {
    if (!is_cons(catchClause))
      throw new SchemeEvalError(`Bad catch clause ${string(catchClause)}`);
    let catchVar = catchClause[CAR], catchForms = catchClause[CDR];
    if (!is_cons(catchForms))
      throw new SchemeEvalError(`Bad catch clause ${string(catchClause)}`);
    let typeMatch;
    if (typeof catchForms[CAR] === 'string' || typeof catchForms[CAR] === 'function') {
      typeMatch = catchForms[CAR];
      catchForms = catchForms[CDR];
    }
    if (!is_cons(catchForms))
      throw new SchemeEvalError(`Bad catch clause ${string(catchClause)}`);
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
      throw new SchemeEvalError(`Define requires two parameters`);
    let defined = forms[CAR], value = forms[CDR][CAR];
    let scope = this, name = defined;
    if (is_cons(defined)) {
      name = defined[CAR];
      let args = defined[CDR];
      value = lambda.call(scope, cons(args, cons(value, NIL)));
    } else {
      value = _eval(value, scope);
    }
    if (typeof name === 'string') name = Atom(name);
    // Prevent a tragic mistake that's easy to make by accident. (Ask me how I know.)
    if (name === QUOTE_ATOM) throw new SchemeEvalError("Can't redefine quote");
    if (typeof name !== 'symbol')
      throw new TypeError(`Must define symbol or string ${string(defined)}`);
    globalScope[name] = value;
    return name;
  }

  //
  // This is where the magic happens
  //

  exportAPI("_eval", _eval);
  function _eval(form, scope) {
    if (is_null(form)) return NIL;
    if (typeof form === 'symbol') {
      let val = scope[form];
      if (val === undefined) throw new SchemeEvalError(`Undefined symbol ${string(form)}`);
      return val;
    }
    if (is_cons(form)) {
      let fn = form[CAR], args = form[CDR];
      if (fn === QUOTE_ATOM) // QUOTE is a function that will do this, but catch it here anyway.
        return form[CDR][CAR];
      if (is_cons(fn)) {
        let fnCar = fn[CAR];
        if (!(typeof fn === 'function' || fnCar === LAMBDA_ATOM || fnCar === SLAMBDA_ATOM))
          fn = _eval(fn, scope);
      } else {
        if (typeof fn !== 'function')
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
            value = value.value;
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
    let paramCount = 0, evalCount = MAX_INTEGER, restParam = false;
    if (is_cons(form) && (typeof form !== 'function' || form[CLOSURE_ATOM] === true)) {
      let opSym = form[CAR];
      if (opSym === SLAMBDA_ATOM) {
        evalCount = form[CAR];
      } else if (opSym === SCLOSURE_ATOM) {
        if (!is_cons(form[CDR])) throw new SchemeEvalError(`Bad form ${string(form)}`);
        evalCount = form[CDR][CAR];
      }
    } else if (typeof form === 'function') {
      let fn = form;
      // The function descriptor is encoded as:
      //   (~evalCount << 16) | (paramCount &0x7fff) | (restParam ? 0x8000 : 0);
      // If there's no function descriptor the default is to eval every argument
      // which, by no accident, is zero.
      let functionDescriptor = fn[FUNCTION_DESCRIPTOR_SYMBOL];
      if (functionDescriptor == null)
        functionDescriptor = createFunctionDescriptor(form);
      // Turns paramCounts and evalCounts that were MAX_INTEGER back into MAX_INTEGER, without branches
      evalCount = ~functionDescriptor >> 15 >>> 1;
      paramCount = functionDescriptor << 17 >> 16 >>> 1;
      restParam = (functionDescriptor & 0x8000) !== 0;
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
    let lift = restParam ? MAX_INTEGER : paramCount;
    if (typeof form === 'function' && form[CLOSURE_ATOM] !== true) { // Scheme functions impose as Cons cells
      let jsArgs = [];
      for (let i = 0; i < lift; ++i) {
        if (is_cons(args)) {
          jsArgs.push(args[CAR]);
          args = args[CDR];
        } else { 
          if (i < paramCount && paramCount !== MAX_INTEGER) {
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
            let closure;
            if (i < evalCount)
              closure = lambda.call(scope, (cons(paramList, forms)));
            else
              closure = special_lambda.call(scope, cons(i-evalCount, cons(paramList, forms)));
            return closure;
          }
          break;
        }
      }
      if (evalCount !== MAX_INTEGER) // last parameter gets the remaining arguments as a list
        jsArgs.push(args);
      return form.apply(scope, jsArgs);
    }
    if (is_cons(form)) {
      let opSym = form[CAR];
      let body = form[CDR];
      if (!is_cons(body)) throw new SchemeEvalError(`Bad form ${string(form)}`);
      if (opSym === CLOSURE_ATOM) {
        if (!is_cons(body)) throw new SchemeEvalError(`Bad closure ${string(form)}`);
        scope = body[CAR];
        body = body[CDR];
        opSym = LAMBDA_ATOM;
      }
      if (opSym === SCLOSURE_ATOM) {
        if (!is_cons(body) || !is_cons(body[CDR])) throw new SchemeEvalError(`Bad closure ${string(form)}`);
        scope = body[CDR][CAR];
        body = body[CDR][CDR];
        opSym = SLAMBDA_ATOM;
      }
      if (opSym === LAMBDA_ATOM || opSym === SLAMBDA_ATOM) {
        if (!is_cons(body)) throw new SchemeEvalError(`Bad lambda ${string(form)}`);
        let params = body[CAR];
        let forms = body[CDR];
        if (typeof params === 'symbol') { // Curry notation :)
          params = cons(params, NIL);
          forms = cons(forms, NIL);
        }
        scope = newScope(scope, "lambda-scope");
        let scratchParams = params, i = 0;
        while (is_cons(scratchParams)) {
          let param = scratchParams[CAR];
          if (typeof param !== 'symbol') throw new TypeError(`Param must be a symbol ${param}`);
          if (!is_null(args)) {  // XXX ???
            scope[param] = args[CAR];
            if (is_cons(args)) args = args[CDR];
          } else {
            // Curry up now! (partial application)
            let closure;
            if (i < evalCount)
              closure = lambda(cons.call(scope, scratchParams, forms));
            else
              closure = special_lambda.call(scope, cons(i-evalCount, cons(scratchParams, forms)));
            return closure;
          }
          scratchParams = scratchParams[CDR];
          i -= 1
        }
        if (typeof params === 'symbol')  // Neat trick for 'rest' params!
          scope[params] = args;
        else if (!is_null(scratchParams))
          throw new SchemeEvalError(`Bad parameter list ${string(params)}`);
        let res = NIL;
        while (is_cons(forms)) {
          res = _eval(forms[CAR], scope);
          forms = forms[CDR];
        }
        return res;
      }
    }
    throw new SchemeEvalError(`Can't apply ${string(form)}`);
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
  exportAPI("string", string);
  function string(obj, opts = {}) {
    opts = { ...schemeOpts, ...opts };
    let stringWrap = opts.stringWrap ?? 100;
    let wrapChar = opts.wrapChar ?? "\n";
    let maxDepth = opts.maxDepth ?? 500;
    let maxCarDepth = opts.maxCarDepth ?? maxDepth;
    let maxCdrDepth = opts.maxCdrDepth ?? maxDepth;
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
      if (obj[CLOSURE_ATOM] === true || objType === 'object') {
        // MUST do this before the is_null test, which will cause eager evaluation of
        // a LazyIteratorList, cause it to call next() and mutate into something else.
        if (obj[SUPERLAZY])
          return put("(...)");
        if (is_null(obj)) return put("()");
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
        if (obj[LAZYCDR]) {
          put("(");
          sep = "";
          if (obj[LAZYCAR])
            put("..")
          else
            toString(obj[CAR], maxCarDepth-1, maxCdrDepth);
          sep = " ";
          return put("...)", true);
        }
        if (is_cons(obj)) {
          put("(");
          indent += indentMore;
          sep = "";
          if (!obj[LAZYCAR]) {
            let objCar = obj[CAR];
            if ((objCar === LAMBDA_ATOM || objCar === SLAMBDA_ATOM ||
                objCar === CLOSURE_ATOM || objCar === SCLOSURE_ATOM)
                  && is_cons(obj[CDR])) {
              // Specal treatment of lambdas and closures with curry notation
              if (objCar === CLOSURE_ATOM|| objCar === SCLOSURE_ATOM) {
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
                      toString(evalCount, maxCarDepth, maxCdrDepth-1);
                      sep = " ";
                    }
                    toString(objCar, maxCarDepth-1, maxCdrDepth);  // %%closure or %%%closure
                    sep = " ";
                    toString(scopeCons[CAR], maxCarDepth-1, maxCdrDepth-2);  // scope
                    sep = " ";
                    toString(params, maxCarDepth-1, maxCdrDepth-2);  // actually the atom
                    sep = ""; put(".");
                    toString(scopeCons[CDR][CDR], maxCarDepth, maxCdrDepth-3);  // the form
                    sep = ""; put(")", true);
                    indent = saveIndent;
                    return;
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
                toString(forms, maxCarDepth, maxCdrDepth-1);
                sep = "";
                put(")", true);
                indent = saveIndent;
                return;
              }
            }
          }
          while (is_cons(obj)) {
            if (obj[LAZYCAR])
              put("..");
            else
              toString(obj[CAR], maxCarDepth-1, maxCdrDepth);
            sep = " ";
            if (obj[LAZYCDR])
              return put("...)", true);
            obj = obj[CDR];
            maxCdrDepth -= 1;
            if (obj[SUPERLAZY])
              return put("...)", true);
          }
          if (!is_null(obj)) {
            put(".");
            sep = " ";
            toString(obj, maxCarDepth, maxCdrDepth-1);
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
            toString(item, maxCarDepth-1, maxCdrDepth);
            maxCdrDepth -= 1;
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
            if (item instanceof EvaluateKeyValue) {
              prefix = "[";
              toString(item.key, maxCdrDepth, maxCdrDepth);
              prefix = "]: ";
              toString(item.value, maxCarDepth, maxCdrDepth);
            } else {
              prefix = `${string(name)}: `;
              toString(item, maxCarDepth-1, maxCdrDepth);
              maxCdrDepth -= 1;
            }
            sep = ", ";
          }
          sep = "";
          put("}", true);
          indent = saveIndent;
          return;
        }
      }
      if (typeof obj === 'function' && obj[CLOSURE_ATOM] !== true) {
        let fnDesc = analyzeJSFunction(obj);
        let name = fnDesc.name ? ` ${fnDesc.name}` : '';
        let params = fnDesc.printParams;
        let printBody = fnDesc.printBody;
        if (fnDesc.value && !fnDesc.body && !printBody)
          return put(`{${params} => ${fnDesc.value}`);
        if (printBody && (printBody.length > 60 || printBody.includes('\n')))
          printBody = '';
        put(`{function${name}${params}${printBody}}`);
        if (obj[COMPILED]) {
          sep = " ";
          toString(obj[COMPILED], maxCarDepth, maxCdrDepth);
        }
        sep = "";
        return put("}", true);
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

  function jsChar(charCode) {
    let hex = '000000' + charCode.toString(16);
    hex = hex.substr(hex.length-6);
    return `\\u{${hex}}`;
  }

  // Turns iterable objects like arrays into lists, recursively to "depth" (default 1) deep.
  defineGlobalSymbol("to-list", to_list);
  function to_list(obj, ...rest) {
    let depth = rest[0];
    if (!bool(depth)) depth = 1;
    if (depth <= 0) return obj;
    if (is_null(obj) || is_cons(obj)) return obj;
    if (typeof obj === 'object') {
      if (is_cons(obj)) return obj;  // Careful; Cons is iterable itself
      let list = NIL, last;
      if (!is_iterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
      for (let value of obj) {
        if (depth > 1 && is_iterable(value))
          value = to_list.call(this, value, depth-1);
        if (last) last = last[CDR] = cons(value, NIL);
        else list = last = cons(value, NIL);
      }
      return list;
    }
    return NIL;
  }

  //
  // Lazy lists by mutating down to ordinary Cons cells
  //
  class LazyCarList {
    [LAZYCAR]; [CDR];
    constructor(getCar, cdr) {
      this[LAZYCAR] = getCar;
      this[CDR] = cdr;
    }
    toString() { return string(this) }
    get [CAR]() {
      let car = this[LAZYCAR]();
      delete this[LAZYCAR];
      Object.setPrototypeOf(this, Cons.prototype);
      return this[CAR] = car;
    }
    set [CAR](val) {
      delete this[LAZYCAR];
      Object.setPrototypeOf(this, Cons.prototype);
      this[CAR] = val;
    }
    [Symbol.iterator] = pairIterator();
  }
  LazyCarList.prototype[PAIR] = true;
  LazyCarList.prototype[LIST] = true;

  class LazyCdrList {
    [CAR]; [LAZYCDR];
    constructor(car, getCdr) {
      this[CAR] = car;
      this[LAZYCDR] = getCdr;
    }
    toString() { return string(this) }
    get [CDR]() {
      let cdr = this[LAZYCDR]();
      delete this[LAZYCDR];
      Object.setPrototypeOf(this, Cons.prototype);
      return this[CDR] = cdr;
    }
    set [CDR](val) {
      delete this[LAZYCDR];
      Object.setPrototypeOf(this, Cons.prototype);
      this[CDR] = val;
    }
    [Symbol.iterator] = pairIterator();
  }
  LazyCdrList.prototype[PAIR] = true;
  LazyCdrList.prototype[LIST] = true;

  class LazyCarCdrList {
    [LAZYCAR]; [LAZYCDR];
    constructor(getCar, getCdr) {
      this[LAZYCAR] = getCar;
      this[LAZYCDR] = getCdr;
    }
    toString() { return string(this) }
    get [CAR]() {
      let car = this[LAZYCAR]();
      delete this[LAZYCAR];
      Object.setPrototypeOf(this, LazyCdrList.prototype);
      return this[CAR] = car;
    }
    set [CAR](val) {
      delete this[LAZYCAR];
      Object.setPrototypeOf(this, LazyCdrList.prototype);
      this[CAR] = val;
    }
    get [CDR]() {
      let cdr = this[LAZYCDR]();
      delete this[LAZYCDR];
      Object.setPrototypeOf(this, LazyCarList.prototype);
      return this[CDR] = cdr;
    }
    set [CDR](val) {
      delete this[LAZYCDR];
      Object.setPrototypeOf(this, LazyCarList.prototype);
      this[CDR] = val;
    }
    [Symbol.iterator] = pairIterator();
  }
  LazyCarCdrList.prototype[PAIR] = true;
  LazyCarCdrList.prototype[LIST] = true;
  
  //
  // Doesn't even know if it's a cons cell or null yet!
  //
  class LazyIteratorList {
    [LAZYCAR]; [LAZYCDR];
    constructor(iterator, mapper) {
      this[LAZYCAR] = mapper;
      this[LAZYCDR] = iterator;
    }
    get [CAR]() {
      if (!this[PAIR]) throw new TypeError(`car of nil`);
      return this[CAR];
    }
    set [CAR](val) {
      let mapper = this[LAZYCAR];
      if (!this[PAIR]) throw new TypeError(`set car of nil`);
      if (mapper)
        Object.setPrototypeOf(this, Cons.prototype);
      this[CAR] = val;
    }
    get [CDR]() {
      if (!this[PAIR]) throw new TypeError(`cdr of nil`);
      return this[CDR];
    }
    set [CDR](val) {
      if (!this[PAIR]) throw new TypeError(`set cdr of nil`);
      this[CDR] = val;
    }
    get [NULLSYM]() { return !this[PAIR] }
    get [PAIR]() { // Doesn't even know whether it's a cons or nil yet!
      let iterator = this[LAZYCDR], mapper = this[LAZYCAR];
      let { done, value: car } = iterator.next();
      if (done) {
        Object.setPrototypeOf(this, Object.getPrototypeOf(NIL));
        delete this[LAZYCDR];
        delete this[LAZYCAR];
        delete this[CAR];
        delete this[CDR];
        this[NULLSYM] = true;
        return false;
      }
      let cdr = new LazyIteratorList(iterator, mapper);
      if (mapper) {
        Object.setPrototypeOf(this, LazyCarList.prototype);
        delete this[LAZYCDR];
        delete this[LAZYCAR];
        this[LAZYCAR] = () => mapper(car);
      } else {
        Object.setPrototypeOf(this, Cons.prototype);
        delete this[LAZYCDR];
        delete this[LAZYCAR];
        this[CAR] = car;
      }
      this[CDR] = cdr;
      return true;
    }
    toString() { return string(this) }
    [Symbol.iterator] = pairIterator();
  }
  LazyIteratorList.prototype[SUPERLAZY] = true;
  LazyIteratorList.prototype[LAZYCAR] = true;
  LazyIteratorList.prototype[LAZYCDR] = true;
  LazyIteratorList.prototype[LIST] = true;

  defineGlobalSymbol("list-view", list_view);
  function list_view(obj) {
    let iterator = iteratorFor(obj, TypeError);
    return new LazyIteratorList(iterator);
  }

  defineGlobalSymbol("lazy-map", lazy_map);
  function lazy_map(fn, obj) {
    if (!bool(fn)) return NIL; // XXX probably should throw; also mapcar
    let scope = this, iterator = iteratorFor(obj, TypeError);
    return new LazyIteratorList(iterator, a => _apply(fn, cons(a, NIL), scope))
  }

  // Can't be "string" directly because that has an optional parameter and
  // calling to-string with one parameter would result in a closure.
  const to_string = obj => string(obj);
  defineGlobalSymbol("to-string", to_string);

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
      if (depth > 1 && is_iterable(item))
        value = to_array.call(this, item, depth-1);
      res.push(item);
    }
    return res;
  }

  //
  // S-epression tokenizer and parser
  //
  function* schemeTokenGenerator(characterSource, opts) {
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
      line = lineCount;
      lineChar = lineCharCount;

      if (NL[ch]) {
        yield { type: 'newline', position, line, lineChar };
        nextc();
        continue;
      }

      if (ch === ';' || (ch === '/' && peekc() === '/')) {  // ; or // begins a comment
        parseContext.push({ type: 'comment', value: ch === ';' ? ch : '//', position, line, lineChar });
        while (ch && !NL[ch])
          nextc();
        parseContext.currentToken = { type: 'endcomment', value: ';', endPosition: charCount-1, endWidth: 1, line, lineChar };
        parseContext.pop();
        yield { type: (ch ? 'newline': 'end'), position, line, lineChar };
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
            } else {
              ch = ESCAPE_STRINGS[ch] ?? ch;
            }
          }
          // Traditional string continuation begins when a string hits a newline and the last char
          // is a "\""; the "\"" is ignored and the string continues at the beginning of the
          // next line. Multiline string continuation begins when a the line ends in "\\". In that
          // case initial whitespace is skipped on the next line(s) and the string continues
          // after a "+", which simply appends the following text, or a "|", which appends a newline
          // then the following text. The line does _not_ need to be terminateed with a "\"
          // or "\\" and parsing continues until an unescaped "\".
          if (NL[ch]) {
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

      if (SINGLE_CHAR_TOKENS[ch]) {
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
            yield { type: 'number', value, position, line, lineChar };
            continue;
          }
        }
      }

      if (IDENT1[ch]) {
        let str = '';
        while (ch && IDENT2[ch]) {
          // lambda symbols are special so we can parse \x as \ x
          if ((str[0] === '\\' || str[0] === LAMBDA_CHAR) && JSIDENT[ch])
            break;
          str += ch, nextc();
        }
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
      let next = characterGenerator.next();
      if (next.done) {
        _done = true;
        return ch = '';
      }
      charCount += 1;
      lineCharCount += 1;
      if (NL[ch]) {
        lineCount += 1;
        lineCharCount = 0;
      }
      return ch = next.value;
    }

    function peekc(n = 0) {
      for (let get = n - _peek.length + 1; get > 0; --get) {
        let next = characterGenerator.next();
        if (next.done) {
          _done = true;
          return '';
        }
        charCount += 1;
        lineCharCount += 1;
        if (NL[ch]) {
          lineCount += 1;
          lineCharCount = 0;
        }
        _peek.push(next.value);
      }
      return _peek[n];
    }
  }

  let gensym_count = 0;
  const gensym = () => Symbol(`*gensym-${gensym_count++}*`);
  defineGlobalSymbol("gensym", gensym);

  defineGlobalSymbol("parse", parseSExpr);
  exportAPI("parseSExpr", parseSExpr);
  function parseSExpr(characterStream, ...rest) {
    let opts = rest[0] ?? {};
    opts = { ...schemeOpts, ...opts };
    let parseContext = opts.parseContext ?? [];
    opts.parseContext = parseContext;
    parseContext.length = 0;
    let path = opts.path;
    let assignSyntax = opts.assignSyntax ?? false;
    let _tokens = [], _done = false;
    let tokenGenerator = schemeTokenGenerator(characterStream, opts);
    let expr;
    if (assignSyntax && token().type === 'atom' &&
        token(1).type === 'atom' && token(1).value === Atom('=')) {
      // Allows the REPL to select a mode where, at the top-level only,
      //    a = expr
      // is the same as
      //    (define a expr)
      // It might or might not be a good idea. It isn't worse than "evalquote", if anyone
      // else remembers that. In any case it's opt-in.
      let sym = token().value;
      parseContext.push({ type: 'assign', value: sym });
      consumeToken(), consumeToken();
      let assigned = parseExpr();
      parseContext.pop();
      expr = list(Atom("define"), sym, assigned);
    } else {
      expr = parseExpr(0);
    }
    let initialNewlineOK = true;
    let tok = token(0, initialNewlineOK);
    if (tok.type === 'newline' || tok.type === 'end')
      return expr;
    throwSyntaxError();

    function parseExpr() {
      if (token().type === 'string' || token().type === 'number') {
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
          if (token().type === ')') {
            parseContext.pop();
            consumeToken();
            return NIL;
          } else if (token().type === '.') {
            consumeToken();
            let val = parseExpr();
            parseContext.pop();
            if (token().type !== ')') throwSyntaxError();
            consumeToken();
            return val;
          }
          if (token().type === 'end' || token().type === 'partial')
            throw new SchemeParseIncomplete(path, token(), parseContext);
          if (token().type === 'garbage') throwSyntaxError();
          let first = parseExpr();
          let rest = parseListBody();
          return cons(first, rest);
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
            throw new SchemeParseIncomplete(path, token(), parseContext);
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
                || token().type === 'number' || token().type === '[') {
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
                res[gensym] = new EvaluateKeyValue(sym, val);
              else
                res[sym] = val;
            }
            gotIt = true;
            if (token().type === ',')  // Comma is optional
              consumeToken();
            if (token().type === 'end'|| token().type === 'partial')
              throw new SchemeParseIncomplete(path, token(), parseContext);
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
        throw new SchemeParseIncomplete(path, errorToken, parseContext)
      let newline = false;
      while (_tokens.length > 0) {
        // "detokenize" any lookahead tokens
        let token = _tokens.pop();
        newline = (token.type === 'newline');
        str += (tok.value !== undefined ? string(tok.value) : tok.type);
        str += " ";
      }
      while (!newline) {
        let { done, value: ch } = characterStream.next();
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
    let str = fn.toString(), pos = 0, token = "", paramPos = 0, nonOptionalCount;
    let name = fn.name, params = [], restParam, value, body, native = false, printParams, printBody;
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
          value = possibleValue;
      }
    }
    if (nonOptionalCount === undefined)
      nonOptionalCount = params.length;
    let res = { name, params, restParam, value, body, printBody, printParams, native, nonOptionalCount };
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
        if (nonOptionalCount === undefined)
          nonOptionalCount = params.length-1;
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
          value = possibleValue;
          body = str.substring(bodyPos, returnPos);
        }
      }
    }

    function nextToken() {
      // Super janky tokenizer.
      // Most of what it returns is garbage, but it correctly returns anything we actually care about.
      // The assumption is that what JS returns is well-formed, so it takes a lot of liberties.
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

  const COMPILE_SENTINEL = Symbol("SENTINEL");

  // (compile (fn args) forms) -- defines a compiled function
  // (compile lambda) -- returns a compiled lambda expression
  defineGlobalSymbol("compile", compile, { evalArgs: 0 });
  function compile(nameAndParams, forms, _) {
    if (!is_cons(nameAndParams)) new TypeError(`First parameter must be a list ${forms}`);
    let name = Atom(nameAndParams[CAR]);
    let args = nameAndParams[CDR];
    if (typeof name !== 'symbol') new TypeError(`Function name must be an atom or string ${forms}`)    
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
    if (name === QUOTE_ATOM) throw new SchemeEvalError("Can't redefine quote ${lambda}");

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
    let evalErrorStr = bind(SchemeEvalError, "EvalError");
    bind(NIL, "NIL");
    bind(bool, "bool");
    bind(cons, "cons");
    bind(car, "car");
    bind(cdr, "cdr");
    bind(COMPILED, "COMPILED");
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
        else if (typeof obj === 'function')
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
      throw new SchemeCompileError(`Too comlpex ${string(form)}`);
    let result;
    if (form === undefined) {
      result = "undefined";
    } else if (form === null) {
      result = "null";
    } else if (typeof form === 'number' || typeof form === 'bigint' || typeof form === 'string') {
      result = string(form);
    } else if (is_null(form)) {
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
    let paramCount = 0, evalCount = MAX_INTEGER, hasRestParam = false;
    let name, params, restParam, value, body, hook;
    let saveIndent = tools.indent
    let boundVal = tools.boundVal(form);
    if (boundVal) form = boundVal;
    if (typeof form === 'function') { // form equals function :)
      hook = form[COMPILE_HOOK];
      ({ name, params, restParam, value, body } = analyzeJSFunction(form));
      if (restParam)  // rest-param functions need a hook
        value = body = undefined;
      let functionDescriptor = form[FUNCTION_DESCRIPTOR_SYMBOL];
      if (functionDescriptor == null)
        functionDescriptor = createFunctionDescriptor(form);
      evalCount = ~functionDescriptor >> 15 >>> 1;
      paramCount = functionDescriptor << 17 >> 16 >>> 1;
      hasRestParam = (functionDescriptor & 0x8000) !== 0;
      if (name === '') name = undefined;
    } else if (is_cons(form)) {
      let opSym = form[CAR];
      if (opSym === SLAMBDA_ATOM) {
        evalCount = 0;
      }
      // Compile closures?
      params = [];
      if (!is_cons(form[CDR])) throw new SchemeEvalError(`Bad lambda ${string(form)}`);
      if (typeof form[CDR] === 'symbol') {
        params.push(form[CDR])
      } else {
        for (let params = form[CDR[CAR]]; is_cons(params); params = params[CDR])
          params.push(params[CAR]);
      }
      paramCount = params.length;
    }
    let result = tools.newTemp(name);
    let argv = [];
    // Materialize ALL the arguments into an array
    for (let i = 0; is_cons(args); ++i) {
      let arg = args[CAR];
      if (i < evalCount)
        arg = compileEval(args[CAR], compileScope, tools);
      argv.push(arg);
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
      let i = 0, lift = hasRestParam ? MAX_INTEGER : paramCount;
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
      }
      if (typeof form === 'function') {
        if (body)
          tools.emit(body); 
        tools.emit(`${result} = (${value});`);
        tools.indent = saveIndent;
        tools.emit(`}`);
        return result;
      }
      if (is_cons(form)) {
        let opSym = form[CAR];
        let body = form[CDR];
        if (!is_cons(body)) throw new SchemeEvalError(`Bad form ${string(form)}`);
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
    if (!is_cons(form)) throw new SchemeEvalError(`Bad lambda ${string(form)}`);
    let body = form[CDR];
    if (!is_cons(body)) throw new SchemeEvalError(`Bad lambda ${string(form)}`);
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
      if (typeof param !== 'symbol') throw new SchemeEvalError(`Param must be a symbol ${param}`);
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
    else if (!is_null(params)) {
      throw new SchemeEvalError(`Bad parameter list ${string(origFormalParams)}`);
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
    let res = 'NIL';
    while (is_cons(forms)) {
      res = compileEval(forms[CAR], compileScope, tools);
      forms = forms[CDR];
    }
    tools.emit(`return ${res};`);
    tools.indent = saveIndent;
    tools.emit(`}`);
    tools.emit(`${result}[COMPILED] = ${tools.bind(form)};`);
    return res;
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
        if (parseContext.currentToken?.type !== 'string')
          indent =  "  ".repeat(parseContext.length);
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
        if (!expr) continue;
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

  exportAPI("_setGlobalScope_test_hook_", _setGlobalScope_test_hook_)
  function _setGlobalScope_test_hook_(scope) {
    let previousGlobalScope = globalScope;
    globalScope = scope;
    return previousGlobalScope;
  }

  return globalScope;
} // Whew!