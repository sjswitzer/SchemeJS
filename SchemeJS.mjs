//
// SchemeJS: Scheme in JavaScript
//
// Copyright 2021 Stan Switzer -- (sjswitzer [at] gmail [dot] com)
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

export const VERSION = "1.1 (alpha)";

//
// Creates a SchemeJS instance.
//
// Instances are distinct to the bones; they do not even recognize each other's
// Cons cells or NIL values. This is by design. People should be able
// to poke things into class definitions to experiment with different ideas
// but that should only affect that specific SchemeJS instance; others should
// be unaffected.
//
// This implementation aims for broad compatibility with SIOD, but inevitably,
// and in the grand tradition of lisp implementations, introduces a new dialect.
// Which is a sin but not a crime.
//
export function createInstance(schemeOpts = {}) {
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
  const lambdaStr = schemeOpts.lambdaStr ?? "\\";

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
  // I presume that typeof checks are very cheap to free because the runtime
  // and JIT generally have to be doing the checks already and sometimes already
  // know through dataflow analysis what the type is already
  //
  // Beware when traversing lists. Things purported to be lists might not be
  // and although lists are conventionally NIL-terminated, the final "cdr"
  // could be anything at all.

  const CAR = Symbol("CAR"), CDR = Symbol("CDR");
  const PAIR = Symbol("PAIR"), LIST = Symbol("LIST"), NULLSYM = Symbol("NULLSYM");
  const LAZYCAR = Symbol("LAZYCAR"), LAZYCDR = Symbol("LAZYCDR"), SUPERLAZY = Symbol("SUPERLAZY");
  const COMPILED = Symbol("COMPILED"), JITCOMPILED = Symbol("JITCOMPILED");
  const NAMETAG = Symbol("NAMETAG");
  // Since these symbols are tagged on external JS functions and objects,label it as ours as a courtesy.
  const PARAMETER_DESCRIPTOR = Symbol('SchemeJS-PARAMETER-DESCRIPTOR'), EQUAL_FUNCTION = Symbol('SchemeJS-EQUAL');

  // I trust JITs to inline these
  const isCons = obj => obj != null && obj[PAIR] === true;
  const isNil = obj => obj != null && obj[NULLSYM] === true;
  const isList = obj => obj != null && obj[LIST] === true;

  // Objects that "eval" to themselves
  // I trust the JavaScript runtime and JITs to reduce this to some
  // tag-bit inspection that they're probably already doing.
  // (Except for the NIL check, which is last for that reason.)
  const isPrimitive = obj => obj == null ||
      (typeof obj !== 'symbol' && typeof obj !== 'object' && typeof obj !== 'function')
      || obj[NULLSYM] === true;

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
        if (!isCons(current))
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

  exportAPI("isCons", isCons);

  //
  // Atoms are Symbols that are in the ATOMS object
  //
  const ATOMS = {};

  const isAtom = obj => typeof obj === 'symbol' && ATOMS[obj.description] === obj;

  function Atom(name) {
    // If they pass in an atom, just return it
    if (isAtom(name)) return name;
    if (typeof name !== 'string')
      throw new SchemeEvalError(`Not a string ${name}`);
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
  const QUESTION_ATOM = Atom("?");

  const isIterable = obj => obj != null && typeof obj[Symbol.iterator] === 'function';

  function iteratorFor(obj, throwException = TypeError) {
    if (obj != null) {
      if (typeof obj[Symbol.iterator] === 'function') return obj[Symbol.iterator]();
      if (typeof obj.next === 'function') return obj;
    }
    if (throwException) throw new throwException(`Not an iterable or list ${obj}`);
  }

  //
  // Character clases for parsing
  //
  const NBSP = '\u00a0', VTAB = '\u000b', FORMFEED = '\u000c', ELIPSIS = '\u0085';
  const MUL = '\u00d7', DIV = '\u00f7';
  const NL = {}, SINGLE_CHAR_TOKENS = {}, QUOTES = {}, DIGITS = {}, NUM1 = {}, NUM2 = {};
  const JSIDENT1 = {}, JSIDENT2 = Object.create(JSIDENT1);
  // IDENT2 includes IDENT1 by inheritence, as does WSNL WS.
  const IDENT1 = {}, IDENT2 = Object.create(IDENT1), WS = {}, WSNL = Object.create(WS);
  for (let ch of `abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$`)
    JSIDENT1[ch] = ch.codePointAt(0);
  for (let ch of `0123456789`)
    DIGITS[ch] = IDENT2[ch] = NUM1[ch] = NUM2[ch] = JSIDENT2[ch] = ch.codePointAt(0);
  for (let ch of `+-.`)
    NUM1[ch] = NUM2[ch] = ch.codePointAt(0);
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

// Why are these initialized here, you ask?
// Because they're indirectly refernced by defineGlobalSymbol is why.
const COMPILE_HOOK = Symbol("COMPILE-HOOK"), COMPILE_INFO = Symbol("BUILTIN-COMPILE-INFO");
const MAX_INTEGER = (2**31-1)|0;  // Presumably allows JITs to do small-int optimizations
const analyzedFunctions = new Map();
let helpGroups = globalScope._helpgroups_ = {};  // For clients that want to implement help.

  //
  // Unlike exportAPI, which exports an API to clients, defineGlobalSymbol
  // defines a symbol for the SchemeJS environment AND exports it as an API.
  // Be careful which one you use!
  //
  // Some of the short exported functions are all on one line. This is intentional.
  // Those function's bodies are included in the string representation used for dispay.
  // 
  exportAPI("defineGlobalSymbol", defineGlobalSymbol);
  function defineGlobalSymbol(name, value, ...aliases) {
    let opts = {};
    if (typeof aliases[0] === 'object')
      opts = aliases.shift();
    let group = opts.group ?? "default";
    if (typeof value === 'function') {
      let evalCount = opts.evalArgs ?? MAX_INTEGER;
      examineFunctionForParameterDescriptor(value, evalCount);
      if (!opts.dontInline)
        examineFunctionForCompilerTemplates(name, value, opts.compileHook, evalCount);
    }
    let { atom, atomName, jsName } = normalizeExportToJavaScriptName(name);
    globalScope[atom] = value;
    helpGroups[atom] = group;
    if (jsName && !opts.schemeOnly) {
      globalScope[jsName] = value;
      helpGroups[jsName] = group;
    }
    for (let alias of aliases) {
      ({ atom, atomName, jsName } = normalizeExportToJavaScriptName(alias));
      globalScope[atom] = value;
      helpGroups[atom] = group;
      if (jsName && !opts.schemeOnly) {
        globalScope[jsName] = value;
        helpGroups[jsName] = group;
      }
    }
    return atom;
  }

  function normalizeExportToJavaScriptName(name) {
    if (typeof name === 'symbol')
      name = name.description;
    let atomName = name;
    let atom = Atom(name);
    // Java API name
    let jsName = replaceAll(name, "->", "_to_");
    jsName = replaceAll(jsName, "-", "_");
    jsName = replaceAll(jsName, "@", "_at_");
    jsName = replaceAll(jsName, "*", "_star_");
    jsName = replaceAll(jsName, "?", "P");
    if (!JSIDENT1[jsName[0]])
      jsName = undefined;
    return { atom, atomName, jsName };
  }

  function subclassOf(cls, supercls) {
    while (cls.__proto__) {
      if (cls === supercls) return true;
      cls = cls.__proto__;
    }
    return false;
  }

  // Substitute for String.prototype.replaceAll until Node.js supports it.
  // (Maybe I just need to undate Node? Well, I can't be the only one, so...)
  // This isn't strictly correct since it blows up if newSubstr contains substr,
  // but it's good enough for our purposes.
  // Note: A module has no business installing Polyfills, even if they ARE correct.
  function replaceAll(str, substr, newSubstr) {
    let prevStr;
    do {
      prevStr = str;
      str = str.replace(substr, newSubstr);
    } while (str !== prevStr);
    return str;
  }

  function examineFunctionForCompilerTemplates(name, fn, hook, evalCount) {
    if (hook) {
      fn[COMPILE_HOOK] = hook;
    } else {
      // A policy thing, I guess. You wouldnt expect to inline an Error class definition
      // and it's tedious to mark them all as "dontInline." This would actually be the case
      // of any "class," but Error classes are the only ones currently defined in the
      // SchemeJS API and there's no way to truly distinguish a class from a function
      // in JavaScript.
      if (subclassOf(fn, Error))
        return;
      if (evalCount !== MAX_INTEGER) { // templates are of no use for special evaluation
        console.log("SPECIAL FUNCTION REQUIRES COMPILE HOOK", name, fn);
        return;
      }
    }
    let fnInfo = analyzeJSFunction(fn);
    // Can't inline a native function, obviously, but you _could_ hook one
    if (fnInfo.native)
      return;
  
    if (!fnInfo.value && !hook) {
      console.log("FUNCTION REQUIRES TEMPLATABLE DEFINITION OR COMPILE HOOK", name, fn);
      return;
    }
    fn[COMPILE_INFO] = fnInfo;
  }

  exportAPI("PAIR_SYMBOL", PAIR);
  exportAPI("LIST_SYMBOL", LIST);
  exportAPI("CAR_SYMBOL", CAR);
  exportAPI("CDR_SYMBOL", CDR);
  exportAPI("LAZYCAR_SYMBOL", LAZYCAR);
  exportAPI("LAZYCDR_SYMBOL", LAZYCDR);
  exportAPI("SUPERLAZY_SYMBOL", SUPERLAZY);

  defineGlobalSymbol("VERSION", VERSION);
  defineGlobalSymbol("intern", Atom, { dontInline: true }, "Atom");

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
  // here. What I'd like to do is define schemeTrue in a way that the JIT can trivially evaluate.
  // In particular, treating NIL as false is expensive.
  //
  const schemeTrue = a => a === true || (a !== false && a != null && !isNil(a));
  exportAPI("schemeTrue", schemeTrue);

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

  const QUOTE_ATOM = defineGlobalSymbol("'", quoted => quoted[CAR], { evalArgs: 0, dontInline: true }, "quote");
  
  defineGlobalSymbol("scope", function() { return this });

  exportAPI("NIL", NIL);
  defineGlobalSymbol("nil", NIL);
  defineGlobalSymbol("undefined", undefined, { schemeOnly: true });
  defineGlobalSymbol("null", null, { schemeOnly: true });
  defineGlobalSymbol("true", true, { schemeOnly: true }, "t", "#t"); // SIOD: t, MIT Scheme: #t
  defineGlobalSymbol("false", false, { schemeOnly: true });
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
  defineGlobalSymbol("typeof", a => typeof a, { schemeOnly: true });

  // Hoist a bunch of JavaScript definitions into the global scope
  defineGlobalSymbol("NaN", NaN, { schemeOnly: true });
  defineGlobalSymbol("Infinity", Infinity, { schemeOnly: true });

  { // (Local scope so we don't hang onto the property descriptors)
    // Import global JavaScript symbols
    let propDescs = Object.getOwnPropertyDescriptors(globalThis);
    for (let name in propDescs) {
      let {value, get} = propDescs[name];
      if (!get && value)
        defineGlobalSymbol(name, value, { schemeOnly: true, dontInline: true, group: "imported" });
    }

    // Object static methods
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
        // SIOD defines sin, cos, asin, etc. so I'll just define them all like that
        if (typeof value === 'function')
          defineGlobalSymbol(name, value, { schemeOnly: true }, `Math-${name}`);
      }
    }
    defineGlobalSymbol("abs", a => a < 0 ? -a : a);  // Overwrite Math.abs; this deals with BigInt too
    // This is redundant but I want them defined in the "builtin" group.
    defineGlobalSymbol("globalThis", globalThis, { schemeOnly: true });
    for (let obj of [Object, Boolean, Symbol, Number, String, BigInt, Array])
      defineGlobalSymbol(obj.name, obj, { schemeOnly: true });
  }

  defineGlobalSymbol("eval-string", eval_string, { dontInline: true });
  function eval_string(str, scope = this) {
    let expr = parseSExpr(str);
    return _eval(expr, scope);
  }

  defineGlobalSymbol("globalScope", globalScope);

  // Pokemon gotta catch 'em' all!
  defineGlobalSymbol("!", a => !schemeTrue(a), { schemeOnly: true, group: "logical-op" }, "not");
  defineGlobalSymbol("~", a => ~a, { schemeOnly: true, group: "binary-op" }, "bit-not");
  defineGlobalSymbol("**", (a,b) => a ** b, { schemeOnly: true, group: "binary-op" }, "pow");  // overrides Math.pow
  defineGlobalSymbol("%", (a,b) => a % b, { schemeOnly: true, group: "binary-op" }, "rem");
  defineGlobalSymbol("<<", (a,b) => a << b, { schemeOnly: true, group: "binary-op" }, "bit-shl");
  defineGlobalSymbol(">>", (a,b) => a >> b, { schemeOnly: true, group: "binary-op" }, "bit-shr");
  defineGlobalSymbol(">>>", (a,b) => a >>> b, { schemeOnly: true, group: "binary-op" }, "bit-ushr");
  defineGlobalSymbol("ash", (a, b) => b < 0 ? a >>> -b : a << b, { schemeOnly: true, group: "binary-op" });  // SIOD
  defineGlobalSymbol("in", (a,b) => a in b, { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("new", (cls, ...args) => new cls(...args), { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("instanceof", (a,b) => a instanceof b, { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("@", (a, b) => a[b], { schemeOnly: true, group: "js-op" }, "aref");  // indexing and member access (SIOD: aref)
  defineGlobalSymbol("@@", (a, b, c) => a[b][c], { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("@@@", (a, b, c, d) => a[b][c][d], { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("@?", (a, b) => a?.[b], { schemeOnly: true, group: "js-op" });  // conditional indexing and member access
  defineGlobalSymbol("@@?", (a, b, c) => a?.[b]?.[c], { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("@@@?", (a, b, c, d) => a?.[b]?.[c]?.[d], { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("@!", (a, b, ...params) => a[b](...params), { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("@@!", (a, b, c, ...params) => a[b][c](...params), { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("@@@!", (a, b, c, d, ...params) => a[b][c][d](...params), { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("@?!", (a, b, ...params) => a?.[b](...params), { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("@@?!", (a, b, c, ...params) => a?.[b]?.[c](...params), { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("@@@?!", (a, b, c, d, ...params) => a?.[b]?.[c]?.[d](...params), { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("@=", (a, b, c) => a[b] = c, { schemeOnly: true, group: "js-op" }, "js-assign");
  defineGlobalSymbol("@@=", (a, b, c, d) => a[b][c] = d), { schemeOnly: true, group: "js-op" };
  defineGlobalSymbol("@@@=", (a, b, c, d, e) => a[b][b][c] = d, { schemeOnly: true, group: "js-op" });
  defineGlobalSymbol("delete", (a, b) => delete a[b]), { schemeOnly: true, group: "js-op" };
  defineGlobalSymbol("void", _ => undefined, { schemeOnly: true, group: "js-op" });

  defineGlobalSymbol("to-lower-case", to_lower_case);
  function to_lower_case(str, locale = undefined) {
    if (typeof str !== 'string') throw new TypeError(`${string(str)} is not a string}`);
    let result;  // write this way so that it can be a compiler template
    if (locale === undefined) result = str.toLowerCase();
    else result = str.toLocaleLowerCase(locale);
    return result;
  }

  defineGlobalSymbol("to-upper-case", to_upper_case);
  function to_upper_case(str, locale = undefined) {
    if (typeof str !== 'string') throw new TypeError(`${string(str)} is not a string}`);
    let result;  // write this way so that it can be a compiler template
    if (locale === undefined) result = str.toUpperCase();
    else result = str.toLocaleUpperCase(locale);
    return result;
  }

  //
  // Variable args definitions
  //
  defineGlobalSymbol("+", add, { compileHook: add_hook, group: "var-op" }, "add");
  function add(a, b, ...rest) {
    a += b;
    for (b of rest)
      a += b;
    return a;
  }
  function add_hook(args, ssaScope, tools) {
    if (args.length === 0) return 'NaN';
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` + ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("-", sub, { compileHook: sub_hook, group: "var-op" }, "sub");
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
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` - ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("*", mul, { compileHook: mul_hook, group: "var-op" }, MUL, "mul");
  function mul(a, b, ...rest) {
    a *= b;
    for (let b of rest)
      a *= b;
    return a;
  }
  function mul_hook(args, ssaScope, tools) {
    if (args.length === 0) return 'NaN';
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` * ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol('/', div, { compileHook: div_hook, group: "var-op" }, DIV, "div");
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
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` / ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("&", bit_and, { compileHook: bit_and_hook, group: "var-op" }, "bit-and");
  function bit_and(a, b, ...rest) {
    a &= b;
    for (b of rest)
      a &= b;
    return a;
  }
  function bit_and_hook(args, ssaScope, tools) {
    if (args.length < 2) return 0;
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` & ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("|", bit_or, { compileHook: bit_or_hook, group: "var-op" }, "bit-or");
  function bit_or(a, b, ...rest) {
    a |= b;
    for (let b of rest)
      a |= b;
    return a;
  }
  function bit_or_hook(args, ssaScope, tools) {
    if (args.length < 2) return 0;
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` | ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("^", bit_xor, { compileHook: bit_xor_hook, group: "var-op" }, "bit-xor");
  function bit_xor(a, b, ...rest) {
    a ^= b;
    for (let b of rest)
      a ^= b;
    return a;
  }
  function bit_xor_hook(args, ssaScope, tools) {
    if (args.length < 2) return 0;
    let str = `(${args[0]}`;
    for (let i = 1; i < args.length; ++i)
      str += ` ^ ${args[i]}`;
    str += `)`;
    return str;
  }

  defineGlobalSymbol("<", lt, { evalArgs: 2, compileHook: lt_hook, group: "var-op" }, "lt");
  function lt(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!(a < b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
    }
    return true;
  }
  function lt_hook(args, ssaScope, tools) {
    return compare_hooks(args, ssaScope, tools, 'A < B', 'lt');
  }

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

  defineGlobalSymbol("<=", le, { evalArgs: 2, compileHook: le_hook, group: "compare-op" }, "le");
  function le(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!(a <= b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
    }
    return true;
  }
  function le_hook(args, ssaScope, tools) {
    return compare_hooks(args, ssaScope, tools, 'A <= B', 'le');
  }

  defineGlobalSymbol(">", gt, { evalArgs: 2, compileHook: gt_hook, group: "compare-op" }, "gt");
  function gt(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!(a > b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
    }
    return true;
  }
  function gt_hook(args, ssaScope, tools) {
    return compare_hooks(args, ssaScope, tools, 'A > B', 'gt');
  }

  defineGlobalSymbol(">=", ge, { evalArgs: 2, compileHook: ge_hook, group: "compare-op" }, "ge");
  function ge(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!(a >= b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
    }
    return true;
  }
  function ge_hook(args, ssaScope, tools) {
    return compare_hooks(args, ssaScope, tools, 'A >= B', 'ge');
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
    return compare_hooks(args, ssaScope, tools, 'equal(A, B)', 'scheme_eq');
  }

  defineGlobalSymbol("==", eq, { evalArgs: 2, compileHook: eq_hook, group: "compare-op" });
  function eq(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!(a == b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
    }
    return true;
  }
  function eq_hook(args, ssaScope, tools) {
    if (args.length < 2) return 'true';
    return compare_hooks(args, ssaScope, tools, 'A == B', 'eq');
  }

  defineGlobalSymbol("===", eeq, { evalArgs: 2, compileHook: eeq_hook, group: "compare-op" }); // XXX name
  function eeq(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!(a === b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
    }
    return true;
  }
  function eeq_hook(args, ssaScope, tools) {
    if (args.length < 2) return 'true';
    return compare_hooks(args, ssaScope, tools, 'A === B', 'eeq');
  }

  // TODO: give = the variadic treatment
  defineGlobalSymbol("=", (a, b) =>  deep_eq(a, b));

  defineGlobalSymbol("!=", neq, { evalArgs: 2, compileHook: neq_hook, group: "compare-op" }, "ne");
  function neq(a, b, ...rest) {
    return !eq.call(this, a, b, ...rest);
  }
  function neq_hook(args, ssaScope, tools) {
    if (args.length < 2) return 'false';
    let eq = compare_hooks(args, ssaScope, tools, 'A == B', 'eq');
    return `(!${eq})`;
  }

  defineGlobalSymbol("!==", neeq, { evalArgs: 2, compileHook: neeq, group: "compare-op" });
  function neeq(a, b, ...rest) {
    return !eeq.call(this, a, b, ...rest);
  }
  function neeq(args, ssaScope, tools) {
    let eeq = compare_hooks(args, ssaScope, tools, 'A == B', 'eq');
    return `(!${eeq})`;
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

  // Logical & Conditional

  defineGlobalSymbol("&&", and, { evalArgs: 0, compileHook: and_hook, group: "logical-op" }, "and");
  function and(...forms) {
    let val = true;
    for (let i = 0, formsLength = forms.length; i < formsLength; ++i) {
     val = _eval(forms[i], this);
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

  defineGlobalSymbol("||", or, { evalArgs: 0, compileHook: or_hook, group: "logical-op" }, "or");
  function or(...forms) {
    let val = false;
    for (let i = 0, formsLength = forms.length; i < formsLength; ++i) {
      val = _eval(forms[i], this);
      if (schemeTrue(val)) return val;
    }
    return val;
  }
  function or_hook(args, ssaScope, tools,) {
    return and_or_hook(args, ssaScope, tools, 'or', 'false', 'schemeTrue');
  }

  defineGlobalSymbol("??", nullish, { evalArgs: 0, compileHook: nullish_hook, group: "logical-op" }, "nullish");
  function nullish(...forms) {
    let val = undefined;
    for (let i = 0, formsLength = forms.length; i < formsLength; ++i) {
      val = _eval(forms[i], this);
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
  // "?" can be partially-applied.
  //    (? true) returns a function that evaluates its first parameter.
  //    (? false) returns a function that evaluates its second parameter.
  //
  defineGlobalSymbol("?", ifelse, { evalArgs: 1, compileHook: ifelse_hook, group: "core", schemeOnly: true }, "if");
  function ifelse(p, t, f) {
    p = schemeTrue(p);
    if (p)
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function ifelse_hook(args, ssaScope, tools) {
    if (args[0] === undefined && args[1] === undefined && args[2] === undefined)
      return 'undefined';
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
  defineGlobalSymbol("bigint?", is_bigint, { evalArgs: 1, compileHook: is_bigint_hook, group: "pred-op", schemeOnly: true }, "is_bigint");
  function is_bigint(a, t = true, f = false) {
    if (typeof a === 'bigint')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_bigint_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_bigint', `typeof * === 'bigint'`);
  }

  defineGlobalSymbol("atom?", is_atom, { evalArgs: 1, compileHook: is_atom_hook, group: "pred-op" }, "is_atom");
  function is_atom(a, t = true, f = false) {
    if (isAtom(a))
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_atom_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_atom', `isAtom(*)`);
  }

  defineGlobalSymbol("undefined?", is_undefined, { evalArgs: 1, compileHook: is_undefined_hook, group: "pred-op", schemeOnly: true }, "is_undefined")
  function is_undefined(a, t = true, f = false) {
    if (a === undefined)
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_undefined_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_undefined', `* === undefined`);
  }

  defineGlobalSymbol("pair?", isCons, { evalArgs: 1, compileHook: is_pair_hook, group: "pred-op" }, "is-pair", "is-cons");
  function is_pair(a, t = true, f = false) {
    if (isCons(a))
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_pair_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_pair', `is_Cons(*)`);
  }

  defineGlobalSymbol("list?", isList, { evalArgs: 1, compileHook: is_list_hook, group: "pred-op" }, "is-list");
  function is_list(a, t = true, f = false) {
    if (isList(a))
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_list_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_list', `isList(*)`);
  }

  defineGlobalSymbol("null?", is_null, { evalArgs: 1, compileHook: is_null_hook, group: "pred-op" }, "is-null");  // SIOD clained it first. Maybe rethink the naming here.
  function is_null(a, t = true, f = false) {
    if (isNil(a))
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_null_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_null', `isNil(*)`);
  }

  defineGlobalSymbol("jsnull?", is_jsnull, { evalArgs: 1, compileHook: is_jsnull_hook, group: "pred-op", schemeOnly: true }, "is-jsnull");
  function is_jsnull(a, t = true, f = false) {
    if (a === null)
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_jsnull_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_jsnull', `* === null`);
  }

  defineGlobalSymbol("nullish?", is_nullish, { evalArgs: 1, compileHook: is_nullish_hook, group: "pred-op", schemeOnly: true }, "is-nullish");
  function is_nullish(a, t = true, f = false) {
    if (a == null)
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_nullish_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_nullish', `* == null`);
  }

  defineGlobalSymbol("boolean?", is_boolean, { evalArgs: 1, compileHook: is_boolean_hook, group: "pred-op", schemeOnly: true }, "is-boolean");
  function is_boolean(a, t = true, f = false) {
    if (typeof a === 'boolean')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_boolean_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_boolean', `typeof * === 'boolean'`);
  }

  defineGlobalSymbol("number?", is_number, { evalArgs: 1, compileHook: is_number_hook, group: "pred-op", schemeOnly: true }, "is-number");
  function is_number(a, t = true, f = false) {
    if (typeof a === 'number')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_number_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_number', `typeof * === 'number'`);
  }

  defineGlobalSymbol("numeric?", is_numeric, { evalArgs: 1, compileHook: is_numeric_hook, group: "pred-op", schemeOnly: true }, "is-numeric");
  function is_numeric(a, t = true, f = false) {
    if (typeof a === 'number' || typeof a === 'bigint')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_numeric_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_numeric', `typeof * === 'number' || typeof a === 'bigint`);
  }

  defineGlobalSymbol("string?", is_string, { evalArgs: 1, compileHook: is_string_hook, group: "pred-op", schemeOnly: true }, "is-string");
  function is_string(a, t = true, f = false) {
    if (typeof a === 'string')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_string_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_string', `typeof * === 'string'`);
  }

  defineGlobalSymbol("symbol?", is_symbol, { evalArgs: 1, compileHook: is_symbol_hook, group: "pred-op", schemeOnly: true }, "is-symbol");
  function is_symbol(a, t = true, f = false) {
    if (typeof a === 'symbol')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_symbol_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_symbol', `typeof * === 'symbol'`);
  }

  defineGlobalSymbol("function?", is_function, { evalArgs: 1, compileHook: is_function_hook, group: "pred-op", schemeOnly: true }, "is-function");
  function is_function(a, t = true, f = false) {
    if (typeof a === 'function')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_function_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_function', `typeof * === 'function'`);
  }

  defineGlobalSymbol("object?", is_object, { evalArgs: 1, compileHook: is_object_hook, group: "pred-op", schemeOnly: true }, "is-object");
  function is_object(a, t = true, f = false) {
    if (a !== null && typeof a === 'object')
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_object_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_object', `* !== null && typeof * === 'object'`);
  }

  defineGlobalSymbol("array?", is_array, { evalArgs: 1, compileHook: is_array_hook, group: "pred-op", schemeOnly: true }, "is-array");
  function is_array(a, t = true, f = false) {
    if (Array.isArray(a))
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_array_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_array', `Array.isArray(*)`);
  }

  defineGlobalSymbol("nan?", is_nan, { evalArgs: 1, compileHook: is_nan_hook, group: "pred-op", schemeOnly: true } , "is-NaN", "NaN?");
  function is_nan(a, t = true, f = false) {
    if (isNaN(a))
      return isPrimitive(t) ? t : _eval(t, this);
    else
      return isPrimitive(f) ? f : _eval(f, this);
  }
  function is_nan_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_nan', `isNaN(*)`);
  }

  defineGlobalSymbol("finite?", is_finite, { evalArgs: 1, compileHook: is_finite_hook, group: "pred-op", schemeOnly: true } , "is-finite");
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
  defineGlobalSymbol("begin", begin, { evalArgs: 0, compileHook: begin_hook, group: "core" });
  function begin(...forms) {
    let res = NIL;
    for (let i = 0, formsLength = forms.length; i < formsLength; ++i)
      res = _eval(forms[i], this);
    return res;
  }
  function begin_hook(args, ssaScope, tools) {
    let ssaResult = 'NIL';
    for (let i = 0; i < args.length; ++i)
      ssaResult = compileEval(args[i], ssaScope, tools);
    return ssaResult;
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
      if (!isCons(clause))
        throw new SchemeEvalError(`Bad clause in "cond" ${string(clause)}`);
    }
    for (let i = 0, clausesLength = clauses.length; i < clausesLength; ++i) {
      let clause = clauses[i];
      let predicateForm = clause[CAR], forms = clause[CDR];
      let evaled = _eval(predicateForm, this);
      if (schemeTrue(evaled)) {
        let res = NIL;
        for ( ; isCons(forms); forms = forms[CDR])
          res = _eval(forms[CAR], this);
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
      let predicateForm = clause[CAR], forms = clause[CDR];
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
          if (last) last = last[CDR] = cons(expr, NIL);
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

  // Can be inlined (if isList, isIterator, isCons, etc. are bound), but doesn't seem wise
  defineGlobalSymbol("append", append, { dontInline: true, group: "list-op" });
  function append(...lists) {
    let res = NIL, last;
    for (let list of lists) {
      if (isList(list)) {
        // Could handle as iterable, but faster not to
        for ( ; isCons(list); list = list[CDR])
          if (last) last = last[CDR] = cons(list[CAR], NIL);
          else res = last = cons(list[CAR], NIL);
      } else {
        if (!isIterable(list)) throw new SchemeEvalError(`Not a list or iterable ${list}`);
        for (let value of list) {
          let item = cons(value, NIL);
          if (last) last = last[CDR] = item;
          else res = last = item;
        }
      }
    }
    return res;
  }

  defineGlobalSymbol("last", last, { dontInline: true, group: "list-op" });
  function last(list) {
    let res = NIL;
    if (!list || isNil(list)) return NIL; // XXX check this.
    if (isCons(list)) {
      for ( ; isCons(list); list = list[CDR])
        res = list[CAR];
    } else {
      // Don't special-case string. Its iterator returns code points by combining surrogate pairs
      if (Array.isArray(list)) {
        if (list.length > 0)
          return list[list.length-1];
        return NIL;
      }
      if (!isIterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
      for (let value of list)
        res = value;
    }
    return res;
  }

  defineGlobalSymbol("butlast", butlast, { dontInline: true, group: "list-op" }); // TODO: needs unit test!
  function butlast(list) {
    let res = NIL, last;
    if (isList(list)) {
      for ( ; isCons(list) && isCons(list[CDR]); list = list[CDR])
        if (last) last = last[CDR] = cons(list[CAR], NIL);
        else res = last = cons(list[CAR], NIL);
    } else {
      if (!isIterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
      let previous, first = true;;
      for (let value of list) {
        if (!first) {
          let item = cons(previous, NIL);
          if (last) last = last[CDR] = item;
          else res = last = item;
        }
        first = false;
        previous = value;
      }
    }
    return res;
  }

  defineGlobalSymbol("length", length, { dontInline: true, group: "list-op" });
  function length(list) {
    let n = 0;
    if (isList(list)) {
      for ( ; isCons(list); list = list[CDR])
        n += 1;
    } else {
      // Don't special-case string. Its iterator returns code points by combining surrogate pairs
      if (Array.isArray(list) && list.length > 0)
        return list.length;
      if (!isIterable(list)) throw new TypeError(`Not a list or iterable ${string(list)}`);
      for (let _ of list)
        n += 1;
    }
    return n;
  }

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

  defineGlobalSymbol("reverse", reverse, { dontInline: true, group: "list-op" });
  function reverse(list) {
    let res = NIL;
    for ( ; isCons(list); list = list[CDR])
      res = cons(list[CAR], res)
    return res;
  }

  defineGlobalSymbol("nreverse", in_place_reverse, { dontInline: true, group: "list-op" });  // Name from SIOD
  function in_place_reverse(list) {
    let res = NIL;
    while (isCons(list)) {
      let next = list[CDR];
      list[CDR] = res;
      res = list;
      list = next;
    }
    return res;
  }

  defineGlobalSymbol("copy-list", copy_list, { dontInline: true, group: "list-op" });  // TODO: unit tests!
  function copy_list(list) {
    let res = NIL, last;
    if (isNil(list)) return NIL;
    if (isCons(list)) {
      for ( ; isCons(list); list = list[CDR]) {
        let item = cons(list[CAR], NIL);
        if (last) last = last[CDR] = item;
        else res = last = item;
      }
      return res;
    }
    if (isIterable(list)) {
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

  // XXX TODO: member and memq are almost certainly wrong. Need to find out about SIOD equality.
  // (member key list)
  //     Returns the portion of the list where the car is equal to the key, or () if none found.
  defineGlobalSymbol("member", member, { dontInline: true, group: "list-op" });
  function member(key, list) {
    for ( ; isCons(list); list = list[CDR])
      if (key === list[CAR])   // TODO: == or ===?
        return list;
    return NIL;
  }

  // (memq key list)
  //     Returns the portion of the list where the car is eq to the key, or () if none found.
  defineGlobalSymbol("memq", memq, { dontInline: true, group: "list-op" });
  function memq(key, list) {
    for ( ; isCons(list); list = list[CDR])
      if (key === list[CAR])
        return list;
    return NIL;
  }

  // (nth index list)
  //     Reference the list using index, with the first element being index 0.
  defineGlobalSymbol("nth", nth, { dontInline: true, group: "list-op" });
  function nth(index, list) {
    if (typeof index !== 'number' || Math.trunc(index) !== index)
      throw new TypeError(`Not an integer ${string(index)}`);
    if (index < 0) throw new RangeError(`nth`);
    if (isList(list)) {
      for ( ; index > 0 && isCons(list); list = list[CDR])
        index -= 1;
      if (isCons(list))
        return list[CAR];
  ``} else if (Array.isArray(list)) {
      if (index < list.length)
        return list[index];
    } else {
      if (!isIterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
      for (let value of list) {
        if (index <= 0)
          return value;
        index -= 1;
      }
    }
    throw new RangeError(`nth`);
  }

  // (apropos substring) -- Returns a list of all symbols containing the given substring
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

  // (map fn list1 list2 ...)
  defineGlobalSymbol("map", map, { dontInline: true, group: "list-op" }, "mapcar"); // mapcar alias for SIOD compatibility
  function map(fn, ...lists) {
    // Actually, this will work for any iterables, and lists are iterable.
    let result = NIL, last;
    for (let list of lists) {
      if (isList(list)) {
        // Could just let the list iterator handle it but might as well just follow the Cons chain
        // and not have to manufacture an iterator.
        for ( ; isCons(list); list = list[CDR]) {
          let item = list[CAR];
          item = fn.call(this, item);
          item = cons(item, NIL)
          if (last) last = last[CDR] = item;
          else result = last = item;
        }
      } else {
        if (!isIterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
        for (let item of list) {
          item =  fn.call(this, item);
          item = cons(item, NIL);
          if (last) last = last[CDR] = item;
          else result = last = item;
        }
      }
    }
    return result;
  }

  // Same as map but results in an Array
  defineGlobalSymbol("array-map", array_map, { dontInline: true, group: "list-op" });
  function array_map(fn, ...lists) {
    let res = [];
    // TODO: does not currently special-case the list case.
    // This is intentional for now as it ensures lists-as-iterables is sufficiently tested.
    for (let list of lists) {
      for (let item of list) {
        item = fn.call(this, item);
        res.push(item);
      }
    }
    return res;
  }

  // (filter fn list1 list2 ...)
  defineGlobalSymbol("filter", filter, { dontInline: true, group: "list-op" });
  function filter(predicateFn, ...lists) {
    // Actually, this will work for any iterables, and lists are iterable.
    let result = NIL, last;
    for (let list of lists) {
      if (isList(list)) {
        // Could just let the list iterator handle it but might as well just follow the Cons chain
        // and not have to manufacture an iterator.
        for ( ; isCons(list); list = list[CDR]) {
          let item = list[CAR];
          if (schemeTrue(predicateFn(item))) {
            item = cons(item, NIL);
            if (last) last = last[CDR] = item;
            else result = last = item;
          }
        }
      } else {
        if (!isIterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
        for (let item of list) {
          if(schemeTrue(predicateFn(item))) {
            item = cons(item, NIL);
            if (last) last = last[CDR] = item;
            else result = last = item;
          }
        }
      }
    }
    return result;
  }

  // TODO: lazy-filter?

  // Turns iterable objects like arrays into lists, recursively to "depth" (default 1) deep.
  defineGlobalSymbol("to-list", to_list, { dontInline: true, group: "list-op" });
  function to_list(obj, depth = 1) {
    if (depth <= 0) return obj;
    if (isNil(obj) || isCons(obj)) return obj;
    if (typeof obj === 'object') {
      if (isCons(obj)) return obj;  // Careful; Cons is iterable itself
      let list = NIL, last;
      if (!isIterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
      for (let value of obj) {
        if (depth > 1 && isIterable(value))
          value = to_list.call(this, value, depth-1);
        if (last) last = last[CDR] = cons(value, NIL);
        else list = last = cons(value, NIL);
      }
      return list;
    }
    return NIL;
  }

  // Turns iterable objects like lists into arrays, recursively to "depth"
  defineGlobalSymbol("to-array", to_array, { dontInline: true, group: "list-op" });
  function to_array(obj, depth = 1) {
    if (depth <= 0) return obj;
    res = [];
    for (let item of obj) {
      if (depth > 1 && isIterable(item))
        value = to_array.call(this, item, depth-1);
      res.push(item);
    }
    return res;
  }

  // (let (binding1 binding2 ...) form1 form2 ...) -- let* behavior
  //     (let ((x 10)
  //           (y 20))
  //       (+ x y))
  // Because this implementation uses a scope chain instead
  // of an environment, each kind of let is as powerful as "letrec".
  //
  // TODO: Reconsider that; it's easy to implement let and let*;
  // it's just that I think they're bad ideas, historically baked-in.
  // But it's possible there's existing SIOD code that depends on the
  // behavior of let and let*, for instance,
  //    (let ((x (something-that-uses-outer-scope-x) ...
  //
  // "letrec" can be partially-applied, returning a function that
  // evaluates its arguments in the let scope!
  //
  defineGlobalSymbol("letrec", letrec, { evalArgs: 0, compileHook: letrec_hook, group: "core", schemeOnly: true }, "let", "let*");
  function letrec(bindings, form, ...forms) {
    let scope = newScope(this, "letrec-scope");
    for ( ; isCons(bindings); bindings = bindings[CDR]) {
      let binding = bindings[CAR];
      if (!isCons(binding))
        throw new SchemeEvalError(`Bad binding ${string(binding)}`);
      let boundVar = binding[CAR], bindingForms = binding[CDR];
      if (typeof boundVar !== 'symbol')
        throw new SchemeEvalError(`Bad binding ${string(binding)}`);
      let val = NIL;
      for ( ; isCons(bindingForms); bindingForms = bindingForms[CDR])
        val = _eval(bindingForms[CAR], scope);
      scope[boundVar] = val;
    }
    let res = _eval(form, scope);
    for (let i = 0, formsLength = forms.length; i < formsLength ; ++i)
      res = _eval(forms[i], scope);
    return res;
  }
  function letrec_hook(args, ssaScope, tools) {
    let emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
    if (args.length < 2) throw new SchemeCompileError(`Bad letrec`);
    let bindings = args[0];
    let ssaResult = newTemp("letrec");
    ssaScope = newScope(ssaScope, "compiler-letrec-scope");
    let ssaTmpScope = newTemp("scope_tmp");
    emit(`let ${ssaTmpScope} = scope;`);
    emit(`let ${ssaResult} = NIL; { // letrec`);
    let saveIndent = tools.indent;
    tools.indent += '  ';
    emit(`let scope = newScope(${ssaTmpScope}, "compiled-letrec-scope");`);
    for( ; isCons(bindings); bindings = bindings[CDR]) {
      let binding = bindings[CAR];
      if (!isCons(binding))
        throw new SchemeCompileError(`Bad binding ${string(binding)}`)
      let boundVar = binding[CAR], bindingForms = binding[CDR];
      if (typeof boundVar !== 'symbol')
        throw new SchemeEvalError(`Bad binding ${string(binding)}`);
      let ssaBoundVar = newTemp(boundVar);
      emit(`let ${ssaBoundVar} = NIL;`);
      for ( ; isCons(bindingForms); bindingForms = bindingForms[CDR]) {
        let ssaVal = compileEval(bindingForms[CAR], ssaScope, tools);
        emit(`${ssaBoundVar} = ${ssaVal};`);
      }
      let paramAtom = use(bind(boundVar));
      ssaScope[boundVar] = ssaBoundVar;
      emit(`scope[${paramAtom}] = ${ssaBoundVar};`);
    }
    for (let i = 1; i < args.length; ++i) {
      let ssaVal = compileEval(args[i], ssaScope, tools);
      emit(`${ssaResult} = ${ssaVal};`);
    }
    tools.indent = saveIndent;
    emit(`}`);
    return ssaResult;
  }

  // Something like this would be nice, but it's not quite right
  //  let setSymWithWith = new Function("symbol", "value", "scope",
  //    "with (scope) { return symbol = value }");
  // The point is that the JavaScript runtime HAS a suitable primitive; I just don't
  // think you can get at it directly from user code.

  defineGlobalSymbol("set'", setq, { evalArgs: 0, compileHook: setq_hook, group: "core" }, "setq");
  function setq(symbol, valueForm, ...values) {
    let value = _eval(valueForm, this);
    for (let valueForm of values)
      value = _eval(valueForm, this);
    let result = setSym(symbol, value, this);
    return result;
  }
  function setq_hook(body, ssaScope, tools) {
    let emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
    if (!isCons(body))
      throw new SchemeCompileError(`Bad setq params ${body}`);
    let varSym = body[CAR], valForms = body[CDR];
    let ssaValue = 'NIL';
    for (let form of valForms)
      ssaValue = compileEval(form, ssaScope, tools);
    let boundVar = ssaScope[varSym];
    if (boundVar) {
      emit(`${boundVar} = ${ssaValue};`);
    } else {
      let ssaSetSym = use(bind(setSym));
      emit(`${ssaSetSym}.call(this, ${ssaValue});`);
    }
    return ssaValue;
  }

  defineGlobalSymbol("set", set, {group: "core" });
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
    return val;
  }

  // (qsort list predicate-fcn access-fcn)
  //   "qsort" is a lie for API compatibility with SIOD, but this sort has
  //   comparable performance and is excellent with partially-sorted lists.
  defineGlobalSymbol("mergesort", mergesort, { dontInline: true, group: "list-op" }, "sort", "qsort");
  function mergesort(list, predicateFn = undefined, accessFn = undefined) {
    if (isNil(list)) return NIL;
    // Sort Arrays as Arrays
    if (Array.isArray(list))
      return in_place_mergesort(list.slice(0), predicateFn, accessFn);
    // Lists and other iterables are sorted as lists
    if (isCons(list))
      return in_place_mergesort(copy_list(list), predicateFn, accessFn);
    let copied = NIL, last;
    if (!isIterable(list)) throw new TypeError(`Not a list or iterable ${list}`);
    for (let item of list) {
      item = cons(item, NIL);
      if (last) last = last[CDR] = item;
      else copied = last = item;
    }
    return in_place_mergesort(copied, predicateFn, accessFn);
  }

  defineGlobalSymbol("in-place-mergesort", in_place_mergesort, { dontInline: true, group: "list-op" }, "in-place-sort", "nsort");
  function in_place_mergesort(list, predicateFn = undefined, accessFn = undefined) {
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
    if (Array.isArray(list)) {
      // ES10 stipulates that it only cares whether the compare function
      // returns > 0, which means move "b"  before "a," or <= zero,
      // which means leave "a" before "b". There's no ned to distinguish
      // the "equal" case. Which is nice for us because the "before"
      // predicate doesn't distinguish that case (without a second call
      // with reversed arguments.)
      list.sort((a,b) => before.call(scope, a, b) ? -1 : 1);
      return list;
    }
    if (isCons(list)) {
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
    while (isCons(list)) {
      // Accumulate a run that's already sorted.
      let run = list, runTail = list;
      list = list[CDR];
      while (isCons(list)) {
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
      while (isCons(left) && isCons(right)) {
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
      if (isCons(left)) {
        if (last) last[CDR] = left;
        else merged = left;
      } else if (isCons(right)) {
        if (last) last[CDR] = right;
        else merged = right;
      }
      return merged;
    }
  }

  // Because lists can be circular and stack depths are finite, equal
  // must have bounds on its recursion and looping. The bounds are large and
  // user-optionble, but they are finite. If the bounds are exceeded, deep_eq
  // returns "undefined," which is "falsey" but distinguishable from
  // "false."
  // A client can pass in a "report" object which is filled-in with a report
  // on where and how objects differ. This has proved useful for unit testing
  // but can be generally useful. The report can also contain a strCmp
  // property that determines how strings are compared, for instance you can ignore case
  // or leading and trailing whitespace. Sometimes you're playing horseshoes.
  // You can also opt that NaNs are considered equal.
  defineGlobalSymbol("equal", equal, { dontInline: true });
  function equal(a, b, maxDepth = 10000, maxLength = 10000000, report = {}) {
    let stringCompare = report.stringCompare ?? ((a, b) => a === b);
    let NaNsEqual = report.NaNsEqual;
    return deep_eq(a, b, 0, 0);
    function deep_eq(a, b, depth, length) {
      if (depth > maxDepth) {
        report.maxedOut = report.maxDepth = depth;
        return undefined;
      }
      if (length > maxLength) {
        report.maxedOut = report.maxLength = length;
        return undefined;
      }
      // Any object or class can have an EQUAL_FUNCTION
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
      if (isList(a)) {
        if (!isList(b)) {
          report.a = a, report.b = b;
          report.valuesDiffer = true;
          return false;
        }
        let i = 0, aRest = a, bRest = b;
        for ( ; isCons(aRest) && isCons(bRest); ++i, ++length, aRest = aRest[CDR], bRest = bRest[CDR]) {
          let res = deep_eq(aRest[CAR], bRest[CAR], depth+1, length);
          if (!res) {
            report.list = a, report.a = aRest[CAR], report.b = bRest[CAR];
            report.elementsDiffer = i;
            report = {};  // Don't let this report get smashed by caller.
            return res;
          }
        }
        if (isNil(aRest) && isNil(bRest)) // possible imposter nils; they are just fine
          return true;
        return deep_eq(aRest, bRest, depth+1, length+1);
      } else if (isList(b)) {
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
      if (Array.isArray(a)) {
        if (a.length != b.length) {
          report.a = a, report.b = b;
          report.aVal = a.length, report.bVal = b.length;
          report.property = 'length';
          report.valuesDiffer = true;
          report = {};
          return false;
        }
        // Array elements are also properties so fall through to Object comparison.
        // This also distinguishes Arrays that have missing elements.
      }
      // Object compare
      for (let property of Object.getOwnPropertyNames(a).concat(Object.getOwnPropertySymbols(a))) {
        if (!b.hasOwnProperty(property)) {
          report.a = a, report.b = b;
          report.aVal = a[property];
          report.bMissingProperty = property;
          report = {};
          return false;
        }
      }
      for (let property of Object.getOwnPropertyNames(b).concat(Object.getOwnPropertySymbols(b))) {
        if (!a.hasOwnProperty(property)) {
          report.a = a, report.b = b;
          report.bVal = b[property];
          report.aMissingProperty = property;
          report = {};
          return false;
        }
        let res = deep_eq(a[property], b[property], depth+1, length);
        if (!res) {
          report.a = a, report.b = b;
          report.aVal = a[property], report.bVal = b[property];
          report.elementsDiffer = property;
          report.valuesDiffer = true;
          report = {};
          return res;
        }
      }
      return true;
    }
  }

  function makeConsImposter(fn, form) {
    fn[PAIR] = fn[LIST] = true;
    fn[CAR] = form[CAR];
    fn[CDR] = form[CDR];
    return fn;
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

  // (throw value) -- JavaScript style
  defineGlobalSymbol("throw", js_throw);
  function js_throw(value) { throw value; return undefined; } // "return" lets compiler use template

  // (catch (var forms) forms) -- JavaScript style
  defineGlobalSymbol("catch", js_catch, { evalArgs: 0, compileHook: js_catch_hook });
  function js_catch(catchClause, form, ...forms) {
    if (!isCons(catchClause))
      throw new SchemeEvalError(`Bad catch clause ${string(catchClause)}`);
    let catchVar = catchClause[CAR], catchForms = catchClause[CDR];
    if (!isCons(catchForms))
      throw new SchemeEvalError(`Bad catch clause ${string(catchClause)}`);
    if (!isCons(catchForms))
      throw new SchemeEvalError(`Bad catch clause ${string(catchClause)}`);
    let val;
    try {
      val = _eval(form, this);
      for (let i = 0, formsLength = forms.length; i < formsLength; ++i)
        val = _eval(forms[i], this);
    } catch (e) {
      let scope = newScope(this, "js-catch-scope");
      scope[catchVar] = e;
      for ( ; isCons(catchForms); catchForms = catchForms[CDR])
        val = _eval(catchForms[CAR], scope);
    }
    return val;
  }
  function js_catch_hook(args, ssaScope, tools) {
    let emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
    if (args.length < 2) throw new LogicError(`Bad js_catch`);
    let catchClause = args[0];
    if (!isCons(catchClause) && isCons(catchClause[CDR]))
      throw new SchemeCompileError(`Bad catch clause ${string(catchClause)}`);
    let catchVar = catchClause[CAR], catchForms = catchClause[CDR];
    let ssaCatchSym = newTemp(catchVar);
    let ssaResult = newTemp('js_catch'), ssaValue = 'NIL';
    let ssaTmpScope = newTemp('tmp_scope');
    emit(`let ${ssaTmpScope} = scope;`);
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
    emit(`let scope = newScope(${ssaTmpScope}, "compiled-js-catch-scope");`);
    let ssaCatchAtom = use(bind(catchVar));
    emit(`scope[${ssaCatchAtom}] = ${ssaCatchSym};`);
    let ssaCatchVal = 'NIL';
    for (let form of catchForms)
      ssaCatchVal = compileEval(form, ssaScope, tools);
    emit(`${ssaResult} = ${ssaCatchVal};`);
    tools.indent = saveIndent;
    emit(`}`);
    return ssaResult;
  }

  // (define variable value)
  // (define (fn args) forms)
  defineGlobalSymbol("define", define, { evalArgs: 0, dontInline: true });
  function define(defined, value) {
    let scope = this, name = defined;
    if (isCons(defined)) {
      name = defined[CAR];
      let params = defined[CDR];
      value = lambda.call(scope, params, value);
    } else {
      value = _eval(value, scope);
    }
    if (typeof name === 'string') name = Atom(name);
    // Prevent a tragic mistake that's easy to make by accident. (Ask me how I know!)
    if (name === QUOTE_ATOM) throw new SchemeEvalError("Can't redefine quote");
    if (typeof name !== 'symbol')
      throw new TypeError(`Must define symbol or string ${string(defined)}`);
    if (value != null &(typeof value === 'function' || typeof value === 'object'))
      value[NAMETAG] = name.description;
    globalScope[name] = value;
    // Make available to JavaScript as well
    let { jsName } = normalizeExportToJavaScriptName(name);
    globalScope[jsName] = value;
    return name;
  }

  // (compile (fn args) forms) -- defines a compiled function
  // (compile lambda) -- returns a compiled lambda expression
  defineGlobalSymbol("compile", compile, { evalArgs: 0, dontInline: true });
  function compile(nameAndParams, forms) {
    if (!isCons(nameAndParams)) new TypeError(`First parameter must be a list ${forms}`);
    let name = Atom(nameAndParams[CAR]);
    let args = nameAndParams[CDR];
    if (typeof name !== 'symbol') new TypeError(`Function name must be an atom or string ${forms}`)    
    let form = list(LAMBDA_ATOM, args, forms);
    let compiledFunction = compile_lambda.call(this, name, name.description, form);
    compiledFunction[NAMETAG] = name;
    globalScope[name] = compiledFunction;
    // Make available to JavaScript as well
    let { jsName } = normalizeExportToJavaScriptName(name);
    globalScope[jsName] = compiledFunction;
    globalScope[name] = compiledFunction;
    return name;
  }

  //
  // This is where the magic happens
  //
  // Beware that compileEval closely parallels this function, if you make a change
  // here you almost certainly need to make a corresponding one there.
  //

  exportAPI("eval", _eval, { dontInline: true });
  function _eval(form, scope = this) {
    // Can't be called "eval" because "eval", besides being a global definition,
    // is effectively a keyword in JavaScript.
    if (isNil(form)) return NIL;  // Normalizes NIL imposters to "the" NIL, for no particular reason
    if (isPrimitive(form)) return form;
    if (typeof form === 'symbol') { // atom resolution is the most common case
      let val = scope[form];
      if (val === undefined) checkUndefinedInScope(form, scope);
      return val;
    }
    if (TRACE_INTERPRETER)
      console.log("EVAL", string(form));
    if (isCons(form)) {
      let fn = form[CAR];
      if (fn === QUOTE_ATOM) { // QUOTE is a special function that will do this but catch it here anyway.
        if (!isCons(form)) throwBadForm();
        return form[CDR][CAR];
      }
      fn = _eval(fn, scope);
      if (typeof fn !== 'function') throwBadForm();
      // See makeParameterDescriptor for the truth, but
      //   parameterDescriptor = (evalCount << 16) | requiredCount
      let parameterDescriptor = fn[PARAMETER_DESCRIPTOR] ?? examineFunctionForParameterDescriptor(fn);
      let requiredCount = parameterDescriptor & 0xffff;
      let evalCount = parameterDescriptor >> 15 >>> 1;  // restores MAX_INTEGER to MAX_INTEGER
      if (fn[CAR] === SCLOSURE_ATOM) {
        let fnCdr = fn[CDR];
        if (!isCons(fnCdr)) throwBadForm();
        scope = fnCdr[CAR];
      }
      // Run through the arg list evaluating args
      let  args = form[CDR], argCount = 0;
      for (let tmp = args; isCons(tmp); tmp = tmp[CDR])
        ++argCount;
      let argv = new Array(argCount);
      for (let i = 0; isCons(args); ++i, args = args[CDR]) {
        let item = args[CAR];
        if (i < evalCount)
          item = _eval(item, scope);
        argv[i] = item;
      }
      let fName = fn[NAMETAG] ?? fn.name;
      let jitCompiled = fn[JITCOMPILED];
      if (jitCompiled) fn = jitCompiled;
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
      // This is a bit involved, but it doesn't happen often
      const boundArgv = argv;
      let closure = (...args) => fn.apply(scope, [...boundArgv, ...args]);
      // A closure function leads a double life: as a closure function but also a closure form!
      // Dig out the original function's closure, if it had one.
      let closureBody = fn[CDR];
      let closureParams = NIL, closureForms;
      if (closureBody) {
        scope = closureBody[CAR];
        scope = newScope(scope, "closure-scope");
        let lambdaBody = closureBody[CDR];
        if (fn[CAR] === SCLOSURE_ATOM)  // Skip the evalCount param
          lambdaBody = lambdaBody[CDR];
        closureParams = lambdaBody[CAR];
        closureForms = lambdaBody[CDR];
        for (let i = 0; i < argCount; ++i, closureParams = closureParams[CDR]) {
          if (!isCons(closureParams)) throw new LogicError(`Shouldn't happen`);
          let param = closureParams[CAR];
          if (isCons(param) && param[CAR] === QUESTION_ATOM && isCons(param[CDR]))
            param = param[CDR][CAR];
          if (typeof param !== 'symbol') throw new LogicError(`Shouldn't happen`);
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
        closureForms = cons(Atom(`{*js-function-${fName}(${paramStr})*}`), NIL);
        if (restParam)
          closureParams = Atom(restParam);
        for (let i = params.length; i > argCount; --i)
          closureParams = cons(Atom(params[i-1]), closureParams);
        for (let i = 0; i < argCount; ++i)
          scope[Atom(params[i])] = argv[i];
      }
      if (evalCount !== MAX_INTEGER) {
        evalCount -= argCount;
        if (evalCount < 0)
          evalCount = 0;
        closure[CAR] = SCLOSURE_ATOM;
        closure[CDR] = cons(scope, cons(evalCount, cons(closureParams, closureForms)));
      } else {
        closure[CAR] = CLOSURE_ATOM;
        closure[CDR] = cons(scope, cons(closureParams, closureForms));
      }
      closure[LIST] = closure[PAIR] = true;
      closure[CLOSURE_ATOM] = true; // marks closure for special "printing"
      requiredCount -= argCount;
      if (requiredCount < 0) throw new LogicError(`Shouldn't happen`);
      closure[PARAMETER_DESCRIPTOR] = makeParameterDescriptor(requiredCount, evalCount);
      return closure;
    }

    // Special eval for JS arrays and objects:
    //   Values that are evaluated and placed in
    //   a new Object or Array in correspoding position.
    // TODO: Investigate Symbol.species (also for map, etc.)
    if (form !== null && typeof form === 'object') {
      if (Array.isArray(form)) {
        let res = [];
        for (let element of form)
          res.push(_eval(element, scope));
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
    throw new LogicError(`Shouldn't happen. All cases should be handled above`);

    function throwBadForm() {
      throw new SchemeEvalError(`Bad form ${string(form)}`);
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

  function examineFunctionForParameterDescriptor(fn, evalCount = MAX_INTEGER) {
    //let res = { name, params, restParam, value, body, printBody, printParams, native, requiredCount };
    let { requiredCount, native } = analyzeJSFunction(fn);
    if (native)
      requiredCount = 0;
    return fn[PARAMETER_DESCRIPTOR] = makeParameterDescriptor(requiredCount, evalCount);
  }

  function makeParameterDescriptor(requiredCount, evalCount = MAX_INTEGER55) {
    if (requiredCount < 0 || requiredCount >= 0xffff)
      throw new LogicError(`RequiredCount out of range`);
    if (evalCount < 0 || (evalCount !== MAX_INTEGER && evalCount >= 0xffff))
      throw new LogicError(`Too many evaluated paramaters`);
    return (evalCount << 16) | requiredCount;
  }

  defineGlobalSymbol("apply", apply, { dontInline: true });
  function apply(fn, args, scope = this) {
    let argv = [];
    for ( ;isCons(args); args = args[CDR])
      argv.push(args[CAR])
    let fName;
    if (TRACE_INTERPRETER) {
      fName = fn[NAMETAG] ?? fn.name;
      let logArgs = [ "APPLY (api)", fName, ...argv ];
      console.log.apply(scope, logArgs);
    }
    let result = fn.apply(scope, argv);
    if (TRACE_INTERPRETER)
      console.log("RETURNS", fName, result);
    return result;
  }

  // (\ (params) (form1) (form2) ...)
  defineGlobalSymbol(LAMBDA_CHAR, lambda, { evalArgs: 0, dontInline: true }, "\\", "lambda");
  function lambda(params, ...forms) {
    let body = NIL;
    for (let i = forms.length; i > 0; --i)
      body = cons(forms[i-1], body);
    let lambda = cons(LAMBDA_ATOM, cons(params, body));
    let scope = this;
    let schemeClosure = cons(CLOSURE_ATOM, cons(scope, lambda[CDR]));
    return makeJsClosure(scope, params, lambda, body, schemeClosure);
  }

  // (\# evalCount (params) (body1) (body2) ...)
  defineGlobalSymbol(LAMBDA_CHAR+"#", special_lambda, { evalArgs: 0, dontInline: true }, "\\\\");
  function special_lambda(evalCount, params, ...forms) {
    let body = NIL;
    for (let i = forms.length; i > 0; --i)
      body = cons(forms[i-1], body);
    let lambda = cons(SLAMBDA_ATOM, cons(evalCount, cons(params, body)));
    if (!isCons(body)) throwBadLambda(lambda);
    let scope = this;
    let schemeClosure = cons(SCLOSURE_ATOM, cons(scope, lambda[CDR]));
    return makeJsClosure(scope, params, lambda, body, schemeClosure, evalCount);
  }

  //
  // Beware that compileClosure closely parallels this function. If you make a change
  // here, you almost certainly need to make a change there. "string" also
  // has special handling for "printing" closures. In particular, closures are
  // decorated with a CLOSURE_ATOM symbol to clue the string function to print
  // it as a closure. Closures are also decorated with CAR, CDR, LIST and PAIR
  // so that they look exactly like lists to the SchemeJS runtime.
  //
  function makeJsClosure(scope, lambdaParams, lambda, forms, schemeClosure, evalCount = MAX_INTEGER) {
    // Examine property list and throw any errors now rather than later.
    // In general, do more work here so the closure can do less work when executed.
    if (typeof lambdaParams === 'symbol') { // curry notation; normalize to classic
      lambdaParams = cons(lambdaParams, NIL);
      forms = cons(forms, NIL);
    }
    let params = lambdaParams, paramCount = 0, requiredCount;
    let paramv = [], restParam;
    for ( ; isCons(params); params = params[CDR]) {
      let param = params[CAR];
      if (isCons(param)) {
        if (!param[CAR] === QUESTION_ATOM && isCons(param[CDR] && typeof param[CDR][CAR] === 'symbol'))
          throwBadLambda(lambda, `what's this?  ${string(param)}`);
        if (!requiredCount)
          requiredCount = paramCount;
        param = param[CDR][CAR];
      } else if (typeof param !== 'symbol') {
        throwBadLambda(lambda, `parameter ${string(param)} not an atom`);
      }
      paramCount += 1;
      paramv.push(param.description);
    }
    if (typeof params === 'symbol')
      restParam = params.description;
    else if (!isNil(params))
      throwBadLambda(`bad "rest" parameter ${string(params)}`);
    if (!requiredCount)
      requiredCount = paramCount;
    let jitCount = jitThreshold ? jitThreshold|0 : undefined;
    function jsClosure(...args) {
      scope = newScope(scope, "lambda-scope");
      if (jitThreshold !== undefined) {  // Disable by optioning jitThreshold as undefined
        let jitFn = jsClosure[JITCOMPILED];
        // SchemeJS will almost always call the jitFn directly, but external JS will still call this closure.
        if (jitFn)
          return jitFn.apply(this, args);
        if (--jitCount < 0) {
          jitCount = jitThreshold;
          jsClosure[JITCOMPILED] = compile_lambda.call(scope, undefined, jsClosure[NAMETAG], lambda, jsClosure);
        }
      }
      let params = lambdaParams, i = 0, argLength = args.length;
      for ( ; isCons(params); ++i, params = params[CDR]) {
        let param = params[CAR], optionalForms, arg;
        if (isCons(param) && param[CAR] === QUESTION_ATOM) {
          let paramCdr = param[CDR];
          param = paramCdr[CAR];
          optionalForms = paramCdr[CDR];
        }
        arg = args[i];  // undefined if i >= length OR if deliberately undefined
        if (arg === undefined) {
          arg = NIL;
          if (optionalForms) {
          for ( ; isCons(optionalForms); optionalForms = optionalForms[CDR])
            arg = _eval(optionalForms[CAR], scope);
          }
        }
        scope[param] = arg;
      }
      if (typeof params === 'symbol') {  // rest param
        let rest = NIL;
        for (let j = argLength; j > i; --j)
          rest = cons(args[j-1], rest);
        scope[params] = rest;
      }
      let result = NIL;
      for (let form of forms)
        result = _eval(form, scope);
      return result;
    }
    jsClosure[PARAMETER_DESCRIPTOR] = makeParameterDescriptor(requiredCount, evalCount);
    jsClosure[CAR] = schemeClosure[CAR];
    jsClosure[CDR] = schemeClosure[CDR];
    jsClosure[LIST] = jsClosure[PAIR] = true;
    jsClosure[CLOSURE_ATOM] = true; // marks closure for special "printing"
    // Because the closure has a generic (...args) parameter, the compiker needs more info
    // to be able to create binding closures over it.
    jsClosure[COMPILE_INFO] = { params: paramv, restParam };
    return jsClosure;
  }

  function throwBadLambda(lambda, msg) { throw new SchemeEvalError(`Bad lambda ${lambda}` + (msg ? `, ${msg}` : '')) }

  defineGlobalSymbol("closure?", is_closure, { evalArgs: 1, compileHook: closureP_hook }, "is_closure")
  function is_closure(a, t = true, f = false) {
    if (isClosure(a)) return typeof t === 'boolean' ? t : _eval(t, this);
    else return typeof f === 'boolean' ? f : eval(f, this);
  }
  function closureP_hook(args, ssaScope, tools) {
    return conditionalHooks(args, ssaScope, tools, 'is_closure', `is_closure(*)`);
  }
  exportAPI("isClosure", isClosure);
  function isClosure(obj) {
    return isCons(obj) && (obj[CAR] === CLOSURE_ATOM || obj[CAR] === SCLOSURE_ATOM);
  }

  const ESCAPE_STRINGS = { t: '\t', n: '\n', r: '\r', '"': '"', '\\': '\\', '\n': '' };
  const STRING_ESCAPES = (() => {
    let res = {};
    for (let [key, value] of Object.entries(ESCAPE_STRINGS))
      res[value] = '\\' + key;
    return res;
  })();

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
  defineGlobalSymbol("to-string", a => string(a));
  exportAPI("string", string);
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
        // MUST do this before the isNil test, which will cause eager evaluation of
        // a LazyIteratorList, cause it to call next() and mutate into something else.
        if (obj[SUPERLAZY])
          return put("(...)");
        if (isNil(obj)) return put("()");
        if (obj instanceof Scope) {
          let symStrs = "";
          if (obj !== globalScope) {
            for (let sym of Object.getOwnPropertySymbols(obj)) {
              if (!isAtom(sym)) continue; // Not an atom (e.g. SCOPE_IS_SYMBOL)
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
          return put(`{*${obj[SCOPE_IS_SYMBOL]}*${symStrs}}`);
        }
        if (obj[LAZYCDR]) {
          put("(");
          sep = "";
          if (obj[LAZYCAR])
            put("..");  // .. signifies a lazy car
          else
            toString(obj[CAR], maxCarDepth-1, maxCdrDepth);
          sep = " ";
          return put("...)", true);
        }
        if (isCons(obj)) {
          put("(");
          indent += indentMore;
          sep = "";
          if (!obj[LAZYCAR]) {
            let objCar = obj[CAR];
            if ((objCar === LAMBDA_ATOM || objCar === SLAMBDA_ATOM ||
                objCar === CLOSURE_ATOM || objCar === SCLOSURE_ATOM)
                  && isCons(obj[CDR])) {
              // Specal treatment of lambdas and closures with curry notation
              if (objCar === CLOSURE_ATOM|| objCar === SCLOSURE_ATOM) {
                if (isCons(obj[CDR][CDR])) {
                  let evalCount, scopeCons = obj[CDR];
                  if (objCar === SCLOSURE_ATOM) {
                    evalCount = obj[CDR][CAR];
                    scopeCons = obj[CDR][CDR];
                  }
                  let params = scopeCons[CDR][CAR];
                  if (typeof params === 'symbol') {
                    sep = "";
                    if (objCar === SCLOSURE_ATOM) {
                      toString(evalCount, maxCarDepth, maxCdrDepth-1);
                      sep = " ";
                    }
                    toString(objCar);  // %%closure or %%%closure
                    sep = "";
                    if (obj[COMPILED]) put(`{*compiled-${obj[COMPLIED]}*}`);
                    sep = " ";
                    toString(scopeCons[CAR], maxCarDepth-1, maxCdrDepth-2);  // scope
                    sep = " ";
                    toString(params, maxCarDepth-1, maxCdrDepth-2);  // actually the atom
                    sep = ""; put(".");
                    toString(scopeCons[CDR][CDR], maxCarDepth, maxCdrDepth-3);  // the form
                    sep = ""; put(")");
                    return;
                  }
                }
              }
              let str = '', params = obj[CDR][CAR], forms = obj[CDR][CDR];
              if (objCar === LAMBDA_ATOM) str += lambdaStr;
              if (objCar === SLAMBDA_ATOM) str += lambdaStr + '#';
              if (typeof params === 'symbol') {  // curry notation
                str += `${params.description}.`;
                put(str);
                indent += indentMore;
                toString(forms, maxCarDepth, maxCdrDepth-1);
                sep = ""; put(")");
                indent = saveIndent;
                return;
              }
            }
          }
          while (isCons(obj)) {
            if (obj[LAZYCAR]) {
              put("..");  // .. signifies a lazy car
            } else if (obj[COMPILED]) {
              toString(obj[CAR])
              sep = "";
              put(`{*compiled-${obj[COMPILED]}*}`);
              sep = " ";
            } else {
              toString(obj[CAR], maxCarDepth-1, maxCdrDepth);
            }
            sep = " ";
            if (obj[LAZYCDR])
              return put("...)", true);
            obj = obj[CDR];
            maxCdrDepth -= 1;
            if (maxCdrDepth < 0)
              return put("....)", true);
            if (obj != null && obj[SUPERLAZY])
              return put("...)", true);
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
        if (Array.isArray(obj)) {
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
            if (item instanceof EvaluateKeyValue) {
              prefix = "[";
              toString(item.key, maxCdrDepth, maxCdrDepth);
              prefix = "]: ";
              toString(item.value, maxCarDepth, maxCdrDepth);
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
        let name;
        if (obj[NAMETAG])
          name = obj[NAMETAG];
        else
          name = fnDesc.name ?? '';
        let params = fnDesc.printParams;
        let printBody = fnDesc.printBody;
        if (fnDesc.value && !fnDesc.body && !printBody)
          return put(`{${params} => ${fnDesc.value}}`);
        if (printBody && (printBody.length > 80 || printBody.includes('\n')))
          printBody = '';
        put(`{function ${name}${params}${printBody}`);
        sep = "";
        return put("}", true);
      }
      if (objType === 'symbol') {
        if (obj === LAMBDA_ATOM) return put(lambdaStr);
        if (obj === SLAMBDA_ATOM) return put(lambdaStr+"#");
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

  defineGlobalSymbol("list-view", list_view, { dontInline: true, group: "list-op" });
  function list_view(obj) {
    let iterator = iteratorFor(obj, TypeError);
    return new LazyIteratorList(iterator);
  }

  defineGlobalSymbol("lazy-map", lazy_map, { dontInline: true });
  function lazy_map(fn, obj) {
    let scope = this, iterator = iteratorFor(obj, TypeError);
    return new LazyIteratorList(iterator, a => fn(a))
  }

  defineGlobalSymbol("to-string", (obj, maxCarDepth = 100, maxCdrDepth = 10000) => string(obj, { maxCarDepth, maxCdrDepth }));

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
            yield { type: 'literal', value, position, line, lineChar };
            continue;
          }
        }
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

  let gensym_count = 0;
  const gensym = (name = `*gensym-${gensym_count++}*`) => Symbol(name);
  defineGlobalSymbol("gensym", gensym);

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
              if (tail) tail[CDR] = val;
              else head = val;
              return head;
            }
            if (token().type === 'end' || token().type === 'partial')
              throw new SchemeParseIncompleteError(path, token(), parseContext);
            if (token().type === 'garbage') throwSyntaxError();
            let item = parseExpr();
            item = cons(item, NIL);
            if (tail) tail = tail[CDR] = item;
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
                res[gensym] = new EvaluateKeyValue(sym, val);
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
    let str = fn.toString(), pos = 0, token = "", paramPos = 0, requiredCount;
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
    if (requiredCount === undefined)
      requiredCount = params.length;
    let res = { name, params, restParam, value, body, printBody, printParams, native, requiredCount };
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

  exportAPI("compile_lambda", compile_lambda)
  function compile_lambda(name, displayName, lambdaForm, jitFunction) {
    let scope = this;
    if (!isCons(lambdaForm) && typeof lambdaForm === 'symbol' ) throw new SchemeEvalError(`Bad lambda ${string(lambda)}`);
    let { code, bindSymToObj } = lambda_compiler.call(this, name, displayName, lambdaForm, jitFunction);
    if (TRACE_COMPILER || TRACE_COMPILER_CODE)
      console.log("COMPILED", name, code, bindSymToObj);
    let binder = new Function("bound", "resolveUnbound", "invokeUnbound", code);
    let compiledFunction = binder.call(this, bindSymToObj, resolveUnbound, invokeUnbound);
    return compiledFunction;
    function resolveUnbound(x) {
      let val = scope[x];
      if (val === undefined) checkUndefinedInScope(x, scope);
      return val;
    }
    function invokeUnbound(fn, args) {
      let parameterDescriptor = fn[PARAMETER_DESCRIPTOR] ?? examineFunctionForParameterDescriptor(fn);
      let requiredCount = parameterDescriptor & 0xffff;
      let evalCount = parameterDescriptor >> 15 >>> 1;  // restores MAX_INTEGER to MAX_INTEGER
      let argv = [], argCount = 0;
      for (; isCons(args); ++argCount, args = args[CDR]) {
        let arg = args[CAR];
        if (argCount < evalCount)
          arg = _eval(arg, scope);
        argv.push(arg);
      }
      return fn.apply(scope, argv);
    }
  }

  function lambda_compiler(nameAtom, displayName, lambdaForm, jitFunction) {
    // Prevent a tragic mistake that's easy to make by accident. (Ask me how I know!)
    if (nameAtom === QUOTE_ATOM) throw new SchemeEvalError("Can't redefine quote ${lambda}");
    let scope = this;
    let bindSymToObj = {}, guardedSymbols = {}, bindObjToSym = new Map(), functionDescriptors = {};
    let tempNames = {}, varNum = 0, emitted = [], usedSsaValues = {};
    let tools = { emit, bind, use, newTemp, scope, deleteEmitted, indent: '', evalLimit: 100, functionDescriptors };
    let ssaScope = new Scope();
    // Well-known names
    use(bind(string, "string"));
    use(bind(NIL, "NIL"));
    use(bind(schemeTrue, "schemeTrue"));
    use(bind(Cons, "Cons"));
    use(bind(car, "car"));
    use(bind(cdr, "cdr"));
    use(bind(Atom, "Atom"));
    use(bind(newScope, "newScope"));
    use(bind(CAR, "CAR"));
    use(bind(CDR, "CDR"));
    use(bind(PAIR, "PAIR"));
    use(bind(LIST, "LIST"));
    use(bind(COMPILED, "COMPILED"));
    // For template bodies
    use(bind(isList, "isList"));
    use(bind(isCons, "isCons"));
    use(bind(cons, "cons"));
    use(bind(CLOSURE_ATOM, "CLOSURE_ATOM"));
    let ssaFunction = compileLambda(nameAtom, displayName, lambdaForm, ssaScope, tools);
    if (jitFunction) {
      let ssaGuardFunction = newTemp(displayName + '_guard');
      emit(`function ${ssaGuardFunction}(...params) {`);
      emit(`  if (false`)
      for (let sym in guardedSymbols) {
        use(sym);
        let ssaAtom = use(bind(guardedSymbols[sym]));
        emit(`      || ${sym} !== scope[${ssaAtom}]`);
      }
      emit(`      ) {`);
      let ssaBoundJittedFn = bind(jitFunction);
      let ssaJITCOMPILD = use(bind(JITCOMPILED));
      emit(`    ${ssaBoundJittedFn}[${ssaJITCOMPILD}] = undefined;`);
      emit(`    ${ssaBoundJittedFn}(...params);`)
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
        if (bindSymToObj[name])  // collision
          name = newTemp(name);
      } else {
        if (typeof obj === 'symbol')
          name = newTemp(obj.description+'_atom');
        else if (typeof obj === 'function')
          name = newTemp(obj.name);
        else
          name = newTemp();
      }
      bindSymToObj[name] = obj;
      bindObjToSym.set(obj, name);
      if (guardSym)
        guardedSymbols[name] = guardSym;
      return name;
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
    // This keeps compileed templates from being bound unnecessarily at runtime.
    function use(ssaValue) {
      usedSsaValues[ssaValue] = true;
      return ssaValue;
    }
  }

  //
  // This function parallels "_eval" as closely as possible. If you make a change there
  // you almost certainly have to make a corresponding one here.
  //
  function compileEval(form, ssaScope, tools) {
    let emit = tools.emit, use = tools.use, bind = tools.bind, scope = tools.scope, newTemp = tools.newTemp;
    if (--tools.evalLimit < 0)
      throw new SchemeCompileError(`Too comlpex ${string(form)}`);
    if (form === undefined) return "undefined";
    if (form === null) return "null";
    if (typeof form === 'number' || typeof form === 'bigint' || typeof form === 'string')
      return string(form);
    if (form === true) return "true";
    if (form === false) return "false";
    if (isNil(form)) return "NIL";
    if (isPrimitive(form)) throw new LogicError(`All primitives should be handled by now`)
    if (typeof form === 'symbol') {
      let sym = form;
      let ssaValue = ssaScope[sym];
      if (ssaValue)
        return ssaValue;
      // for now, only bind functions from outside scope
      let scopedVal = scope[sym];
      if (scopedVal && typeof scopedVal === 'function') {
        if (!tools.functionDescriptors[ssaValue]) {
          let fn = scopedVal;
          let name = fn[NAMETAG] ?? fn.name ?? sym.description;
          ssaValue = bind(fn, newTemp(name), sym);
          let parameterDescriptor = fn[PARAMETER_DESCRIPTOR] ?? examineFunctionForParameterDescriptor(fn);
          let requiredCount = parameterDescriptor & 0xffff;
          let evalCount = parameterDescriptor >> 15 >>> 1;  // restores MAX_INTEGER to MAX_INTEGER
          let compileHook = fn[COMPILE_HOOK];
          let fnInfo = fn[COMPILE_INFO];
          let usesScope = true;  // conservative default
          let params, restParam, valueTemplate, bodyTemplate, native;
          if (fnInfo) {  // this is a built-in OR a jsClosure
            params = fnInfo.params;
            restParam = fnInfo.restParam;
            valueTemplate = fnInfo.value;
            bodyTemplate = fnInfo.body;
            native = fnInfo.native;
            if (valueTemplate) { 
              usesScope = false;  // templates generally don't use the scope, unless they use "this"
              if (valueTemplate.includes("this"))
                usesScope = true;
              else if (bodyTemplate && bodyTemplate.includes("this"))
                usesScope = true;
            }
            if (compileHook)  // If a hook uses the scope, it can set "used" in the scope itself
              usesScope = false;
          } else {  // Still need param info for closures, but don't use for templates
            fnInfo = analyzeJSFunction(fn);
            params = fnInfo.params;
            restParam = fnInfo.restParam;
            native = fnInfo.native;
          }
          if (native || isClosure(fn))
            usesScope = false;
          // Everything you need to know about invoking a JS function is right here
          tools.functionDescriptors[ssaValue] = {
            requiredCount, evalCount, name, compileHook, params, restParam,
            valueTemplate, bodyTemplate, usesScope
          };
        }
        return ssaValue;
      }
      ssaScope.used = true;
      return `resolveUnbound(${use(bind(sym))})`;
    }
    if (TRACE_COMPILER)  // too noisy and not very informative to trace the above
      console.log("COMPILE EVAL", string(form));
    if (isCons(form)) {
      let fn = form[CAR];
      if (fn === QUOTE_ATOM) {
        if (!isCons(form)) throwBadForm();
        return bind(form[CDR][CAR], 'quoted');
      }
      let ssaFunction;
      if (fn === LAMBDA_ATOM || fn === SLAMBDA_ATOM)
        return compileLambda(null, fn.description, form, ssaScope, tools);
      else
        ssaFunction = compileEval(fn, ssaScope, tools);
      let args = form[CDR];
      let functionDescriptor = tools.functionDescriptors[ssaFunction];
      if (!functionDescriptor) {
        use(ssaFunction);
        let fName = typeof fn === 'symbol' ? fn.description : 'unbound';
        let ssaResult = newTemp(fName+'_result');
        let ssaArgList = use(bind(args, `${fName}_args`));
        ssaScope.used = true;
        emit(`let ${ssaResult} = invokeUnbound(${ssaFunction}, ${ssaArgList});`);
        return ssaResult;
      }
      let requiredCount = functionDescriptor.requiredCount;
      let evalCount = functionDescriptor.evalCount;
      let fName = functionDescriptor.name;
      let params = functionDescriptor.params;
      let restParam = functionDescriptor.restParam;
      let compileHook = functionDescriptor.compileHook;
      let valueTemplate = functionDescriptor.valueTemplate;
      let bodyTemplate = functionDescriptor.bodyTemplate;
      let usesScope = functionDescriptor.usesScope;
      if (usesScope)
        ssaScope.used = true;
      if (typeof fName === 'symbol')
        fName = fName.description;
      if (!fName)
        fName = 'anon';

      // Run through the arg list evaluating args
      let ssaArgv = [], ssaArgStr = '', sep = '', argCount = 0;
      for ( ; isCons(args) ; ++argCount, args = args[CDR]) {
        let arg = args[CAR], ssaArg;
        if (argCount < evalCount)
          arg = ssaArg  = use(compileEval(arg, ssaScope, tools));
        else if (!compileHook)  // hooks get unbound unevaluated args
          arg = ssaArg = use(bind(arg, `arg_${argCount}`));
        ssaArgv.push(arg);
        if (ssaArg) {
          ssaArgStr += `${sep}${ssaArg}`;
          sep = ', ';
        }
      }
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
        if (usesScope)
          emit(`let ${ssaResult} = ${ssaFunction}.call(scope, ${ssaArgStr});`)
        else
          emit(`let ${ssaResult} = ${ssaFunction}(${ssaArgStr});`)
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
      if (fn[CAR] === CLOSURE_ATOM || fn[CAR] === SCLOSURE_ATOM) {
        closureBody = fn[CDR];
        if (fn[CAR] === SCLOSURE_ATOM) // Skip the evalCount param
          closureBody = closureBody[CDR];
        innerParams = closureBody[CAR];
        innerForms = closureBody[CDR];
      } else {
        if (restParam)
          innerParams = Atom(restParam);
        for (let i = params.length; i > 0; --i)
          innerParams = cons(Atom(params[i-1]), innerParams);
        innerForms = cons(cons(fn, innerParams), NIL);
      }
      // Peel off the number of parameters we have arguments for
      let capturedParams = NIL, last;
      for (let i = argCount; i > 0; --i, innerParams = innerParams[CDR]) {
        if (!isCons(innerParams))
          throw new LogicError(`There should be enough params`);
        let item = cons(innerParams[CAR], NIL);
        if (last) last = last[CDR] = item;
        else capturedParams = last = item;
      }
      if (!last) throw new LogicError(`There should be at least one param`);
      closureBody = cons(innerParams, innerForms);
      let closureForm = cons();
      if (evalCount !== MAX_INTEGER) {
        evalCount -= argCount;
        if (evalCount < 0)
          evalCount = 0;
        closureForm[CAR] = SCLOSURE_ATOM;
        closureForm[CDR] = cons("PATCH", cons(evalCount, closureBody));
      } else {
        closureForm[CAR] = CLOSURE_ATOM;
        closureForm[CDR] = cons("PATCH", closureBody);
      }
      let ssaClosureForm = bind(closureForm, "closureForm");
      // Now go through the arguments, matching to capturedParams, adding to both the ssaScope
      // and to the scope.
      ssaScope.used = true;
      ssaScope = newScope(ssaScope, "compiler-closure-scope");
      let closedArgStr = '';
      sep = '';
      for (let i = 0, tmp = capturedParams; i < ssaArgv.length; ++i, tmp = tmp[CDR]) {
        let arg = ssaArgv[i];
        if (isCons(tmp)) {
          let param = tmp[CAR];
          let ssaParam = newTemp(param);
          ssaScope[params[i]] = ssaParam;
          let ssaParamName = use(bind((param)));
          emit(`let ${ssaParam} = scope[${ssaParamName}] = ${arg};`)
          closedArgStr += `${sep}${ssaParam}`;
          sep = ', ';
        }
      }
      let paramStr = '';
      sep = '';
      for (; isCons(innerParams); innerParams = innerParams[CDR]) {
        let param = innerParams[CAR];
        let ssaParam = newTemp(param);
        paramStr += sep + ssaParam;
        sep = ', ';
      }
      if (typeof innerParams === 'symbol')
        paramStr += `${sep}...${newTemp(innerParams)}`;
      use(ssaFunction);
      // We always need a scope here
      emit(`${ssaResult} = (${paramStr}) => ${ssaFunction}.call(scope, ${closedArgStr}, ${paramStr});`);
      let displayName = `(${paramStr}) => ${ssaFunction}.call(scope${closedArgStr}, ${paramStr})`;
      decorateCompiledClosure(ssaResult, displayName, closureForm, requiredCount, evalCount, tools);
      tools.indent = saveIndent;
      emit(`}`);
      tools.functionDescriptors[ssaResult] = { requiredCount, evalCount, name, noScope: true };
      return ssaResult;
    }
    // Special eval for JS Arrays and Objects
    if (form !== null && typeof form === 'object') {
      if (Array.isArray(form )) {
        let ssaArrayLiteral = newTemp("arrayliteral");
        let evalledSsaValues = [];
        for (let element of form) {
          let ssaValue = compileEval(element, ssaScope, tools);
          use(ssaValue);
          evalledSsaValues.push(ssaValue);
        }
        emit(`let ${ssaArrayLiteral} = [`);
        for (let ssaElement of evalledSsaValues)
          emit(`  ${ssaElement},`);
        emit(`];`);
        return ssaArrayLiteral;
      }
      let ssaObjectLiteral = newTemp("objectliteral");
      emit(`let ${ssaObjectLiteral} = {};`);
      for (let key of [ ...Object.getOwnPropertyNames(form), ...Object.getOwnPropertySymbols(form) ]) {
        let value = form[key];
        let ssaKey;
        if (value instanceof EvaluateKeyValue) {
          ssaKey = compileEval(value.key, ssaScope, tools);
          value = value.value;
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

    function throwBadForm() {
      throw new SchemeCompileError(`BadForm ${string(form)}`);
    }
  }

  //
  // This function parallels makeJsClosure as closely as possible. If you make a change
  // there, you almost certainly have to make a corresponding change here.
  //
  function compileLambda(nameAtom, displayName, lambda, ssaScope, tools) {
    let emit = tools.emit, use = tools.use, bind = tools.bind, scope = tools.scope, newTemp = tools.newTemp;
    if (!isCons(lambda)) throwBadCompiledLambda(lambda);
    let body = lambda[CDR];
    let evalCount = MAX_INTEGER;
    if (lambda[CAR] === SLAMBDA_ATOM) {
      if (!isCons(lambda)) throwBadCompiledLambda(lambda);
      evalCount = body[CAR];
      if (typeof evalCount !== 'number') throwBadCompiledLambda(lambda);
      body = body[CDR];
    }
    if (!displayName) displayName = nameAtom ? nameAtom.description : evalCount === MAX_INTEGER ? `lambda` : `slambda#${evalCount}`;
    let ssaFunction = newTemp(displayName);
    if (!isCons(body)) throwBadCompiledLambda(lambda);
    let params = body[CAR];
    let forms = body[CDR];
    if (typeof params === 'symbol') { // Curry notation
      params = cons(params, NIL);
      forms = cons(forms, NIL);
    }
    if (!isList(params)) throwBadCompiledLambda(lambda);
    let ssaParamv = [], ssaRestParam, paramv = [], restParam;
    let originalSsaScope = ssaScope;
    ssaScope = newScope(ssaScope, "compiler-lambda-scope");
    let paramCount = 0, requiredCount, optionalFormsVec = [];
    for (; isCons(params); ++paramCount, params = params[CDR]) {
      let param = params[CAR], ssaParam;
      if (isCons(param)) {
        if (!param[CAR] === QUESTION_ATOM && isCons(param[CDR] && typeof param[CDR][CAR] === 'symbol'))
          throwBadCompiledLambda(lambda, `Bad param ${string(param)}`);
        optionalFormsVec.push(param[CDR][CDR]);
        param = param[CDR][CAR]
        ssaParam = newTemp(param);
        if (requiredCount === undefined)
          requiredCount = paramCount;
      } else {
        ssaParam = newTemp(param);
        optionalFormsVec.push(undefined);
      }
      ssaParamv.push(ssaParam);
      ssaScope[param] = ssaParam;
      paramv.push(param);
    }
    if (typeof params === 'symbol') {  // rest param (does not increment paramCount)
      restParam = params;
      ssaRestParam = newTemp(params);
      ssaScope[params] = ssaRestParam;
    }
    else if (!isNil(params))
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
    let ssaScopeTmp = newTemp("tmp_scope"), scopeLines = [];
    scopeLines.push(emit(`let ${ssaScopeTmp} = scope;`));
    emit(`function ${ssaFunction}(${ssaParamStr}) { // COMPILED ${displayName}`);
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
        let ssaVal = 'NIL';
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
      let ssaTmp = newTemp();
      emit(`let ${ssaTmp} = NIL;`);
      emit(`for (let i = ${ssaRestParam}.length; i > 0; --i)`);
      emit(`  ${ssaTmp} = cons(${ssaRestParam}[i-1], ${ssaTmp});`);
      emit(`${ssaRestParam} = ${ssaTmp};`);
      let ssaParamName = use(bind(restParam));
      scopeLines.push(emit(`scope[${ssaParamName}] =${ssaTmp};`));
    }
    let ssaResult = 'NIL';
    for ( ; isCons(forms); forms = forms[CDR])
      ssaResult = compileEval(forms[CAR], ssaScope, tools);
    if (ssaScope.used)
      originalSsaScope.used = true;
    else
      tools.deleteEmitted(scopeLines);
    use(ssaResult);
    emit(`return ${ssaResult};`);
    tools.indent = saveIndent;
    emit(`}`);
    let closureAtom = CLOSURE_ATOM;
    if (evalCount !== MAX_INTEGER) {
      closureAtom = SCLOSURE_ATOM;
      body = cons(evalCount, body);
    }
    let closureForm = cons(closureAtom, cons("PATCH", body));
    decorateCompiledClosure(ssaFunction, displayName, closureForm, requiredCount, evalCount, tools);
    return ssaFunction;
  }

  function throwBadCompiledLambda(lambda, msg) { throw new SchemeCompileError(`Bad lambda ${lambda}` + (msg ? `, ${msg}` : '')) }
  
  function decorateCompiledClosure(ssaClosure, displayName, closureForm, requiredCount, evalCount, tools) {
    let emit = tools.emit, use = tools.use, bind = tools.bind;
    let ssaClosureForm = use(bind(closureForm, "closureForm"));
    let ssaParameter_descriptor = use(bind(PARAMETER_DESCRIPTOR, "PARAMETER_DESCRIPTOR"))
    let parameterDescriptor = makeParameterDescriptor(requiredCount, evalCount);
    let evalCountStr = evalCount === MAX_INTEGER ? "MAX_INTEGER" : String(evalCount);
    emit(`// evalCount: ${evalCountStr}, requiredCount: ${requiredCount}`)
    emit(`${ssaClosure}[${ssaParameter_descriptor}] = ${parameterDescriptor};`);
    // The function is simultaneously a Scheme closure object
    let closureStr = string(closureForm);
    for (let str of closureStr.split('\n'))
      emit(`// ${str}`);
    emit(`${ssaClosure}[CAR] = ${ssaClosureForm}[CAR];`);
    emit(`${ssaClosure}[CDR] = new Cons(scope, ${ssaClosureForm}[CDR][CDR]);`);
    emit(`${ssaClosure}[COMPILED] = ${string(displayName)}`);
    // Mark object as a list, a pair, and a closure.
    emit(`${ssaClosure}[PAIR] = ${ssaClosure}[LIST] = ${ssaClosure}[CLOSURE_ATOM] = true;`);
  }

  function redecorateCompiledClosure(ssaToFn, ssaFromFn, emit) {
    emit(`${ssaToFn}[CAR] = ${ssaFromFn}[CAR];`);
    emit(`${ssaToFn}[CDR] = ${ssaFromFn}[CDR];`);
    emit(`${ssaToFn}[COMPILED] = ${ssaFromFn}[COMPILED];`);
    // Mark object as a list, a pair, and a closure.
    emit(`${ssaToFn}[PAIR] = ${ssaToFn}[LIST] = ${ssaToFn}[CLOSURE_ATOM] = true;`);
  }
  
  const JS_IDENT_REPLACEMENTS  = {
    '~': '$tilde', '!': '$bang', '@': '$at', '#': '$hash', '$': '$cash', '%': '$pct', '^': '$hat',
    '&': '$and', '|': '$or', '*': '$star', '+': '$plus', '-': '$dash', '=': '$eq', '<': '$lt',
    '>': '$gt', '/': '$stroke', '\\': '$bs', '?': '$q'
  };

  exportAPI("toJavaScriptIdentifier", toJavaScriptIdentifier);
  function toJavaScriptIdentifier(name) {
    if (typeof name === 'symbol')
      name = name.description;
    let newName = "", fragment = "";
    for (let ch of name) {
      if (JS_IDENT_REPLACEMENTS[ch]) {
        newName += fragment + JS_IDENT_REPLACEMENTS[ch];
        fragment = "";
      } else if (JSIDENT2[ch]) {
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