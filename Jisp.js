// Lisp in JavaScript
"use strict";

class LispError extends Error {
  constructor(...args) {
    super(...args);
  }
  toString() { return this.message; }   // no type prefix on the error string
};
class EvalError extends LispError {};
class ResolveError extends LispError {};
class ParseError extends LispError {};

// Atoms are Symbols
function Atom(name) {
  let val = Atom.ATOMS.get(name);
  if (val !== undefined) return val;
  return Atom._defineAtom(name);
}
Atom.ATOMS = new Map();
Atom._defineAtom = function(...names) {
  let atom = Symbol(names[0]);
  for (let name of names)
    Atom.ATOMS.set(name, atom);
  return atom;
}

Atom.LAMBDA = Atom._defineAtom("lambda", "\\", "\u03BB");
Atom.SLAMBDA = Atom._defineAtom("special-lambda", "\\\\");

// It's VERY tempting to use JS undefined for NIL but I want NIL (the empty list)
// to be iterable in JavaScript like any other list. The cheapest way
// to make NIL is to use a distinguished Cons object. That way the JIT
// will only ever see one shape in a list. The downside is that
// NIL is not JavaScript falsey.
//

class Cons {
  constructor(car, cdr) {
    this.car = car;
    this.cdr = cdr;
    Object.freeze(this);
  }
  toString() {
    return lispToString(this);
  }
  *[Symbol.iterator]() { return { next: nextCons, _current: this } }
}

// Nil must be a Cons because it is a list: the empty one. It should be JS-iterable.
const NIL = new (class Nil extends Cons {});  // Makes things look nicer in debugger

function nextCons() {
  let current = this._current, done = current === NIL, value = undefined;
  if (!done) {
    this._current = current.cdr;
    value = current.car;
  }
  return { done, value };
}
// XXX There should be a way to do this in the class decl, but how?
Cons.prototype.isCons = true;

class LazyCons {
  _carFn; _cdrFn;
  constructor(carFn, cdrFn) {
    this._carFn = carFn;
    this._cdrFn = cdrFn;
    Object.freeze(this);
  }
  get car() { return this._carFn(); }
  get cdr() { return this._carFn(); }
  toString() {
    return lispToString(this);
  }
  *[Symbol.iterator]() { return { next: nextCons, _current: this } }
}
LazyCons.prototype.isCons = true;

class LazyCarCons {
  _carFn; cdr;
  constructor(carFn, cdr) {
    this._carFn = carFn;
    this.cdr = cdr;
    Object.freeze(this);
  }
  get car() { return this._carFn(); }
  toString() {
    return lispToString(this);
  }
  *[Symbol.iterator]() { return { next: nextCons, _current: this } }
}
LazyCarCons.prototype.isCons = true;

class LazyCdrCons {
  car; _cdrFn;
  constructor(car, cdrFn) {
    this.car = car;
    this._cdrFn = cdrFn;
    Object.freeze(this);
  }
  get cdr() { return this._carFn(); }
  toString() {
    return lispToString(this);
  }
  *[Symbol.iterator]() { return { next: nextCons, _current: this } }
}
LazyCdrCons.prototype.isCons = true;

//
// Jisp strives to maintain JavaScript consistency wherever possibe but enough is enough.
// In Jisp, NIL, null, undefined, and false are false and everything else is true.
//
function lispToBool(val) {
  if (val === NIL || val === undefined || val === null || val === false)
    return false;
  return true;
}

//
// Coerce Lisp values to JS values
//
function lispToJS(val) {
  if (val === NIL) val = null;
  else if (val === Atom.TRUE) val = true;
  else if (val === Atom.FALSE) val = false;
  return val;
}

const cons = (car, cdr) => new Cons(car, cdr);
const car = (cons) => cons.car, first = car;
const cdr = (cons) => cons.cdr, rest = cdr;
const cadr = (cons) => cons.cdr.car;  // Beware the order here; in JS it's reversed
const cdar = (cons) => cons.car.cdr;
const cddr = (cons) => cons.cdr.cdr;

//
// An environment is simply a Map.
// A Scope is a list of environments.
//
const Env = Map;
const Scope = Cons;
const GlobalEnv = new Env;
const GlobalScope = new Scope(GlobalEnv, NIL);
const evalArgsSymbol = Symbol("*eval-args*");
const liftSymbol = Symbol("*lift*");
const BIGGEST_INT32 = 2**32-1;

function defineGlobalSymbol(val, opts = {}, ...names) {
  let evalArgs = opts.evalArgs ?? BIGGEST_INT32, lift = opts.lift ?? 0;
  if (!evalArgs) evalArgs = BIGGEST_INT32;
  if (lift === '*') lift = BIGGEST_INT32;
  if (val instanceof Function) {
    val[evalArgsSymbol] = evalArgs;
    val[liftSymbol] = lift;
  }
  let atom = Atom._defineAtom(...names);
  GlobalEnv.set(atom, val);
  return atom;
}
Atom.NIL = defineGlobalSymbol(NIL, {}, "nil");
Atom.TRUE = defineGlobalSymbol(true, {}, "true", "t");
Atom.FALSE = defineGlobalSymbol(false, {}, "false");
Atom.NULL = defineGlobalSymbol(null, {}, "null");
Atom.UNDEFINED = defineGlobalSymbol(false, {}, "undefined");
Atom.QUOTE = defineGlobalSymbol((x) => x, { evalArgs: 0 }, "quote", "'");
defineGlobalSymbol(cons, { lift: 2 }, "cons");
defineGlobalSymbol(car, { lift: 1 }, "car");
defineGlobalSymbol(cdr, { lift: 1 }, "cdr");
defineGlobalSymbol(cadr, { lift: 1 }, "cadr");
defineGlobalSymbol(cdar, { lift: 1 }, "cdar");
defineGlobalSymbol(cddr, { lift: 1 }, "cddr");
defineGlobalSymbol(a => !!a.isCons, { lift: 1 }, "consp");
defineGlobalSymbol(a => typeof a == 'number', { lift: 1 }, "numberp");
defineGlobalSymbol(a => a < 0 ? -a : a, { lift: 1 }, "abs");
defineGlobalSymbol(a => Math.sqrt(a), { lift: 1 }, "sqrt");
defineGlobalSymbol(a => Math.cbrt(a), { lift: 1 }, "cbrt");

// Pokemon gotta catch 'em' all!
defineGlobalSymbol((a) => !lispToBool(a), { lift: 1 }, "not", "!");
defineGlobalSymbol((a) => ~a, { lift: 1 }, "bit-not", "~");
defineGlobalSymbol((a,b) => a ** b, { lift: 2 }, "exp", "**");
defineGlobalSymbol((a,b) => a % b, { lift: 2 }, "rem", "%");
defineGlobalSymbol((a,b) => a << b, { lift: 2 }, "bit-shl", "<<");
defineGlobalSymbol((a,b) => a >> b, { lift: 2 }, "bit-shr", ">>");
defineGlobalSymbol((a,b) => a >>> b, { lift: 2 }, "bit-ushr", ">>");
defineGlobalSymbol((a,b) => a in b, { lift: 2 }, "in");
defineGlobalSymbol((cls, ...args) => new cls(...args), { lift: '*' }, "new");
defineGlobalSymbol((a,b) => a instanceof b, { lift: 2 }, "instanceof");
defineGlobalSymbol((a,b) => a != b, { lift: 2 }, "ne", "!=");
defineGlobalSymbol((a,b) => a !== b, { lift: 2 }, "neeq", "!==");

// variable args
defineGlobalSymbol((...args) => {
  let a = 0;
  for (let b of args)
    a += b;
  return a;
}, { lift: '*' }, "add", "+");

defineGlobalSymbol((a, ...rest) => {
  if (rest.length === 0) return -a;  // XXX ???
  for (let b of rest)
    a -= b;
  return a;
}, { lift: '*' }, "sub", "-");

defineGlobalSymbol((...args) => {
  let a = 1;
  for (let b of args)
    a *= b;
  return a;
}, { lift: '*' }, "mul", "*");

defineGlobalSymbol((a, ...rest) => {
  if (rest.length === 0) return 1/a;  // XXX ???
  for (let b of rest)
    a /= b;
  return a;
}, { lift: '*' }, "div", "/");

defineGlobalSymbol((...args) => {
  let a = ~0;
  for (let b of args)
    a &= b;
  return a;
}, { lift: '*' }, "bit-and", "&");

defineGlobalSymbol((...args) => {
  let a = 0;
  for (let b of args)
    a |= b;
  return a;
}, { lift: '*' }, "bit-or", "|");

defineGlobalSymbol((...args) => {
  let a = 0;
  for (let b of args)
    a ^= b;
  return a;
}, { lift: '*' }, "bit-xor", "");

defineGlobalSymbol((...args) => {
  let a = ~0;
  for (let b of args)
    a &= b;
  return a;
}, { lift: '*' }, "bit-and", "&");

defineGlobalSymbol(function(a, ...rest) {
  if (rest.length === 0) return false; // not less than itself
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a < b)) return false;
    a = b;
  }
  return true;
}, { evalArgs: 1, lift: '*' }, "lt", "<");

defineGlobalSymbol(function(a, ...rest) {
  if (rest.length === 0) return true; // equal to itself
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a <= b)) return false;
    a = b;
  }
  return true;
}, { evalArgs: 1, lift: '*' }, "le", "<=");

defineGlobalSymbol(function(a, ...rest) {
  if (rest.length === 0) return false;
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a > b)) return false;
    a = b;
  }
  return true;
}, { evalArgs: 1, lift: '*' }, "gt", ">");

defineGlobalSymbol(function(a, ...rest) {
  if (rest.length === 0) return true;
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a >= b)) return false;
    a = b;
  }
  return true;
}, { evalArgs: 1, lift: '*' }, "ge", ">=");

defineGlobalSymbol(function(a, ...rest) {
  if (rest.length === 0) return true;
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a == b)) return false;
  }
  return true;
}, { evalArgs: 1, lift: '*' }, "eq", "==");

defineGlobalSymbol(function(a, ...rest) {
  if (rest.length === 0) return true;
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a === b)) return false;
  }
  return true;
}, { evalArgs: 1, lift: '*' }, "eeq", "===");

// logical &conditional

defineGlobalSymbol(function(...args) {
  let a = true;
  for (a of args) {
    a = lispEval(b, this);
    if (!lispToBool(a)) return a;
  }
  return a;
}, { evalArgs: 0, lift: '*' }, "and", "&&");

defineGlobalSymbol(function(...args) {
  let a = false;
  for (a of args) {
    a = lispEval(b, this);
    if (lispToBool(a)) return a;
  }
  return a;
}, { evalArgs: 0, lift: '*' }, "or", "||");

defineGlobalSymbol(function(predicate, trueBlock, falseBlock) {
  let res;
  if (lispToBool(predicate))
    res = lispEval(trueBlock, this);
  else
    res = lispEval(falseBlock, this);
  return res;
}, { evalArgs: 1, lift: 3 }, "if", "?");

// JavaScripty things:
//   XXX TODO: delete, setting

defineGlobalSymbol((a, b) => a[b], { lift: 2 }, "@");  // indexing and member access
defineGlobalSymbol((a, b) => a?.[b], { lift: 2 }, "?@");  // conditional indexing and member access

//
// try/catch/filnally. Just a sketch for now.
//

defineGlobalSymbol(e => { throw e }, { lift: 1 }, "throw");

defineGlobalSymbol(function (handler, body) {  // XXX order of args?
  let val = NIL;
  try { val = lispEval(body, this); }
  finally { lispEval(handler, this); }
  return val;
}, { evalArgs: 0, lift: 2 }, "finally");

defineGlobalSymbol(function(e, handler, body) {  // XXX order of args?
  // Tempting to save some of this work for the handler, but better to report errors early
  let cls, sym;
  if (typeof e === 'symbol') {
    sym = e;
  } else if (e instanceof Cons) {
    cls = e.car;
    if (typeof cls !== 'function')
      cls = lispEval(cls, this);
    if (typeof cls !== 'function')
      throw new EvalError(`Not a class ${cls}`);
    if (e.cdr instanceof Cons)
      sym = e.cdr.car;
  }
  if (typeof sym !== 'symbol')
    throw new EvalError(`Bad catch binding syntax ${e}`);
  // Whew!
  let val = NIL;
  try { val = lispEval(body, this) }
  catch (e) {
    if (cls && !(e instanceof cls)) throw e;
    let env = new Env;
    env.set(sym, e);
    val = lispEval(handler, new Scope(env, this));
  }
  return val;
}, { evalArgs: 0, lift: 3 }, "catch");

//
// This is where the magic happens
//

function lispEval(expr, scope = GlobalScope) {
  if (expr === NIL) return expr;
  if (typeof expr === 'symbol') {
    let val = resolveSymbol(expr, scope);
    if (val === undefined) throw new ResolveError(`Can't resolve ${expr.description}`);
    return val;
  }
  if (typeof expr !== 'object') return expr;
  if (!(expr instanceof Cons)) return expr;  // Worry about evaluating lazy lists later. Or not!
  let op = expr.car, args = expr.cdr;
  if (op === Atom.QUOTE) {  // this is just an optimization; the quote function will do this too
    if (!(args instanceof Cons)) throw new EvalError(`Bad argument list ${args}`);
    return args.car;
  }
  if (typeof op === 'symbol') { // XXX a deeper eval needed? A different order?
    let resolved = resolveSymbol(op, scope);
    if (!resolved) throw new ResolveError(`Can't resolve symbol "${op.description}"`);
    op = resolved;
  }
  if (op instanceof Function) {
    let evalCount = op[evalArgsSymbol] ?? BIGGEST_INT32;
    args = evalArgs(args, scope, evalCount);
    let lift = op[liftSymbol] ?? 0, jsArgs = [], noPad = lift === BIGGEST_INT32;
    while (lift > 0) {
      if (args !== NIL && (args instanceof Cons)) {
        jsArgs.push(args.car);
        args = args.cdr;
      } else {
        // don't let cons, etc, be seeing any undefined parmaters
        if (noPad) // but not infinitely many of them!
          break;
        jsArgs.push(NIL);
      }
      --lift;
    }
    if (args !== NIL)  // "rest" arg; however NIL shows up as "undefined" in this one case
      jsArgs.push(args);
    return op.apply(scope, jsArgs);  // "this" is the scope!
  }
  // (\ args body)
  if (op instanceof Cons)  {
    if (op.car === Atom.LAMBDA || op.car === Atom.SLAMBDA) {
      if (op.car !== Atom.SLAMBDA)
        args = evalArgs(args, scope);
      let formalParams = op.cdr;
      if (!formalParams) throw new EvalError(`Lambda has no parameters`);
      let body = formalParams.cdr;
      if (typeof formalParams === 'symbol') // allow lambda x . body notation. Why not?
        formalParams = cons(formalParams, NIL);
      let newEnv = new Env;
      let newScope = new Scope(newEnv, scope);
      while (formalParams instanceof Cons) {
        let param = formalParams.car;
        if (typeof param !== 'symbol') throw new EvalError(`Param must be a symbol ${param}`);
        if (args !== NIL) {
          newEnv[param] = args.car;
          args = arg.cdr
        } else {
          newEnv[param] = NIL;
        }
      }
      if (typeof formalParams === 'symbol')  // Neat trick for 'rest' params!
        newEnv[formalParams] = args;
      return lispEval(body, newScope);
    }
  }
  throw new EvalError(`Cannot eval ${expr}`);
}

function resolveSymbol(sym, scope) {
  while (scope !== NIL) {
    let val = scope.car.get(sym);
    if (val !== undefined)
      return val;
    scope = scope.cdr;
  }
}

function evalArgs(args, scope, evalCount) {
  if (args === NIL || evalCount <= 0)
    return args;
  if (!(args instanceof Cons))
    return args;
  let val = lispEval(args.car, scope);
  return cons(val, evalArgs(args.cdr, scope, evalCount-1));
}

const ESCAPE_STRINGS = { t: '\t', n: '\n', r: '\r', '"': '"', '\\': '\\' }
const STRING_ESCAPES = (() => {
  let res = {};
  for (let [key, value] of Object.entries(ESCAPE_STRINGS))
    res[value] = key;
  return res;
})();

// XXX Line wrapping
function lispToString(obj, maxDepth = 1000, opts, moreList, quoted) {
  if (maxDepth <= 0) return "...";
  let objType = typeof obj;
  if (obj === NIL) return "()";
  if (objType === 'object') {
    if (obj instanceof Cons) {
      let before = "(", after = ")";
      if (quoted)
        before = after = "";
      else if (moreList)
        before = " ", after = "";
      if (opts?.quoteNotation && obj.car === Atom.QUOTE) {
        before = moreList ? " " : "";
        return before + "'" + lispToString(obj.cdr, maxDepth-1, opts, true, true);
      }
      if (obj.cdr === NIL)
        return before + lispToString(obj.car, maxDepth-1, opts) + after;
      if (obj.cdr instanceof Cons)
        return before +
            lispToString(obj.car, maxDepth-1, opts) +
            lispToString(obj.cdr, maxDepth-1, opts, true) +
            after;
      return before + lispToString(obj.car, maxDepth-1, opts) + " . " + 
          lispToString(obj.cdr, maxDepth-1, opts) + after;
    }
    if (obj instanceof Array) {
      let str = "[", sep = "";
      for (let item of obj) {
        str += sep + lispToString(item, maxDepth-1, opts);
        sep = ", ";
      }
      return str + "]";
    }
    {
      let str = "{", sep = "";
      // Plain object
      for (let name of Object.getOwnPropertyNames(obj)) {
        let item = obj[name];
        str += sep + name + ": " + lispToString(item, maxDepth-1, opts);
        sep = ", ";
      }
      return str + "}";
    }
  }
  if (objType === 'symbol') {
    return obj.description;
  }
  if (objType === 'string') {
    let str = '"';
    for (let ch of obj) {
      let replace = STRING_ESCAPES[ch];
      if (replace)
        str += "\\" + replace;
      else
        str += ch;
    }
    str += '"';
    return str;
  }
  // XXX there must be more to do here
  return String(obj);
}

const toLispSymbol = Symbol("toLisp");

//
// Turn iterables objects into lists
//
function toList(obj, opts) { 
  if (typeof obj === 'object') {
    let iterator = obj;
    if (typeof obj[toLispSymbol] === 'function')
      return obj[toLispSymbol](opts);
    if (typeof obj[Symbol.iterator] === 'function')
      iterator = obj[Symbol.iterator]();
    if (typeof iterator.next == 'function') {
      let next = iterator.next();
      if (typeof (next?.done) === 'boolean') {
        if (next.done)
          return NIL;
        let value = toList(next.value, opts);
        return cons(value, toList(iterator));
      }
    }
    // guess it wasn't an iterator after all. Oh well!
  }
  return obj;
}

//
// S-epression parser
//

const TOKS = {}, DIGITS = {}, ALPHA = {}, IDENT1 = {}, IDENT2 = {},
    NUM1 = {}, NUM2 = {}, OPERATORS = {}, WS = {}, NL = {};
for (let ch of `()[]{},':`) TOKS[ch] = true;
for (let ch of ` \t`) WS[ch] = true;
for (let ch of `\n\r`) NL[ch] = true;
for (let ch of `abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ`)
  ALPHA[ch] = IDENT1[ch] = IDENT2[ch] = true;
for (let ch of `0123456789`)
  DIGITS[ch] = IDENT2[ch] = NUM1[ch] = NUM2[ch] = true;
for (let ch of `+-.`)
  NUM1[ch] = NUM2[ch] = true;
for (let ch of `eEoOxXbBn`)
  NUM2[ch] = true;
for (let ch of `~!@#$%^&|*_-+-=|\\<>?/`)
  OPERATORS[ch] = IDENT1[ch] = IDENT2[ch] = true;

function* lispTokenGenerator(characterGenerator) {
  if (!(typeof characterGenerator.next === 'function')) {
    if (typeof characterGenerator[Symbol.iterator] === 'function') {
      let origParam = characterGenerator;
      characterGenerator = characterGenerator[Symbol.iterator]();
      if (!(typeof characterGenerator.next === 'function'))
        throw new Error(`Not an iterator or iterable ${origParam}`);
    }
  }
  let ch = "", _peek = [], _done = false;
  function nextc() {
    if (_peek.length > 0)
      return ch = _peek.shift();
    if (_done) return "";
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

    // JS numbers are weird. The strategy here is to try them all and let JS sort it out.
    // XXX Things like "+ " go through unnecessary hoops here.
    if (NUM1[ch]) {
      let pos = 0, str = ch;
      for (;;) {
        let ch = peekc(pos++);
        if (!NUM2[ch])
          break;
        str += ch;
      }
      let value = Number(str);
      if (isNaN(value)) {
        value = undefined;
        if (str.endsWith('n')) {
          str = str.substr(0, str.length-1);
          try {  // maybe it's a BigInt?
            value = BigInt(str);
          } catch (e) {
            // eat it
          }
        }
      }
      if (value !== undefined) {
        // consume all the characters we peeked and succeed
        while (pos-- > 0) nextc();
        yield { type: 'number', value};
        continue;
      }
    }

    /*
    // XXX deal with +/-
    if (ch === '.' || DIGITS.includes(ch)) {
      let dot = ch === '.';
      let str = ch;
      nextc();
      while(DIGITS.includes(ch) || (!dot && ch === ".")) {
        str += ch;
        if (ch === ".") dot = true;
        nextc();
      }
      if (ch === 'e') {
        let pc = peekc();
        if (DIGITS.includes(pc) || ((pc === "+" || pc === "-") && DIGITS.includes(peekc(1)))) {
          str += ch, nextc();
          if (ch === "+" || ch === "-")
            str += ch, nextc();
          while (DIGITS.includes(ch))
            str += ch, nextc();
        }
      }
      yield { type: 'number', value: Number(str) };
      continue;
    } */

    // XXX move later; it's less common
    if (ch === '"') {
      let str = "";
      nextc();
      while (ch && ch != '"' && !NL[ch]) {
        if (ch === '\\') {
          nextc();
          ch = ESCAPE_STRINGS[ch] ?? `\\$ch}`;
        }
        str += ch;
        nextc();
      }
      if (ch === '"')
        yield { type: 'string', value: str };
      else
        yield { type: 'garbage', value: '"'+str };
      nextc();
      continue;
    }

    if (IDENT1[ch]) {
      let str = "", operatorPrefix = OPERATORS[ch];
      while (ch && IDENT2[ch]) {
        if (operatorPrefix && DIGITS[ch])  // a prefix of operators breaks at a digit
          break;
        if (!OPERATORS[ch]) operatorPrefix = false;
        str += ch, nextc();
      }
      yield { type: 'atom', value: str };
      continue;
    }

    yield { type: 'garbage', value: ch };
    nextc()
  }
}

function parseSExpr(tokenGenerator, opts = {}) {
  let replHints = opts.replHints ?? {};
  let prompt = opts.prompt ?? "Jisp > ";
  let promptMore = opts.promptMore = "  ";
  let quotePromptMore = opts.quotePromptMore ?? promptMore;
  if (typeof tokenGenerator === 'string')
    tokenGenerator = lispTokenGenerator(tokenGenerator);
  if (!(typeof tokenGenerator.next === 'function')) {
    if (typeof tokenGenerator[Symbol.iterator] === 'function') {
      let origParam = tokenGenerator;
      tokenGenerator = tokenGenerator[Symbol.iterator]();
      if (!(typeof tokenGenerator.next === 'function'))
        throw new Error(`Not an iterator or iterable ${origParam}`);
    }
  }

  let _token = null, _peek = [], _done = false;
  function token() {
    // Prevent fetching a new line prematurely and detect viable end points
    if (_token) return _token;
    do {
      _token = _nextToken();
      // will never return a newline token, but a newline could be in the peek queue
    } while (_token.type === 'newline')
    return _token;
  }

  function consumeToken() {
    let currToken = token();
    _token = null;
    return currToken;
  }

  function _nextToken() {
    if (_peek.length > 0)
        return _peek.shift();
    else if (_done)
      return { type: 'end'};
    let next = tokenGenerator.next();
    if (next.done) {
      _done = true;
      return { type: 'end'};
    }
    return next.value;
  }

  function peekToken(n = 0) {
    for (let get = n - _peek.length + 1; get > 0; --get) {
      let next = tokenGenerator.next();
      if (next.done) {
        _done = true;
        return { type: 'end'};
      }
      _peek.push(next.value);
    }
    return _peek[n];
  }

  function parseExpr(promptStr) {
    // This will not peek across a linebreak because the newline token will foil it
    let dotNext = peekToken().type === '.';

    if (!dotNext && (token().type === 'atom' || token().type === 'string' ||
        token().type === 'number')) {
      let thisToken = consumeToken();
      if (thisToken.type === 'atom')
        return Atom(thisToken.value);
      return thisToken.value;
    }

    if (dotNext) {
      let firstToken = consumeToken();
      if (consumeToken().type !== '.') throw new Error("Logic error; should be a dot");
      return cons(firstToken.value, parseExpr());
    }

    if (token().type === '(') {
      let newPrompt = promptStr + promptMore;
      replHints.prompt = newPrompt;
      consumeToken();
      function parseListBody() {
        if (token().type === ')') {
          replHints.prompt = promptStr;
          consumeToken();
          return NIL;
        }
        let first = parseExpr(newPrompt);
        let rest = parseListBody();
        return cons(first, rest);
      }
      return parseListBody();
    }

    if (token().type === '[') {  // JavaScript Array, oddly enough!
      let res = [];
      let newPrompt = promptStr + promptMore;
      replHints.prompt = newPrompt;
      consumeToken();
      for (;;) {
        let item = parseExpr(newPrompt);
        item = lispToJS(item);
        res.push(item);
        if (token().type === ',')  // Comma might as well be optional for now
          consumeToken()
        if (token().type === ']') {
          consumeToken();
          return res;
        }
      }
    }

    if (token().type === '{') {  // JavaScript Object literal too!
      let res = {};
      let newPrompt = promptStr + promptMore;
      replHints.prompt = newPrompt;
      consumeToken();
      for (;;) {
        if (token().type === '}') {
          consumeToken();
          break;
        }
        let gotIt = false;
        if (token().type === 'atom' || token().type === 'string' || token().type === 'number') {
          let sym = token().value;
          consumeToken();
          if (token().type === ':') {
            consumeToken();
            let val = parseExpr(newPrompt);
            val = lispToJS(val);
            res[sym] = val;
            gotIt = true;
            if (token().type === ',')  // might as well be optional for now
              consumeToken();
          }
        }
        if (!gotIt)
          throw new ParseError(`Bad JavaScript object literal`); // XXX details
      }
      return res;
    }

    if (token().type === "'") {
      let newPrompt = promptStr + quotePromptMore;
      consumeToken();
      replHints.prompt = newPrompt;
      let quoted = parseExpr(newPrompt);
      replHints.prompt = promptStr;
      return cons(Atom.QUOTE, cons(quoted, NIL));
    }

    if (peekToken().type === 'end')
      return null;
    throw new ParseError(`Unexpected token ${token().type} ${token().value}`);
  }

  // I'm old enough to be fond of the EvalQuote REPL.
  // So, as a special case, transform "symbol(a b c)" into "(symbol (quote a) (quote b) (quote c))"
  let expr;
  if (peekToken(0).type === 'atom' && peekToken(1).type === '(') {
    function quoteArgs(args) {
      if (args === NIL || !(args instanceof Cons)) return NIL;
      let quoted = args.car;
      quoted = cons(Atom.QUOTE, cons(quoted, NIL))
      return cons(quoted, quoteArgs(args.cdr));
    }
    let tok = consumeToken();
    let sym = Atom(tok.value);
    let quoted = parseExpr(prompt);
    if (quoted)
      expr = cons(sym, quoteArgs(quoted));
  } else {
    // Modern form
    expr = parseExpr(prompt);
  }
  let peek = peekToken();
  let viable = peek.type === 'end' || peek.type === 'newline';
  if (viable)
    return expr;
  throw new ParseError(`Unparsed: ${peek.type} ${peek.value}`);
}

function lispREPL(readline, opts = {}) {
  // readline(prompt) => str | nullish
  let name = opts.name ?? "Jisp";
  let prompt = opts.prompt ?? name + " > ";
  let print = opts.print ?? (x => console.log(name + ":", lispToString(x)));
  let reportLispError = opts.reportLispError ??  (x => console.log(name + " ERROR:", String(x)));;
  let reportError = opts.reportError ??  (x => console.log(name + " SYSTEM ERROR:", String(x), x));;
  let replHints = { prompt };
  let endTest = opts.endTest ?? (line => line === ".");  // end on a "."
  let done = false;
  function* charStreamPromptInput() {
    for(;;) {
      let line = readline(replHints.prompt);
      if (line === null || line === undefined || endTest(line)) {
        done = true;
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
  let tokenGenerator = lispTokenGenerator(charStream);
  while (!done) {
    try {
      let expr = parseSExpr(tokenGenerator, { ...opts, replHints });
      if (!expr) continue;
      let evaluated = lispEval(expr);
      print (evaluated);
    } catch (error) {
      if (error instanceof LispError)
        reportLispError(error);
      else
        reportError(error);
    }
  }
}

let x = toList(['a', 'b', [ 1, 2, 3 ], 'c']);
console.log("Test toList", x);
console.log("Test toString", String(x));
console.log("Test NIL toString", String(NIL));
console.log("Test iterable", [...x]);
console.log("Test NIL iterable", [...NIL]);
console.log("Test cons", String(cons('x', cons('y', NIL))));
console.log("Test cons", String(cons('x', 'y')));
console.log("Test cons", String(cons('x', cons('y', 'z'))));
let str = `(+ b (- 1 2)) ${'\n'} 12 ( .) . 1.23 ' .23 .23.4 .23e+234 a bc b21 "asd"`;
let tokens = lispTokenGenerator(str);
let tokenList = [ ...tokens ];
console.log("Test lispTokenGenerator", tokenList);

let sExpr = parseSExpr(`cons(a b)`);
console.log("Test parseSExpr", String(sExpr), sExpr);
sExpr = parseSExpr(`(+ b (- 1 2))`);
console.log("Test parseSExpr", String(sExpr), sExpr);

sExpr = parseSExpr(`(a b 'c '(abc def))`);
console.log("parseSExpr", String(sExpr), sExpr);

{ // Run the REPL on some "input"
  let input = [ `(a b`, ` 'c '(abc`, ` def))` ];
  lispREPL(() => input.shift());
}

console.log("Test lispEval", lispEval(parseSExpr(`(car '(1 2))`)));
console.log("Test lispEva1", lispEval(parseSExpr(`(+ 1 2 3 4)`)));
console.log("Test lispEva1", lispEval(parseSExpr(`(? (< 1 2) "a" "b")`)));
console.log("Test lispEva1", lispEval(parseSExpr(`(* 2 3)`)));
console.log("Test lispEva1", lispEval(parseSExpr(`+(2 3)`)));
let xx1 = parseSExpr(`{ a: 1, b: "foo" }`);
let xx2 = lispEval(xx1);
console.log("Test parse JS objects", lispToString(xx2));
// console.log("Test lispEval", lispEval(parseSExpr(`foo`)));

//console.log("Test lispEva1", lispEval(parseSExpr(`duck`))); // XXX need beter tesing

if (typeof window === 'undefined' && typeof process !== 'undefined') { // Running under node.js
  let fs = require('fs');
  let inputFd, closeFd, oldRawMode;
  try {
    try {
      if (process.platform === 'win32') {
        inputFd = process.stdin.fd;
        if (process.stdin.setRawMode) {
          oldRawMode = process.stdin.isRaw;
          process.stdin.setRawMode(true);
        }
      } else {
        inputFd = closeFd = fs.openSync('/dev/tty', 'rs')
      }
    } catch(e) {
      console.info("Can't open termnal", e);
    }
    if (inputFd !== undefined) {
      console.log(`Jisp REPL. Type "." to exit.`);
      let buffer = Buffer.alloc(2000);
      function getLine(prompt) {
        process.stdout.write(prompt);
        let read = fs.readSync(inputFd, buffer);
        let line = buffer.slice(0, read).toString();
        while (line.endsWith('\n') || line.endsWith('\r'))
          line = line.substr(0, line.length-1);
        return line;
      }
      lispREPL(getLine);
    }
  } finally {
    if (closeFd !== undefined)
      fs.closeSync(closeFd);
    if (oldRawMode !== undefined)
      process.stdin.setRawMode(oldRawMode);
  }
}

console.log("done");  // breakpoint spot