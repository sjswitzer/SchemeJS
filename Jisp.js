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
}

const NIL = new Cons(null, null);

function nextCons() {
  let current = this._current, done = current === NIL, value = undefined;
  if (!done) {
    this._current = current.cdr;
    value = current.car;
  }
  return { done, value };
}

Cons.prototype[Symbol.iterator] = function() { return { next: nextCons, _current: this } };

class LazyCons {
  _carFn; _cdrFn;
  constructor(carFn, cdrFn) {
    this._carFn = carFn;
    this._cdrFn = cdrFn;
    Object.freeze(this);
  }

  get car() { return this._carFn(); }
  get cdr() { return this._carFn(); }
  [Symbol.iterator] = function() { return { next: nextCons, _current: this } };

  toString() {
    return lispToString(this);
  }
}

class LazyCarCons {
  _carFn; cdr;
  constructor(carFn, cdr) {
    this._carFn = carFn;
    this.cdr = cdr;
    Object.freeze(this);
  }

  get car() { return this._carFn(); }
  [Symbol.iterator] = function() { return { next: nextCons, _current: this } };

  toString() {
    return lispToString(this);
  }
}

class LazyCdrCons {
  car; _cdrFn;
  constructor(car, cdrFn) {
    this.car = car;
    this._cdrFn = cdrFn;
    Object.freeze(this);
  }

  get cdr() { return this._carFn(); }
  [Symbol.iterator] = function() { return { next: nextCons, _current: this } };

  toString() {
    return lispToString(this);
  }
}

//
// Jisp strives to maintain JavaScript consistency wherever possibe but enough is enough.
// In Jisp, NIL, null, undefined, and false are false and everything else is true.
//
function jispToBool(obj) {
  if (obj === NIL || obj === undefined || obj === null || obj === false)
    return false;
  return true;
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
const specialSymbol = Symbol("*special*");
const liftSymbol = Symbol("*lift*");

function defineGlobalSymbol(val, special, lift, ...names) {
  if (special) val[specialSymbol] = special;
  if (lift > 0) val[liftSymbol] = lift;
  let atom = Atom._defineAtom(...names);
  GlobalEnv.set(atom, val);
  return atom;
}
Atom.NIL = defineGlobalSymbol(NIL, false, 0, "nil");
Atom.TRUE = defineGlobalSymbol(true, false, 0, "true");
Atom.FALSE = defineGlobalSymbol(false, false, 0, "false");
Atom.NULL = defineGlobalSymbol(null, false, 0, "null");
Atom.UNDEFINED = defineGlobalSymbol(false, false, 0, "undefined");
Atom.QUOTE = defineGlobalSymbol((x) => x, true, false, "quote", "'");
defineGlobalSymbol(cons, false, 2, "cons");
defineGlobalSymbol(car, false, 1, "car");
defineGlobalSymbol(cdr, false, 1, "cdr");
defineGlobalSymbol(cadr, false, 1, "cadr");
defineGlobalSymbol(cdar, false, 1, "cdar");
defineGlobalSymbol(cddr, false, 1, "cddr");

// Pokemon gotta catch 'em' all!
defineGlobalSymbol((a) => a === NIL || !a, false, 1, "not", "!");
defineGlobalSymbol((a) => ~a, false, 1, "bit-not", "~");
defineGlobalSymbol((a,b) => a ** b, false, 2, "exp", "**");
defineGlobalSymbol((a,b) => a % b, false, 2, "rem", "%");
defineGlobalSymbol((a,b) => a << b, false, 2, "bit-shl", "<<");
defineGlobalSymbol((a,b) => a >> b, false, 2, "bit-shr", ">>");
defineGlobalSymbol((a,b) => a >>> b, false, 2, "bit-ushr", ">>");
defineGlobalSymbol((a,b) => a in b, false, 2, "in");
defineGlobalSymbol((cls, ...args) => new cls(...args), false, Number.MAX_VALUE, "new");
defineGlobalSymbol((a,b) => a instanceof b, false, 2, "instanceof");
defineGlobalSymbol((a,b) => a != b, false, 2, "ne", "!=");
defineGlobalSymbol((a,b) => a !== b, false, 2, "neeq", "!==");

// variable args
defineGlobalSymbol((...args) => {
  let a = 0;
  for (let b of args)
    a += b;
  return a;
}, false, Number.MAX_VALUE, "add", "+");

defineGlobalSymbol((a, ...rest) => {
  if (rest.length === 0) return -a;  // XXX ???
  for (let b of rest)
    a -= b;
  return a;
}, false, Number.MAX_VALUE, "sub", "-");

defineGlobalSymbol((...args) => {
  let a = 1;
  for (let b of rest)
    a *= b;
  return a;
}, false, Number.MAX_VALUE, "mul", "*");

defineGlobalSymbol((a, ...rest) => {
  if (rest.length === 0) return 1/a;  // XXX ???
  for (let b of rest)
    a /= b;
  return a;
}, false, Number.MAX_VALUE, "div", "/");

defineGlobalSymbol((...args) => {
  let a = ~0;
  for (let b of args)
    a &= b;
  return a;
}, false, Number.MAX_VALUE, "bit-and", "&");

defineGlobalSymbol((...args) => {
  let a = 0;
  for (let b of args)
    a |= b;
  return a;
}, false, Number.MAX_VALUE, "bit-or", "|");

defineGlobalSymbol((...args) => {
  let a = 0;
  for (let b of args)
    a ^= b;
  return a;
}, false, Number.MAX_VALUE, "bit-xor", "");

defineGlobalSymbol((...args) => {
  let a = ~0;
  for (let b of args)
    a &= b;
  return a;
}, false, Number.MAX_VALUE, "bit-and", "&");

defineGlobalSymbol(function(a, ...rest) {
  if (rest.length === 0) return false; // not less than itself
  a = lispEval(a, this);
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a < b)) return false;
    a = b;
  }
  return true;
}, true, Number.MAX_VALUE, "lt", "<");

defineGlobalSymbol(function(a, ...rest) {
  if (rest.length === 0) return true; // equal to itself
  a = lispEval(a, this);
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a <= b)) return false;
    a = b;
  }
  return true;
}, true, Number.MAX_VALUE, "le", "<=");

defineGlobalSymbol(function(a, ...rest) {
  if (rest.length === 0) return false;
  a = lispEval(a, this);
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a > b)) return false;
    a = b;
  }
  return true;
}, true, Number.MAX_VALUE, "gt", ">");

defineGlobalSymbol(function(a, ...rest) {
  if (rest.length === 0) return true;
  a = lispEval(a, this);
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a >= b)) return false;
    a = b;
  }
  return true;
}, true, Number.MAX_VALUE, "ge", ">=");

defineGlobalSymbol(function(a, ...rest) {
  if (rest.length === 0) return true;
  a = lispEval(a, this);
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a == b)) return false;
  }
  return true;
}, true, Number.MAX_VALUE, "eq", "==");

defineGlobalSymbol(function(a, ...rest) {
  if (rest.length === 0) return true;
  a = lispEval(a, this);
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a === b)) return false;
  }
  return true;
}, true, Number.MAX_VALUE, "eeq", "===");

// logical &conditional

defineGlobalSymbol(function(...args) {
  let a = true;
  for (a of args) {
    a = lispEval(b, this);
    if (!jispToBool(a)) return a;
  }
  return a;
}, true, Number.MAX_VALUE, "and", "&&");

defineGlobalSymbol(function(...args) {
  let a = false;
  for (a of args) {
    a = lispEval(b, this);
    if (jispToBool(a)) return a;
  }
  return a;
}, true, Number.MAX_VALUE, "or", "||");

defineGlobalSymbol(function(predicate, trueBlock, falseBlock) {
  predicate = lispEval(predicate, this);
  let res;
  if (jispToBool(predicate))
    res = lispEval(trueBlock, this);
  else
    res = lispEval(falseBlock, this);
  return res;
}, true, 3, "if", "?");

// JavaScripty things:
//   XXX TODO: delete, setting

defineGlobalSymbol((a, b) => a[b], false, 2, "@");  // indexing and member access
defineGlobalSymbol((a, b) => a?.[b], false, 2, "?@");  // conditional indexing and member access
defineGlobalSymbol(e => { throw e }, false, 1, "throw");

defineGlobalSymbol(function (handler, body) {  // XXX order of args?
  let val = NIL;
  try { val = lispEval(body, this); }
  finally { lispEval(handler, this); }
  return val;
}, true, 2, "finally");

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
}, true, 3, "catch");

//
// This is where the magic happens
//

function lispEval(expr, scope = GlobalScope) {
  if (expr === NIL || typeof expr !== 'object') return expr;
  if (!(expr instanceof Cons)) return expr;  // Worry about evaluating lazy lists later. Or not!
  let op = expr.car, args = expr.cdr;
  if (op === Atom.QUOTE) {  // this is just an optimization; the quote function will do this too
    if (!(args instanceof Cons)) throw new EvalError(`Bad argument list ${args}`);
    return args.car;
  }
  if (typeof op === 'symbol') {
    op = resolveSymbol(op, scope);
    if (!op) throw new ResolveError(`Can't resolve ${op}`);
  }
  if (op instanceof Function) {
    if (!op[specialSymbol])
      args = evalArgs(args, scope);
    let lift = op[liftSymbol] ?? 0, jsArgs = [];
    while (lift > 0 && (args instanceof Cons) && args !== NIL) {
      jsArgs.push(args.car);
      args = args.cdr;
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
  throw new EvalError(`Cannot eval ${op}`);
}

function resolveSymbol(sym, scope) {
  while (scope !== NIL) {
    let val = scope.car.get(sym);
    if (val !== undefined)
      return val;
    scope = scope.cdr;
  }
}

function evalArgs(args, scope) {
  if (args === NIL)
    return args;
  if (!(args instanceof Cons))
    return args;
  let val = lispEval(args.car, scope);
  return cons(val, evalArgs(args.cdr));
}

const ESCAPE_STRINGS = { t: '\t', n: '\n', r: '\r', '"': '"', '\\': '\\' }
const STRING_ESCAPES = (() => {
  let res = {};
  for (let [key, value] of Object.entries(ESCAPE_STRINGS))
    res[value] = key;
  return res;
})();

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
// s-epression parser
//

const DIGITS = "0123456789";
const ALPHA =
  "abcdefghijklmnopqrstuvwxyz" +
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const IDENT1 = ALPHA + "~!@#$%^&|*_-+-=|\\<>?/";
const IDENT2 = IDENT1 + DIGITS;
const OPERATORS = "+-*/%\\!&|=<>";

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
    while (ch === ' ' || ch === '\t')
      nextc();

    if (ch === '\n' || ch === '\r') {
      yield { type: 'newline' };
      nextc();
      continue;
    }

    if ("()'".includes(ch)) {
      yield { type: ch };
      nextc();
      continue;
    }

    if (ch === "." && !DIGITS.includes(peekc())) {
      yield { type: ch };
      nextc();
      continue;
    }

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
    }

    if (ch === '"') {
      let str = "";
      nextc();
      while (ch && !'"\r\n'.includes(ch)) {
        if (ch === '\\' ) {
          nextc();
          let replace = ESCAPE_STRINGS[ch];
          if (replace)
            str += replace;
          else
            str += "\\" + ch;
          nextc();
          continue;
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

    if (IDENT1.includes(ch)) {
      let str = "";
      while (IDENT2.includes(ch))
        str += ch, nextc();
      yield { type: 'symbol', value: str };
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

    if (!dotNext && (token().type === 'symbol' || token().type === 'string' ||
        token().type === 'number')) {
      let thisToken = consumeToken();
      if (thisToken.type === 'symbol')
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

    if (token().type === "'") {
      let newPrompt = promptStr + quotePromptMore;
      consumeToken();
      replHints.prompt = newPrompt;
      let quoted = parseExpr(newPrompt);
      replHints.prompt = promptStr;
     return cons(Atom.QUOTE, cons(quoted, NIL));
    }

    throw new ParseError(`Unexpected token ${token().type} ${token().value}`);
  }
  if (peekToken().type === 'end')
    return null;
  let expr = parseExpr(prompt);
  let peek = peekToken();
  let viable = peek.type === 'end' || peek.type === 'newline';
  if (viable)
    return expr;
  return new ParseError(`Unparsed: ${peek.type} ${peek.value}`); // XXX better message?
}

function lispREPL(readline, opts = {}) {  // XXX should readline be a generator?
  // readline(prompt) => str || nullish
  let name = opts.name ?? "Jisp";
  let prompt = opts.prompt ?? name + " > ";
  let evaluate = opts.evaluate ?? (x => x);
  let print = opts.print ?? (x => console.log(name + ":", lispToString(x)));
  let reportError = opts.reportError ??  (x => console.log(name + " ERROR:", String(x), x));;
  let replHints = { prompt };
  function* charStreamPromptInput() {
    for(;;) {
      let line = readline(replHints.prompt);
      if (line == null) return; // == intended
      // strip any line endings; we'll add our own
      while (line.endsWith('\n') || line.endsWith('\r'))
        line = line.substr(0, line.length-1);
      // End REPL with any blank line
      if (line.length === 0) return;
      // Feed the charachers
      for (let ch of line)
        yield ch;
      // And then a newline
      yield '\n';
    }
  }
  let charStream = charStreamPromptInput();
  let tokenGenerator = lispTokenGenerator(charStream);
  let expr;
  try {
    expr = parseSExpr(tokenGenerator, { ...opts, replHints });
    if (!expr) return false;  // end REPL
  } catch (error) {
    expr = error;
  }
  if (expr instanceof Error) {
    reportError(expr);
    return true;  // contine REPL
  }
  let evaluated;
  try {
    evaluated = evaluate(expr);
  } catch (error) {
    evaluated = error;
  }
  if (expr instanceof Error) {
    reportError(expr);
    return true;  // continue REPL
  }
  print(evaluated);
  return true;  // continue REPL
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

let sExpr = parseSExpr(`(+ b (- 1 2))`);
console.log("Test parseSExpr", String(sExpr), sExpr);

sExpr = parseSExpr(`(a b 'c '(abc def))`);
console.log("parseSExpr", String(sExpr), sExpr);

{ // Run the REPL on some "input"
  let input = [ `(a b`, ` 'c '(abc`, ` def))` ];
  lispREPL(() => input.shift());
}

console.log("Test lispEval1", lispEval(parseSExpr(`(car '(1 2))`)));
console.log("Test lispEva12", lispEval(parseSExpr(`(+ 1 2 3 4)`)));
console.log("Test lispEva13", lispEval(parseSExpr(`(? (< 1 2) "a" "b")`)));

if (typeof window === 'undefined' && typeof process !== 'undefined') { // Running under node.js
  console.log("Running in node.js");
  let fs = require('fs');
  let inputFd, closeFd;
  try {
    try {
      // Good tips for this in "promot-sync" package
      if (process.platform === 'win32') {
        inputFd = process.stdin.fd;
      } else {
        inputFd = closeFd = fs.openSync('/dev/tty', 'rs')
      }
    } catch(e) {
      console.info("Can't open termnal", e);
    }
    if (inputFd !== undefined) {
      let ok = true;
      let buffer = Buffer.alloc(2000);
      do {
        let nl = "\n";
        function getLine(prompt) {
          process.stdout.write(prompt);
          let read = fs.readSync(inputFd, buffer);
          let line = buffer.slice(0, read).toString();
          while (line.endsWith('\n') || line.endsWith('\r'))
            line = line.substr(0, line.length-1);
          return line;
        }
        ok = lispREPL(getLine, { evaluate: lispEval });
      } while (ok);
    }
  } finally {
    if (closeFd !== undefined)
      fs.closeSync(closeFd);
  }
}

console.log("done");  // breakpoint spot