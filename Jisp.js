// Lisp in JavaScript
"use strict";

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

const cons = (car, cdr) => new Cons(car, cdr);
const car = (cons) => cons.car, first = car;
const cdr = (cons) => cons.cdr, rest = cdr;

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
Atom.QUOTE = defineGlobalSymbol((x) => x, true, false, "quote", "'");
defineGlobalSymbol(car, false, 1, "car");
defineGlobalSymbol(cdr, false, 1, "cdr");
defineGlobalSymbol(cons, false, 2, "cons");
defineGlobalSymbol((a,b) => a + b, false, 2, "add", "+"); // XXX should do arbirary many
defineGlobalSymbol((a,b) => a - b, false, 2, "sub", "-");
defineGlobalSymbol((a,b) => a * b, false, 2, "mul", "*");
defineGlobalSymbol((a,b) => a / b, false, 2, "div", "/");
defineGlobalSymbol((a,b) => a % b, false, 2, "mod", "%");
defineGlobalSymbol((a,b) => a | b, false, 2, "bit-or", "|");
defineGlobalSymbol((a,b) => a & b, false, 2, "bit-and", "&");
defineGlobalSymbol((a,b) => a && b, false, 2, "and", "&&");  // XXX this sb special form
defineGlobalSymbol((a,b) => a || b, false, 2, "or", "||");

class LispError extends Error {
  constructor(...args) {
    super(...args);
  }
  toString() { return this.message; }   // no type prefix on the error string
};
class EvalError extends LispError {};
class ResolveError extends LispError {};

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
    while (lift > 0) {
      if (args !== NIL && args instanceof Cons) {
        jsArgs.push(args.car);
        args = args.cdr;
      } else {
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
    // XXX use map
    for (let ch of obj) {
      switch (ch) {
        case '"':
          str += "\\" + ch;
          break;
        case "\r":
          str += "\\r";
          break;
        case "\n":
          str += "\\n";
          break;
        case "\t":
          srt += "\\t";
          break;
        default:
          str += ch;
          break;
      }
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
const IDENT1 = ALPHA + "_$+-*/%\\!&|=<>";
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
          switch (ch) {
            case "t":
              str += "\t";
              break;
            case "n":
              str += "\n";
              break;
            case "r":
              str += "\r";
              break;
            case "\n": case "\r":
              // eat newline
              break;
            default:
              str += "\\" + ch;
          }
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

    throw new Error(`Unexpected token ${token().type} ${token().value}`);
  }
  if (peekToken().type === 'end')
    return null;
  let expr = parseExpr(prompt);
  let peek = peekToken();
  let viable = peek.type === 'end' || peek.type === 'newline';
  if (viable)
    return expr;
  return new Error(`Unparsed: ${peek.type} ${peek.value}`); // XXX better message?
}

function lispREPL(readline, opts = {}) {
  // readline(prompt) => str || nullish
  let name = opts.name ?? "Jisp";
  let prompt = opts.prompt ?? name + " > ";
  let evaluate = opts.evaluate ?? (x => x);
  let print = opts.print ?? (x => console.log(name + ":", String(x)));
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

console.log("Test lispEval", lispEval(parseSExpr(`(car '(1 2))`)));

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