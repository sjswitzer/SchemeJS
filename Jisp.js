// Lisp in JavaScript
"use strict";

// Atoms are Symbols
function Atom(name) { return Atom.ATOMS.get(name) ?? Atom._defineAtom(name) }
Atom.ATOMS = new Map();
Atom._defineAtom = function(...names) {
  let atom = Symbol(names[0]);
  for (let name of names)
    Atom.ATOMS.set(name, atom);
  return atom;
}
Atom.QUOTE = Atom._defineAtom("quote", "'");
Atom.LAMBDA = Atom._defineAtom("lambda", "\\", "\u03BB");

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

const NIL = new Cons("*nil*", null);

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

/*
class Scope {
  // Optomize this!
  parent;
  _defs = new Map();
  constructor(parent) {
    this.parent = parent;
  }
  resolve(atom) {
  }
}

function evalExpr(expr, scope, opts) {
  if (expr instanceof Cons) {
    // This is where the magic happens
    let car = expr.car;
  }
  return expr;
}
*/

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

const toLispSym = Symbol("toLisp");

function toLisp(obj, opts) {
  if (obj == null) // deliberately using == to check null and undefined
    return NIL;
  let objType = typeof obj;  // I respect the JIT; this is for me :)
  if (objType === 'string') {
    if (!(opts?.string === 'expand'))
      return obj;
    // Don't keep expanding and expanding the string's characters.
    // A one-character string is still a string.
    opts = { ...opts, string: undefined };
  }
  let iterator;
  if (objType === 'object') {
    if (typeof obj[toLispSym] === 'function')
      return obj[toLispSym]().call(obj);
    if (typeof obj[Symbol.iterator] === 'function')
      iterator = obj[Symbol.iterator].call(obj);
    else
      iterator = obj;
  }
  if (iterator && typeof iterator.next == 'function') {
    let next = iterator.next();
    if (typeof (next?.done) === 'boolean') {
      if (next.done)
        return NIL;
      let value = toLisp(next.value, opts);
      return new Cons(value, toLisp(iterator));
    }
    // guess it wasn't an iterator after all! Oh well!
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
  let prompt = opts.prompt ?? name + "> ";
  let evaluater = opts.eval ?? (x => x);
  let print = opts.print ?? (x => console.log(name + " REPL: ", String(x), x));
  let reportError = opts.reportError ??  (x => console.log(name + " REPL ERROR: ", String(x), x));;
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
    evaluated = evaluater(expr);
  } catch (e) {
    evaluated = error;
  }
  if (expr instanceof Error) {
    reportError(expr);
    return true;  // continue REPL
  }
  print(evaluated);
  return true;  // continue REPL
}

let x = toLisp(['a', 'b', [ 1, 2, 3 ], 'c']);
console.log(x);
console.log(String(x));
console.log([...x]);
console.log([...NIL]);
console.log(String(cons('x', cons('y', NIL))));
console.log(String(cons('x', 'y')));
console.log(String(cons('x', cons('y', 'z'))));
console.log(String(NIL));
let str = `(+ b (- 1 2)) ${'\n'} 12 ( .) . 1.23 ' .23 .23.4 .23e+234 a bc b21 "asd"`;
let tokens = lispTokenGenerator(str);
let tokenList = [ ...tokens ];
console.log("tokens", tokenList);

let sExpr = parseSExpr(`(+ b (- 1 2))`);
console.log("sExpr", String(sExpr), sExpr);

sExpr = parseSExpr(`(a b 'c '(abc def))`);
console.log("sExpr", String(sExpr), sExpr);

{
  let input = [ `(a b`, ` 'c '(abc`, ` def))` ];
  lispREPL(() => input.shift());
}

let nodejs = false;
if (typeof window === 'undefined' && typeof process !== 'undefined') { // Running under node.js
  console.log("node.js");
  nodejs = true;
}

if (nodejs) {
  let fs = require('fs');
  let inputFd, closeFd;
  try {
    try {
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
      let buffer = Buffer.alloc(2000); // big enough?
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
        ok = lispREPL(getLine);
      } while (ok);
    }
  } finally {
    if (closeFd !== undefined)
      fs.closeSync(closeFd);
  }
}

console.log("done");  // breakpoint spot