// Lisp in JavaScript
"use strict";

const { rootCertificates } = require('tls');

class Nil {  // could have used undefined, but nah.
  static NIL = new Nil;
  constructor() {
    if (Nil.NIL) return Nil.NIL;
    Object.freeze(this);
  }
  *[Symbol.iterator]() {
  }
  toString() {
    return lispToString(this);
  }
}
const nil = Nil.NIL;
// const nil = undefined;  // Maybe try it anyway

class Cons {
  constructor(car, cdr) {
    this.car = car ?? nil;
    this.cdr = cdr ?? nil;
    Object.freeze(this);
  }

  *[Symbol.iterator]() {
    let next = this;
    while (next !== nil) {
      const nextNext = next.cdr;
      yield next.car;
      next = nextNext;
    }
  }

  toString() {
    return lispToString(this);
  }
}

const cons = (car, cdr) => new Cons(car, cdr);
const car = (cons) => cons.car, first = car;
const cdr = (cons) => cons.cdr, rest = cdr;

class Atom {
  name;
  constructor(name) {
    let mapped = Atom.ATOMS.get(name);
    if (mapped) return mapped;
    this.name = name;
    Object.freeze(this);
    Atom.ATOMS.set(name, this);
  }

  static ATOMS = new Map();
  static QUOTE = new Atom("'");

  toString() {
    return lispToString(this);
  }
}

function lispToString(obj, opts, moreList, quoted) {
  let objType = typeof obj;
  if (objType === 'object') {
    let tailCons = obj.cdr instanceof Cons;
    let tailNil = !tailCons && obj.cdr instanceof Nil;
    if (obj instanceof Cons) {
      let before = "(", after = ")";
      if (quoted)
        before = after = "";
      else if (moreList)
        before = " ", after = "";
      if (obj.car === Atom.QUOTE) {
        before = moreList ? " " : "";
        return before + "'" + lispToString(obj.cdr, opts, true, true);
      }
      if (tailNil)
        return before + lispToString(obj.car, opts) + after;
      if (tailCons)
        return before +
            lispToString(obj.car, opts) +
            lispToString(obj.cdr, opts, true) +
            after;
      return before + lispToString(obj.car, opts) + " . " + lispToString(obj.cdr, opts) + after;
    }
    if (obj instanceof Nil) {
      return "()";
    }
    if (obj instanceof Atom) {
      return obj.name;
    }
  }
  if (objType === 'string') {
    let str = '"';
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
    return nil;
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
        return nil;
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
const IDENT1 = ALPHA + "_$";
const IDENT2 = IDENT1 + DIGITS;

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

    if ("()+-*/%)".includes(ch)) {
      yield { type: 'operator', value: ch };
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
      yield { type: 'atom', value: str };
      continue;
    }

    yield { type: 'garbage', value: ch };
    nextc()
  }
}

function parseSExpr(tokenGenerator, depthReporter = {}) {
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

  let parseDepth = 0;
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

  function parseExpr() {
    depthReporter.parseDepth = parseDepth;
    ++parseDepth;

    let expr = parseExpr2();

    --parseDepth;
    depthReporter.parseDepth = parseDepth;

    return expr;
  }

  function parseExpr2() {
    // This will not peek across a linebreak because the newline token will foil it
    let dotNext = peekToken().type === '.';

    if (!dotNext && (token().type === 'atom' || token().type === 'string' ||
        token().type === 'number' || token().type === 'operator')) {
      let thisToken = consumeToken();
      if (thisToken.type === 'atom' || thisToken.type === 'operator')
        return new Atom(thisToken.value);
      return thisToken.value;
    }

    if (dotNext) {
      let firstToken = consumeToken();
      if (consumeToken().type !== '.') throw new Error("Logic error; should be a dot");
      return cons(firstToken.value, parseExpr());
    }

    if (token().type === '(') {
      consumeToken();
      function parseListBody() {
        if (token().type === ')') {
          consumeToken();
          return nil;
        }
        let first = parseExpr();
        let rest = parseListBody();
        return cons(first, rest);
      }
      return parseListBody();
    }

    if (token().type === "'") {
      consumeToken();
      let quoted = parseExpr();
      return cons(Atom.QUOTE, cons(quoted, nil));
    }

    throw new Error(`Unexpected token ${token().type} ${token().value}`);
  }
  let expr = parseExpr();
  let peek = peekToken();
  let viable = peek.type === 'end' || peek.type === 'newline';
  if (viable)
    return expr;
  return new Error(`Unparsed: ${peek.type} ${peek.value}`); // XXX better message?
}

function lispREPL(promptInput, opts = {}) {
  // promptInput(prompt) => str
  let name = opts.name ?? "Jisp";
  let prompt = opts.prompt ?? name + "> ";
  let evaluater = opts.eval ?? (x => x);
  let print = opts.print ?? (x => console.log(name + " REPL", String(x), x));
  let reportError = opts.reportError ?? print;
  let depthReporter = { parseDepth: 0 };
  function* charStreamPromptInput() {
    for(;;) {
      let line = promptInput(prompt + ("  ".repeat(depthReporter.parseDepth)));
      if (line == null) return; // == intended
      while (line.endsWith('\n') || line.endsWith('\r'))
        line = line.substr(0, line.length-1);
      if (line.length === 0) return;  // End with any blank line
      for (let ch of line)
        yield ch;
      yield '\n';
    }
  }
  let charStream = charStreamPromptInput();
  let tokenGenerator = lispTokenGenerator(charStream);
  // depthReporter.parseDepth = 0; // XXX is this necessary?
  // XXX try/catch?
  let expr = parseSExpr(tokenGenerator, depthReporter);
  if (!expr) return false;
  if (expr instanceof Error) {
    reportError(expr);
    return false;
  }
  // XXX try/catch?
  let evaled = evaluater(expr);
  print(evaled);
  return true;
}

let x = toLisp(['a', 'b', [ 1, 2, 3 ], 'c']);
console.log(x);
console.log(String(x));
console.log([...x]);
console.log([...nil]);
console.log(String(cons('x', cons('y', nil))));
console.log(String(cons('x', 'y')));
console.log(String(cons('x', cons('y', 'z'))));
console.log(String(nil));
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