//
// JavaScript Lisp
//
// Copyright 2021 Stan Switzer
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

"use strict";

// XXX make this a JS module
// XXX Make "Lisp" a function/constructor that returns a "Lisp"

class LispError extends Error {
  constructor(...args) {
    super(...args);
  }
  toString() { return this.message; }   // no type prefix on the error string
};
class EvalError extends LispError {};
class ResolveError extends LispError {};
class ParseError extends LispError {};
const LogicError = Error;

// Atoms are Symbols
const ATOMS = new Map;
function Atom(name) {
  let val = ATOMS.get(name);
  if (val !== undefined) return val;
  return Atom._defineAtom(name);
}
Atom._defineAtom = function(...names) {
  let atom = Symbol(names[0]);
  for (let name of names)
  ATOMS.set(name, atom);
  return atom;
}

const LAMBDA_ATOM = Atom._defineAtom("lambda", "\\", "\u03BB");
const SLAMBDA_ATOM = Atom._defineAtom("special-lambda", "\\\\", "\u03BB");
const CLOSURE_ATOM = Atom._defineAtom("%%closure");
const SCLOSURE_ATOM = Atom._defineAtom("%%special-closure");

const IS_CONS = Symbol("*lisp-isCons*");

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
// XXX There should be a way to do this in the class decl, but how?
Cons.prototype[IS_CONS] = true;

// I hide the Nil class because there's never any reason to
// reference it or instantiate it it more than once. Having it visible
// just invites errors. But it's good to have a distinct class for NIL
// for various reasons including that it looks better in a JS debugger.
const NIL = new (class Nil {
  constructor() {
    Object.freeze(this);
  }
  *[Symbol.iterator]() { return { next: () => ({ done: true, value: null }) } }
});

function nextCons() {
  let current = this._current, done = current === NIL, value = undefined;
  if (!done) {
    this._current = current.cdr;
    value = current.car;
  }
  return { done, value };
}

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
LazyCons.prototype[IS_CONS] = true;

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
LazyCarCons.prototype[IS_CONS] = true;

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
LazyCdrCons.prototype[IS_CONS] = true;

//
// Jisp strives to maintain JavaScript consistency wherever possibe but enough is enough.
// In Jisp, NIL, null, undefined, and false are false and everything else is true.
//
function lispToBool(val) {
  if (val === NIL || val === undefined || val === null || val === false)
    return false;
  return true;
}

const cons = (car, cdr) => new Cons(car, cdr);
const car = cons => cons.car, first = car;
const cdr = cons => cons.cdr, rest = cdr;
// Beware the order here; in JS it's in reversed
const caaar = cons => cons.car.car.car;
const caadr = cons => cons.cdr.car.car;
const caar = cons => cons.car.car;
const cadar = cons => cons.car.cdr.car;
const caddr = cons => cons.cdr.cdr.car;
const cadr = cons => cons.cdr.car;
const cdaar = cons => cons.car.car.cdr;
const cdadr = cons => cons.cdr.car.cdr
const cdar = cons => cons.car.cdr;
const cddar = cons => cons.car.cdr.cdr;
const cdddr = cons => cons.cdr.cdr.cdr;
const cddr = cons => cons.cdr.cdr;

function list(...elements) {  // easy list builder
  let val = NIL;
  for (let i = elements.length; i > 0; --i)
    val = cons(elements[i-1], val);
  return val;
}

//
// An environment is simply a Map.
// A Scope is a list of environments.
//
const Env = Map;
const Scope = Cons;
const GlobalEnv = new Env;
const GlobalScope = new Scope(GlobalEnv, NIL);
const EVAL_ARGS = Symbol("*lisp-eval-args*");
const LIFT_ARGS = Symbol("*lisp-lift-args*");
const BIGGEST_INT32 = 2**32-1;

function defineGlobalSymbol(name, val, ...aliases) {
  let opts = {};
  if (typeof aliases[0] === 'object')
    opts = aliases.shift();
  if (opts.evalArgs !== undefined) val[EVAL_ARGS] = opts.evalArgs;
  if (opts.lift !== undefined) val[LIFT_ARGS] = opts.lift;
  let atom = typeof name === 'string' ? Atom(name) : name;
  GlobalEnv.set(atom, val);
  for (let alias of aliases)
    GlobalEnv.set(Atom(name), val);
  return atom;
}

defineGlobalSymbol("defineGlobalSymbol", defineGlobalSymbol);
defineGlobalSymbol("nil", NIL);
const QUOTE_ATOM = defineGlobalSymbol("quote", quoted => quoted.car, { evalArgs: 0 }, "'");
defineGlobalSymbol("null", null); defineGlobalSymbol("undefined", undefined);
defineGlobalSymbol("cons", cons, { lift: 2 });  // lift guarantees two arguments that aren't "undefined"
defineGlobalSymbol("car", car, { lift: 1 }, "first"); // lift guarantees one argument that isn't "undefined"
defineGlobalSymbol("cdr", cdr, { lift: 1 }, "rest");
defineGlobalSymbol("caaar", caadr, { lift: 1 });
defineGlobalSymbol("caar", caar, { lift: 1 });
defineGlobalSymbol("caadr", caadr, { lift: 1 });
defineGlobalSymbol("cadar", cadar, { lift: 1 });
defineGlobalSymbol("caddr", caddr, { lift: 1 });
defineGlobalSymbol("cadr", cadr, { lift: 1 });
defineGlobalSymbol("cdaar", cdaar, { lift: 1 });
defineGlobalSymbol("cdadr", cdadr, { lift: 1 });
defineGlobalSymbol("cdar", cdar, { lift: 1 });
defineGlobalSymbol("cddar", cddar, { lift: 1 });
defineGlobalSymbol("cdddr", cdddr, { lift: 1 });
defineGlobalSymbol("cddr", cddr, { lift: 1 });
defineGlobalSymbol("pair?", a => typeof a === object && a[IS_CONS] === true);
defineGlobalSymbol("typeof", a => typeof a);
defineGlobalSymbol("undefined?", a => typeof a === 'undefined');
defineGlobalSymbol("null?", a => a === null);
defineGlobalSymbol("boolean?", a => typeof a === 'boolean');
defineGlobalSymbol("number?", a => typeof a === 'number');
defineGlobalSymbol("bigint?", a => typeof a === 'bigint');
defineGlobalSymbol("numeric?", a => typeof a === 'number' || typeof a === 'bigint');
defineGlobalSymbol("string?", a => typeof a === 'string');
defineGlobalSymbol("symbol?", a => typeof a === 'symbol');
defineGlobalSymbol("function?", a => typeof a === 'function');
defineGlobalSymbol("object?", a => typeof a === 'object');
defineGlobalSymbol("array?", a => (typeof a === 'object') && (a instanceof Array));
for (let [name, {value}] of Object.entries(Object.getOwnPropertyDescriptors(Math))) {
  // SIOD defines *pi* so I'll just define them all like that
  if (typeof value === 'number')
    name = `*${name.toLowerCase()}*`;
  // SIOD defines sin, cos, asin, etc. so I'll just define them all like that
  if (typeof value === 'number' || typeof value === 'function')
    defineGlobalSymbol(name, value);
}
defineGlobalSymbol("abs", a => a < 0 ? -a : a);  // unlike Math.abs, this deals with bigint too
defineGlobalSymbol("intern", a => Atom(a));
defineGlobalSymbol("Symbol", a => Symbol(a));  // XXX name?
defineGlobalSymbol("toArray", a => toArray(a));
defineGlobalSymbol("toLisp", a => toLisp(a));
defineGlobalSymbol("toString", a => lispToString(a));
defineGlobalSymbol("toNumber", a => Number(a));
defineGlobalSymbol("toBigInt", a => BigInt(a));
defineGlobalSymbol("lispTokens", (a, b) => [ ... lispTokenGenerator(a), b ]);
defineGlobalSymbol("read-from-string", a => parseSExpr(a));
defineGlobalSymbol("eval", (expr, rest) => {
  env = GlobalEnv;
  if (rest && rest[IS_CONS])
    env = rest.car;
  if (typeof expr === 'string')
    expr = parseSExpr(expr);
  return lispEval(expr);
}, { lift: 1 });

// Pokemon gotta catch 'em' all!
defineGlobalSymbol("!", a => !lispToBool(a), "not");
defineGlobalSymbol("~", a => ~a, "bit-not");
defineGlobalSymbol("**", (a,b) => a ** b, "exp");
defineGlobalSymbol("%", (a,b) => a % b, "rem");
defineGlobalSymbol("<<", (a,b) => a << b, "bit-shl");
defineGlobalSymbol(">>", (a,b) => a >> b, "ash");
defineGlobalSymbol(">>>", (a,b) => a >>> b, "bit-ushr");
defineGlobalSymbol("in", (a,b) => a in b);
defineGlobalSymbol("new", (cls, ...args) => new cls(...args));
defineGlobalSymbol("instanceof", (a,b) => a instanceof b);

// variable args
defineGlobalSymbol("+", (...args) => {
  let a = 0, first = true;
  for (let b of args) {
    if (first) {
      first = false;
      if (typeof b === 'bigint')
        a = 0n;
      else if (typeof b === 'string')
        a = "";
    }
    a += b;
  }
  return a;
}, "add");

defineGlobalSymbol("-", (a, ...rest) => {
  if (rest.length === 0) return -a;  // XXX ???
  for (let b of rest)
    a -= b;
  return a;
}, "sub");

defineGlobalSymbol("*", (...args) => {
  let a = 1, first = true;
  for (let b of args) {
    if (first) {
      first = false;
      if (typeof b === 'bigint')
        a = 1n;
    }
    a *= b;
  }
  return a;
}, "mul");

defineGlobalSymbol('/', (a, ...rest) => {
  if (rest.length === 0) return 1/a;  // XXX ???
  for (let b of rest)
    a /= b;
  return a;
}, "/");

defineGlobalSymbol("&", (...args) => {
  let a = ~0;
  for (let b of args)
    a &= b;
  return a;
}, "bit-and");

defineGlobalSymbol("|", (...args) => {
  let a = 0;
  for (let b of args)
    a |= b;
  return a;
}, "bit-or");

defineGlobalSymbol("^", (...args) => {
  let a = 0;
  for (let b of args)
    a ^= b;
  return a;
}, "bit-xor");

defineGlobalSymbol("&", (...args) => {
  let a = ~0;
  for (let b of args)
    a &= b;
  return a;
}, "bit-and");

// XXX todo: do all of these without lifting to array
defineGlobalSymbol("<", function(a, ...rest) {
  if (rest.length === 0) return false; // not less than itself?
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a < b)) return false;
    a = b;
  }
  return true;
}, { evalArgs: 1 }, "lt");

defineGlobalSymbol("<=", function(a, ...rest) {
  if (rest.length === 0) return true; // equal to itself?
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a <= b)) return false;
    a = b;
  }
  return true;
}, { evalArgs: 1 }, "le");

defineGlobalSymbol(">", function(a, ...rest) {
  if (rest.length === 0) return false;
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a > b)) return false;
    a = b;
  }
  return true;
}, { evalArgs: 1 }, "gt");

defineGlobalSymbol(">=", function(a, ...rest) {
  if (rest.length === 0) return true;
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a >= b)) return false;
    a = b;
  }
  return true;
}, { evalArgs: 1 }, "ge");

defineGlobalSymbol("==", function(a, ...rest) {
  if (rest.length === 0) return true;
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a == b)) return false;
  }
  return true;
}, { evalArgs: 1 }, "equal?");

defineGlobalSymbol("===", function(a, ...rest) {
  if (rest.length === 0) return true;
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a === b)) return false;
  }
  return true;
}, { evalArgs: 1 }, "eq?");

defineGlobalSymbol("!=", function(a, ...rest) {  // all not equal to first
  if (rest.length === 0) return false;
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a != b)) return false;
  }
  return true;
}, { evalArgs: 1 }, "ne");

defineGlobalSymbol("!==", function(a, ...rest) {  // all not equal to first
  if (rest.length === 0) return false;
  for (let b of rest) {
    b = lispEval(b, this);
    if (!(a !== b)) return false;
  }
  return true;
}, { evalArgs: 1 }, "nee");

// logical & conditional

defineGlobalSymbol("&&", function(...args) {
  let a = true;
  for (a of args) {
    a = lispEval(b, this);
    if (!lispToBool(a)) return a;
  }
  return a;
}, { evalArgs: 0 }, "and");

defineGlobalSymbol("||", function(...args) {
  let a = false;
  for (a of args) {
    a = lispEval(b, this);
    if (lispToBool(a)) return a;
  }
  return a;
}, { evalArgs: 0 }, "or");

defineGlobalSymbol("?", function(predicate, trueBlock, falseBlock) {
  let res;
  if (lispToBool(predicate))
    res = lispEval(trueBlock, this);
  else
    res = lispEval(falseBlock, this);
  return res;
}, { evalArgs: 1, lift: 3 }, "if");

// JavaScripty things:
//   XXX TODO: delete, setting

defineGlobalSymbol("@", (a, b) => a[b], "aref");  // indexing and member access
defineGlobalSymbol("?@", (a, b) => a?.[b]);  // conditional indexing and member access
defineGlobalSymbol("toLisp", toLisp);
defineGlobalSymbol("toArray", toArray);
defineGlobalSymbol("NaN", NaN);
defineGlobalSymbol("Infinity", Infinity);
defineGlobalSymbol("isFinite", isFinite);
defineGlobalSymbol("isNaN", isNaN);
defineGlobalSymbol("isFinite", isFinite);
defineGlobalSymbol("parseFloat", parseFloat);
defineGlobalSymbol("parseInt", parseInt);

// SIOD compatibility checklist:
//
// (*catch tag body ...)
// (*throw tag value)
// *pi*
// (acos x), etc from Math
// (append l1 l2 l3 l4 ...)
// (last list)
// (length)
// (list item1 item2 ...) - Conses up its arguments into a list.
// (reverse x) -- Returns a new list which has elements in the reverse order of the list x.
// (butlast x)
//    Returns a new list which has all the elements of the argument x except for the last element.
// (apply function arglist)
// (apropos substring) -- returns a list of all symbols containing the given substring.
// (ass key alist function)
//    Returns the first element of the alist such that the function applied to car of the element and the key returns a non-null value. For example:/
// (assoc key alist) -- Same as (ass key alist equal?).
// (assq key alist) -- Same as (ass key alist eq?).
// (assv key alist) --Same as (ass key alist eql?).
// (begin form1 form2 ...)
//   A special form which evaluates each of its subforms one after another, returning the value of the last subform.
// TODO benchmark fns -- http://people.delphiforums.com/gjc//siod.html#builtin
// (realtime)
//      Returns a double precision floating point value representation of the current realtime number of seconds. Usually precise to about a thousandth of a second.
// (begin form1 form2 ...)
// (define variable value)
// (cond clause1 clause2 ...)  -- clause is (predicate-expression form1 form2 ...)
// define needs to do procedures too -- by rewriting to begin clause
// errobj, (error message object)
// (exp x) -- Computes the exponential function of x.
// (log x)
// (let (binding1 binding2 ...) form1 form2 ...) -- let* behavior
//     (let ((x 10)
//      (y 20))
//      (+ x y))
// (letrec...) -- ?
// (load fname noeval-flag search-flag)
//   If the neval-flag is true then a list of the forms is returned otherwise the forms are evaluated.
//   no use for the search-flag
// (mapcar fcn list1 list2 ...)
//     Returns a list which is the result of applying the fcn to the elements of each of the lists specified.
// (max x1 x2 ...) -- also min
// (member key list)
//     Returns the portion of the list where the car is equal to the key, or () if none found.
// (memq key list)
//     Returns the portion of the list where the car is eq to the key, or () if none found.
// (nth index list)
//     Reference the list using index, with the first element being index 0.
// (null? x)  -- Returns true of x is the empty list.
// (number->string x base width precision)
//     Formats the number according to the base, which may be 8, 10, 16 or the symbol e or f.
//     The width and precision are both optional.
// (number? x) -- Returns true of x is a number.
// (pair? x) -- Returns true if x is a pair (created by cons).
// (parse-number str)
// (pow x y) -- Computes the result of x raised to the y power.
// (print object stream) -- Same as prin1 followed by output of a newline.
// (prog1 form1 form2 form3 ...)
//     A special form which evaluates all its subforms but returns the value of the first one.
// (qsort list predicate-fcn access-fcn)
// (quote x)
// (rand modulus) -- Computes a random number from 0 to modulus-1. Uses C library rand. (srand seed)
// (random modulus) -- Computes a random number from 0 to modulus-1. Uses C library random. (srandom seed)
// (require path)
//   Computes a variable name by concatenating "*" + path + "-loaded*" and then calling (load path nil t)
//   if and only if the variable is not bound to true. After the file is loaded the variable is set to true. This is the correct way of making sure a file is only loaded once.
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

// maybe some relation between of generators and Lazy eval?
// Promises

// Maybe think about bigint some more. Here's factoral now:
//   (define (factoral x) (? (<= x 1) 
//     (? (isBigInt x) 1n 1)
//     (* x (factoral (- x (? (isBigInt x) 1n 1))))
//   ))l

// (lambda (args) (body1) (body2) ...) -- returns (%%closure scope args bodies)
defineGlobalSymbol(LAMBDA_ATOM, function(formalParams, bodies) {
  return list(CLOSURE_ATOM, this, formalParams, bodies);
}, { evalArgs: 0, lift: 1 });

// (lambda (args) (body1) (body2) ...) -- returns (%%closure scope args bodies)
defineGlobalSymbol(SLAMBDA_ATOM, function(formalParams, bodies) {
  return list(SCLOSURE_ATOM, this, formalParams, bodies);
}, { evalArgs: 0, lift: 1 });

//
// try/catch/filnally. Just a sketch for now.
//

defineGlobalSymbol("*throw", e => { throw e });

defineGlobalSymbol("*finally", function (handler, body) {  // XXX order of args?
  let val = NIL;
  try { val = lispEval(body, this); }
  finally { lispEval(handler, this); }
  return val;
}, { evalArgs: 0, lift: 2 });

defineGlobalSymbol("*catch", function(e, handler, body) {  // XXX order of args?
  // Tempting to save some of this work for the handler, but better to report errors early
  let cls, sym;
  if (typeof e === 'symbol') {
    sym = e;
  } else if (e[IS_CONS]) {
    cls = e.car;
    if (typeof cls !== 'function')
      cls = lispEval(cls, this);
    if (typeof cls !== 'function')
      throw new EvalError(`Not a class ${cls}`);
    if (e.cdr[IS_CONS])
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
}, { evalArgs: 0, lift: 3 });

defineGlobalSymbol("define", (nameAndArgs, body) => {
  let name = nameAndArgs.car;
  let args = nameAndArgs.cdr;
  defineGlobalSymbol(name, list(LAMBDA_ATOM, args, body));
  return name;
}, { evalArgs: 0, lift: 2 });

//
// This is where the magic happens
//

function lispEval(expr, scope = GlobalScope) {
  if (expr === NIL) return expr;
  if (typeof expr === 'symbol') {
    let val = resolveSymbol(expr, scope);
    if (val === undefined) throw new ResolveError(`Undefined symbol ${expr.description}`);
    return val;
  }
  if (typeof expr === 'object' && expr[IS_CONS]) {
    let fn = expr.car, args = expr.cdr;
    if (typeof fn !== 'function')
      fn = lispEval(fn, scope);
    return lispApply(fn, args, scope);
  }
  return expr;
}

function lispApply(fn, args, scope) {
  if (typeof fn === 'function') {
    let evalCount = fn[EVAL_ARGS] ?? BIGGEST_INT32;
    if (evalCount > 0)
      args = evalArgs(args, scope, evalCount);
    let lift = fn[LIFT_ARGS] ?? BIGGEST_INT32;
    let jsArgs = [], noPad = lift === BIGGEST_INT32;
    while (lift > 0) {
      // Promote "lift" arguments to JS arguments, filling with NIL
      if (typeof args === 'object' && args[IS_CONS]) {
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
    return fn.apply(scope, jsArgs);  // "this" is the scope!
  }
  if (typeof fn === 'object' && fn[IS_CONS]) {
    let opSym = fn.car;
    if (opSym === LAMBDA_ATOM || opSym === SLAMBDA_ATOM) {
      let formalParams = fn.cdr.car;
      let body = fn.cdr.cdr.car;  // XXX bodies = fn.cdr.cdr
      if (opSym === LAMBDA_ATOM || opSym === CLOSURE_ATOM)
        args = evalArgs(args, scope);
      let env = new Env;
      scope = new Scope(env, scope);
      let origFormalParams = formalParams;
      while (formalParams !== NIL && formalParams[IS_CONS]) {
        let param = formalParams.car;
        if (typeof param !== 'symbol') throw new EvalError(`Param must be a symbol ${param}`);
        if (args !== NIL) {
          env.set(param, args.car);
          args = args.cdr
        } else {
          env[param] = NIL;
        }
        formalParams = formalParams.cdr;
      }
      if (typeof formalParams === 'symbol')  // Neat trick for 'rest' params!
        env[formalParams] = args;
      else if (formalParams !== NIL)
        throw new EvalError(`Bad parameter list ${lispToString(origFormalParams)}`);
      return lispEval(body, scope);
    }
  }
  throw new EvalError(`Can't apply ${fn}`);
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
  if (!(args[IS_CONS]))
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

// XXX TODO: Line wrapping
function lispToString(obj, maxDepth = 1000, opts, moreList, quoted) {
  if (maxDepth <= 0) return "...";
  let objType = typeof obj;
  if (obj === NIL) return "()";
  if (obj === undefined) return "undefined";
  if (obj === null) return "null";   // remember: typeof null === 'object'!
  if (objType === 'object') {
    if (obj[IS_CONS]) {
      let before = "(", after = ")";
      if (quoted)
        before = after = "";
      else if (moreList)
        before = " ", after = "";
      if (opts?.quoteNotation && obj.car === QUOTE_ATOM) {
        before = moreList ? " " : "";
        return before + "'" + lispToString(obj.cdr, maxDepth-1, opts, true, true);
      }
      if (obj.cdr === NIL)
        return before + lispToString(obj.car, maxDepth-1, opts) + after;
      if (obj.cdr[IS_CONS])
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
  if (objType === 'bigint') {
    return `${String(obj)}n`;
  }
  // XXX there must be more to do here
  return String(obj);
}

const TO_LISP = Symbol("*lisp-to-lisp*");

// Turn iterables objects like arrays into lists
function toLisp(obj, depth = BIGGEST_INT32, opts) { 
  if (depth <= 0)return obj;
  let iterator;
  if (typeof obj === 'function') // assume it's an iterator
    iterator = obj;
  if (typeof obj === 'object') {
    if (obj[IS_CONS]) return obj;  // Careful; cons is iterable itself
    if (obj[TO_LISP])
      return obj[TO_LISP](opts);
    let iterator;
    if (obj[Symbol.iterator])
      iterator = obj[Symbol.iterator]();
  }
  if (iterator) {
    function go() {
      let { done, value } = iterator.next();
      if (done) return NIL;
      return cons(toLisp(obj, depth-1, opts), go());
    }
    return go();
  }
  return obj;
}

function toArray(obj, depth = BIGGEST_INT32) {
  if (depth <= 0) return obj;
  if (obj === NIL) return [];
  if (!obj[IS_CONS]) return obj;
  let arr = [];
  while (obj !== NIL && obj[IS_CONS]) {
    arr.push(toArray(obj.car), depth-1);
    obj = obj.cdr;
  }
  return arr;
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
        throw new LogicError(`Not an iterator or iterable ${origParam}`);
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
        if (operatorPrefix && DIGITS[ch])  // a prefix of operators breaks at a digit
          break;
        if (!OPERATORS[ch]) operatorPrefix = false;
        str += ch, nextc();
      }
      yield { type: 'atom', value: Atom(str) };
      continue;
    }

    yield { type: 'garbage', value: ch };
    nextc()
  }
  yield { type: 'end' };
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
        throw new LogicError(`Not an iterator or iterable ${origParam}`);
    }
  }
  replHints.currentPrompt = prompt;

  let _toks = [], _done = false;
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
      str += sep + (tok.value !== undefined ? lispToString(tok.value) : tok.type);
      sep = " ";
    }
    for (;;) {
      let { done, value: tok } = tokenGenerator.next();
      if (done) return str;
      if (tok.type === 'newline' || tok.type === 'end')
        return str;
      let val = tok.value;
      str += sep + (tok.value !== undefined ? lispToString(tok.value) : tok.type);
      sep = " ";
    }
  }

  function parseExpr(promptStr) {
    // This will not peek across a linebreak because the newline token will foil it
    let dotNext = token(1).type === '.';

    if (!dotNext && (token().type === 'atom' || token().type === 'string' ||
        token().type === 'number')) {
      let thisToken = consumeToken();
      return thisToken.value;
    }

    if (dotNext) {
      let firstToken = consumeToken();
      if (consumeToken().type !== '.') throw new Error("Logic error; should be a dot");
      return cons(firstToken.value, parseExpr());
    }

    if (token().type === '(') {
      let newPrompt = promptStr + promptMore;
      replHints.currentPrompt = newPrompt;
      consumeToken();
      function parseListBody() {
        if (token().type === ')') {
          replHints.currentPrompt = promptStr;
          consumeToken();
          return NIL;
        }
        if (token().type === 'end') throw new ParseError(`Parse error; reached end of input`);
        let first = parseExpr(newPrompt);
        let rest = parseListBody();
        return cons(first, rest);
      }
      return parseListBody();
    }

    if (token().type === '[') {  // JavaScript Array, oddly enough!
      let res = [];
      let newPrompt = promptStr + promptMore;
      replHints.currentPrompt = newPrompt;
      consumeToken();
      for (;;) {
        let item = parseExpr(newPrompt);
        res.push(item);
        if (token().type === ',')  // Comma might as well be optional for now
          consumeToken();
        if (token().type === ']') {
          consumeToken();
          return res;
        }
        if (token().type === 'end') throw new ParseError(`Parse error; reached end of input`);
      }
    }

    if (token().type === '{') {  // JavaScript Object literal too!
      let res = {};
      let newPrompt = promptStr + promptMore;
      replHints.currentPrompt = newPrompt;
      consumeToken();
      for (;;) {
        if (token().type === '}') {
          consumeToken();
          break;
        }
        let gotIt = false;
        if (token().type === 'atom' || token().type === 'string' || token().type === 'number') {
          let sym = token().value;
          if (typeof sym === 'symbol')
            sym = sym.description;
          consumeToken();
          if (token().type === ':') {
            consumeToken();
            let val = parseExpr(newPrompt);
            res[sym] = val;
            gotIt = true;
            if (token().type === ',')  // might as well be optional for now
              consumeToken();
          }
          if (token().type === 'end') throw new ParseError(`Parse error; reached end of input`);
        }
        if (!gotIt)
          throw new ParseError(`Bad JavaScript object literal`); // XXX details
      }
      return res;
    }

    if (token().type === "'") {
      let newPrompt = promptStr + quotePromptMore;
      consumeToken();
      replHints.currentPrompt = newPrompt;
      let quoted = parseExpr(newPrompt);
      replHints.currentPrompt = promptStr;
      return cons(QUOTE_ATOM, cons(quoted, NIL));
    }

    if (token(1).type === 'end')
      return null;
    throw new ParseError(`Unexpected token ${token().type} ${token().value}`);
  }

  // I'm old enough to be fond of the EvalQuote REPL.
  // So, as a special case, transform "symbol(a b c)" into "(symbol (quote a) (quote b) (quote c))"
  let expr;
  if (token().type === 'atom' && token(1).type === '(') {
    function quoteArgs(args) {
      if (args === NIL || !(args[IS_CONS])) return NIL;
      let quoted = args.car;
      quoted = cons(QUOTE_ATOM, cons(quoted, NIL))
      return cons(quoted, quoteArgs(args.cdr));
    }
    let tok = consumeToken();
    let sym = tok.value;
    let quoted = parseExpr(prompt);
    if (quoted)
      expr = cons(sym, quoteArgs(quoted));
  } else {
    // Modern form
    expr = parseExpr(prompt);
  }

  let unparsed = unParesedInput();
  if (!unparsed)
    return expr;
  throw new ParseError(`Unparsed: ${unparsed}`);
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
      let line = readline(replHints.currentPrompt);
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

let x = toLisp(['a', 'b', [ 1, 2, 3 ], 'c']);
console.log("Test toList", x);
console.log("Test toString", String(x));
console.log("Test NIL toString", String(NIL));
console.log("Test iterable", [...x]);
console.log("Test NIL iterable", [...NIL]);
console.log("Test cons", String(cons('x', cons('y', NIL))));
console.log("Test cons", String(cons('x', 'y')));
console.log("Test cons", String(cons('x', cons('y', 'z'))));
let str = `12b +5 -3.2e7 15b (+ b (- 1 2)) ${'\n'} 12 ( .) . 1.23 ' .23 .23.4 .23e+234 a bc b21 "asd"`;
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