//
// JavaScript Lisp
//
// Copyright 2021 Stan Switzer
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

"use strict";
// TODO: make this a JS module

function Jisp(lispOpts = {}) {
  if (new.target === undefined) return new Jisp(lispOpts);

  //
  // Encapsulating everything in a function scope because JITs
  // can resolve scoped references most easily.
  //

  class LispError extends Error {};
  LispError.prototype.name = "LispError";

  class EvalError extends LispError {};
  EvalError.prototype.name = "EvalError";

  class ParseError extends LispError {};
  ParseError.prototype.name = "ParseError";

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
  const IS_CONS = Symbol("*lisp-isCons*");

  class Cons {
    // I want creating a Cons to be as cheap as possible, so no subclassing
    // or calls to super. This means that identifying the "class" of Cons
    // cells can't use "instanceof AbstractCons".
    // For now, cons cells are immutable. That might or might not help the JIT
    // but in any case if that decision changes, Object.freeze(this) should
    // become Object.seal().
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
  // There should be a way to do this in the class decl, but not in ES6
  Cons.prototype[IS_CONS] = true;

  // A word about testing for Cons cells. The idiom is
  //    typeof obj === 'object' && obj[IS_CONS]
  // The typeof check is basically free since the JIT always knows the type
  // of an object. And fetching properties is something the optimizer makes
  // as fast as possible. It's probably even as fast as or faster than "instanceof".
  // This should probebly be a function, but oddly enough
  // I don't want to trust the compiler to inline it.
  //
  // Beware when traversing lists. Things purported to be lists might not be
  // and although lists are conventionally NIL-terminated, the final "cdr"
  // could be anything at all.

  // Hide the Nil class because there's never any reason to
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
    // Give priority here to actual true and false values
    if (val === true) return true;
    if (val === false || val === NIL || val === undefined || val === null)
      return false;
    return true;
  }

  const cons = (car, cdr) => new Cons(car, cdr);
  const car = cons => cons.car;
  const cdr = cons => cons.cdr;
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
  const MAX_SMALL_INTEGER = 2**30-1;

  function defineGlobalSymbol(name, val, ...aliases) {
    let opts = {};
    if (typeof aliases[0] === 'object')
      opts = aliases.shift();
    if (typeof val === 'function') {
      if (opts.evalArgs !== undefined) val[EVAL_ARGS] = opts.evalArgs;
      if (opts.lift !== undefined) val[LIFT_ARGS] = opts.lift;
    }
    let atom = typeof name === 'symbol' ? name : Atom(String(name));
    GlobalEnv.set(atom, val);
    for (let alias of aliases)
      GlobalEnv.set(Atom(alias), val);
    return atom;
  }

  defineGlobalSymbol("defineGlobalSymbol", defineGlobalSymbol);
  defineGlobalSymbol("nil", NIL);
  const QUOTE_ATOM = defineGlobalSymbol("quote", quoted => quoted, { evalArgs: 0 }, "'");
  defineGlobalSymbol("null", null);
  defineGlobalSymbol("true", true);
  defineGlobalSymbol("false", false);
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
  defineGlobalSymbol("toArray", a => listToArray(a));
  defineGlobalSymbol("toLisp", a => iterableToList(a));
  defineGlobalSymbol("toString", a => lispToString(a));
  defineGlobalSymbol("toNumber", a => Number(a));
  defineGlobalSymbol("toBigInt", a => BigInt(a));
  defineGlobalSymbol("lispTokens", (a, b) => [ ... lispTokenGenerator(a), b ]);
  defineGlobalSymbol("read-from-string", a => parseSExpr(a));

  defineGlobalSymbol("eval", function(expr, rest) {
    let scope = NIL;
    if (rest === undefined || rest === NIL)
      scope = this;  // use the current scope if unspecified
    // The "rest" parameter is unlifted, so it's a list of arguments.
    // Which means the first argument is rest.car
    // Note the user can set a deliberately enmpty scope for whatever reason.
    if (typeof rest === 'object' && rest[IS_CONS])
      scope = rest.car;
    if (typeof expr === 'string')
      expr = parseSExpr(expr);
    return lispEval(expr, scope);
  }, { lift: 1 });

  defineGlobalSymbol("apply", function(fn, args, rest) {
    let scope = NIL;
    if (rest === undefined || rest === NIL)
      scope = this;  // use the current scope if unspecified
    // The "rest" parameter is unlifted, so it's a list of arguments.
    // Which means the first argument is rest.car
    // Note the user can set a deliberately enmpty scope for whatever reason.
    if (typeof rest === 'object' && rest[IS_CONS])
      scope = rest.car;
    return lispApply(fn, args, scope);
  }, { lift: 2 });

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
    if (rest.length === 0) return -a;
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

  // XXX todo: do all of these without lifting to JS
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

  // (begin form1 form2 ...)
  defineGlobalSymbol("begin", function begin(...forms) {
    let res = NIL;
    for (let form of forms)
      res = lispEval(form, this);
    return res;
  }, { evalArgs: 0 });

  // (prog1 form1 form2 form3 ...)
  defineGlobalSymbol("prog1", function(...forms) {
    let res = NIL, first = true;
    for (let form of forms) {
      let val = lispEval(form, this);
      if (first)
        res = val;
      first = false;
    }
    return res;
  }, { evalArgs: 0 });

  // (cond clause1 clause2 ...)  -- clause is (predicate-expression form1 form2 ...)
  defineGlobalSymbol("cond", function(...clauses) {
    for (let clause of clauses) {
      if (!(typeof clause === 'object' && clause[IS_CONS]))
        throw new EvalError(`Bad clause in "cond" ${lispToString(clause)}`);
      let pe = clause.car, forms = clause.cdr;
      let evaled = lispEval(pe, this);
      if (lispToBool(evaled)) {
        let res = NIL;
        while (typeof forms === 'object' && forms[IS_CONS]) {
          res = lispEval(forms.car, this);
          forms = forms.cdr;
        }
        return res;
      }
    }
    return NIL;
  }, { evalArgs: 0 });

  // JavaScripty things:
  //   XXX TODO: "delete", setting props and array elements
  defineGlobalSymbol("@", (a, b) => b[a]);  // indexing and member access
  defineGlobalSymbol("?@", (a, b) => b?.[a]);  // conditional indexing and member access
  defineGlobalSymbol("toLisp", iterableToList);
  defineGlobalSymbol("toArray", listToArray);
  defineGlobalSymbol("NaN", NaN);
  defineGlobalSymbol("Infinity", Infinity);
  defineGlobalSymbol("isFinite", isFinite);
  defineGlobalSymbol("isNaN", isNaN);
  defineGlobalSymbol("isFinite", isFinite);
  defineGlobalSymbol("parseFloat", parseFloat);
  defineGlobalSymbol("parseInt", parseInt);

  defineGlobalSymbol("require", function lispRequire(path, force) {
    let sym = Atom(`*${path}-loaded*`);
    if (lispToBool(force) || !lispToBool(resolveSymbol(sym, GlobalScope))) {
      try {
        // For now, nodejs-specific
        const fs = require('fs');
        let fileContent = fs.readFileSync(path);
        fileContent = fileContent.toString();
        // The REPL can handle this!
        // XXX Pass in scope, but which one?
        lispREPL(function readline() {
          let line = fileContent;
          fileContent = null;
          return line;
        });
        GlobalEnv.set(sym, true);
      } catch (error) {
        console.log("require failed", String(error));
        return false;
      }
    }
    return sym;
  });

  defineGlobalSymbol("append", (...args) => {
    let res = NIL;
    function zip(list) {
      if (list === NIL) return;
      if (typeof list === 'object' && list[IS_CONS]) {
        zip(list.cdr);
        res = cons(list.car, res);
      }
      else  // What to do with a node that's not a list?
        res = cons(list, res);  // Add it to the list, I guess.
    }
    for (let i = args.length; i > 0; --i)
      zip(args[i-1]);
    return res;
  });

  defineGlobalSymbol("last", (list) => {
    while (typeof list === 'object' && list[IS_CONS]) {
      let next = list.cdr;
      if (next === NIL)
        return list.car;
      list = next;
    }
    return list;  // list was empty or not terminated with NIL
  });

  defineGlobalSymbol("length", (list) => {
    let n = 0;
    while (typeof list === 'object' && list[IS_CONS]) {
      n += 1;
      list = list.cdr;
    }
    return n;
  });

  defineGlobalSymbol("list", (...args) => {
    let res = NIL;
    for (let i = args.length; i > 0; --i)
      res = cons(args[i-1], res);
    return res;
  });

  defineGlobalSymbol("reverse", (list) => {
    let res = NIL;
    while (typeof list === 'object' && list[IS_CONS]) {
      res = cons(list.car, res)
      list = list.cdr;
    }
    return res;
  });

  defineGlobalSymbol("butlast", (list) => {
    let res = NIL;
    if (!(typeof list === 'object' && list[IS_CONS])) return NIL;
    function zip(list) {
      let next = list.cdr;
      if (typeof next === 'object' && next[IS_CONS]) {
        zip(list.cdr);
        res = cons(list.car, res);
      }
    }
    zip(list);
    return res;
  });

  // (apropos substring) -- Returns a list of all symbols containing the given substring
  defineGlobalSymbol("apropos", (substring) => {
    substring = substring.toLowerCase();
    let matches = [];
    for (let [name, _value] of GlobalEnv) {
      name = lispToString(name);
      if (name.toLowerCase().includes(substring))
        matches.push(name);
    }
    matches.sort();
    return iterableToList(matches);
  });

  // (mapcar fcn list1 list2 ...)
  //     Returns a list which is the result of applying the fcn to the elements of each of the lists specified.
  // (max x1 x2 ...) -- also min


  // SIOD compatibility checklist:
  //
  // (*catch tag body ...)
  // (*throw tag value)
  //   A special form which evaluates each of its subforms one after another, returning the value of the last subform.
  // TODO benchmark fns -- http://people.delphiforums.com/gjc//siod.html#builtin
  // (realtime)
  //      Returns a double precision floating point value representation of the current realtime number of seconds. Usually precise to about a thousandth of a second.
  // errobj, (error message object)
  // (let (binding1 binding2 ...) form1 form2 ...) -- let* behavior
  //     (let ((x 10)
  //      (y 20))
  //      (+ x y))
  // (letrec...) -- ?
  // (load fname noeval-flag search-flag)
  //   If the neval-flag is true then a list of the forms is returned otherwise the forms are evaluated.
  //   no use for the search-flag
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
  //     A special form which evaluates all its subforms but returns the value of the first one.
  // (qsort list predicate-fcn access-fcn)
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

  // maybe some relation between of generators and Lazy eval?
  // Promises

  // (lambda (args) (body1) (body2) ...) -- returns (%%closure scope args forms)
  defineGlobalSymbol(LAMBDA_ATOM, function lambda(formalParams, forms) {
    let scope = this;
    let closure = function closure(args) {
      return lispEval(cons(
        cons(LAMBDA_ATOM, cons(formalParams, forms)),
        args), scope);
    };
    closure[LIFT_ARGS] = 0;
    return closure;
  }, { evalArgs: 0, lift: 1 });

  // (lambda (args) (body1) (body2) ...) -- returns (%%closure scope args forms)
  defineGlobalSymbol(SLAMBDA_ATOM, function special_lambda(formalParams, forms) {
    let scope = this;
    let closure = function closure(args) {
      return lispEval(cons(
        cons(SLAMBDA_ATOM, cons(formalParams, forms)),
        args), scope);
    };
    closure[LIFT_ARGS] = 0;
    closure[EVAL_ARGS] = 0;
    return closure;
  }, { evalArgs: 0, lift: 1 });

  //
  // try/catch/filnally. Just a sketch for now.
  //

  defineGlobalSymbol("*throw", e => { throw e });

  defineGlobalSymbol("*finally", function(handler, body) {  // XXX order of args?
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
    } else if (typeof e === 'object' && e[IS_CONS]) {
      cls = e.car;
      if (typeof cls !== 'function')
        cls = lispEval(cls, this);
      if (typeof cls !== 'function')
        throw new EvalError(`Not a class ${cls}`);
      if (typeof e.cdr === 'object' && e.cdr[IS_CONS])
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

  // (define variable value)
  defineGlobalSymbol("define", function(variable, value) {
    let scope = this, name = variable;
    if (typeof variable === 'object' && variable[IS_CONS]) {
      name = variable.car;
      let args = variable.cdr;
      value = list(LAMBDA_ATOM, args, value);
    } else {
      value = lispEval(value, this); // XXX is this right?
    }
    if (typeof name === 'string') name = Atom(name);
    if (typeof name !== 'symbol')
      throw new EvalError(`must define symbol or string ${lispToString(variable)}`);
    GlobalScope.car.set(name, value);  // Or should it be our scope? That would be weird.
    return name;
  }, { evalArgs: 0, lift: 2 });

  //
  // This is where the magic happens
  //

  function lispEval(expr, scope = GlobalScope) {
    if (expr === NIL) return expr;
    if (typeof expr === 'symbol') {
      let val = resolveSymbol(expr, scope);
      if (val === undefined) throw new EvalError(`Undefined symbol ${expr.description}`);
      return val;
    }
    if (typeof expr === 'object' && expr[IS_CONS]) {
      let fn = expr.car, args = expr.cdr;
      if (typeof fn !== 'function' &&
          !(typeof fn === 'object' && fn[IS_CONS] && (fn.car === LAMBDA_ATOM || fn.car === SLAMBDA_ATOM)))
        fn = lispEval(fn, scope);
      return lispApply(fn, args, scope);
    }
    // Experimental special eval for JS arrays and objects:
    //   Values that are evaluable are expanded and placed in
    //   a new Object or Array in correspoding position.
    // XXX Investigate Symbol.species
    if (typeof expr === 'object') {
      if (expr instanceof Array) {
        let res = [];
        for (let item of expr) {
          let val = lispEval(item, scope);
          res.push(val);
        }
        return res;
      } else {
        let res = {};
        for (let [key, value] of Object.entries(expr)) {
          let val = lispEval(value, scope);
          res[key] = val;
        }
        return res;
      }
    }
    return expr;
  }

  function lispApply(fn, args, scope) {
    if (typeof fn === 'function') {
      let evalCount = fn[EVAL_ARGS] ?? MAX_SMALL_INTEGER;
      if (evalCount > 0)
        args = evalArgs(args, scope, evalCount);
      let lift = fn[LIFT_ARGS] ?? MAX_SMALL_INTEGER;
      let jsArgs = [], noPad = lift === MAX_SMALL_INTEGER;
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
        let forms = fn.cdr.cdr;
        if (opSym === LAMBDA_ATOM || opSym === CLOSURE_ATOM)
          args = evalArgs(args, scope);
        let env = new Env;
        scope = new Scope(env, scope);
        let origFormalParams = formalParams;
        while (typeof formalParams === 'object' && formalParams[IS_CONS]) {
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
        let res = NIL;
        while (typeof forms === 'object' && forms[IS_CONS]) {
          res = lispEval(forms.car, scope);
          forms = forms.cdr;
        }
        return res;
      }
    }
    throw new EvalError(`Can't apply ${fn}`);
  }

  function resolveSymbol(sym, scope) {
    while (typeof scope === 'object' && scope[IS_CONS]) {
      let val = scope.car.get(sym);
      if (val !== undefined)
        return val;
      scope = scope.cdr;
    }
    return undefined;
  }

  function evalArgs(args, scope, evalCount) {
    if (args === NIL || evalCount <= 0)
      return args;
    if (!(typeof args === 'object' && args[IS_CONS]))
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

  function lispToString(obj, opts = {}) {
    opts = { ...lispOpts, ...opts };
    let stringWrap = opts.stringWrap ?? 80;
    let wrapChar = opts.wrapChar ?? "\n";
    let maxDepth = opts.maxDepth ?? 100;
    let indentMore = opts.indentMorw ?? "  ";
    let line = "", lines = [], sep = "", prefix = "", indent = "";
    toString(obj);
    if (lines.length === 0) return line;
    if (line)
      lines.push(line);
    return lines.join(wrapChar);
    function put(str) {
      if (line.length + str.length > stringWrap) {
        line += sep;
        lines.push(line);
        line = indent + prefix + str;
      } else {
        line += sep + prefix + str;
      }
      sep = prefix = "";
    }
    function toString(obj, maxdepth) {
      if (maxDepth <= 0) return put("...");
      if (obj === NIL) return put("()");
      if (obj === undefined) return put("undefined");
      if (obj === null) return put( "null");   // remember: typeof null === 'object'!
      let objType = typeof obj;
      let saveIndent = indent;
      // XXX special printing for native functions
      if (objType === 'object') {
        if (obj[IS_CONS]) {
          put("(");
          indent += indentMore;
          sep = "";
          while (typeof obj === 'object' && obj[IS_CONS]) {
            toString(obj.car, maxDepth-1);
            sep = " ";
            obj = obj.cdr;
          }
          if (obj !== NIL) {
            put(".");
            sep = " ";
            toString(obj, maxDepth-1)
          }
          sep = "";
          put(")");
          indent = saveIndent;
          return;
        }
        if (obj instanceof Array) {
          put("[");
          indent += indentMore;
          sep = "";
          for (let item of obj) {
            toString(item, maxDepth-1);
            sep = ", ";
          }
          sep = "";
          put("]");
          indent = saveIndent;
          return;
        }
        {
          // Plain object
          put("{");
          indent += indentMore;
          sep = "";
          for (let name of Object.getOwnPropertyNames(obj)) {
            let item = obj[name];
            prefix = `${name}: `;
            toString(item, maxDepth-1);
            sep = ", ";
          }
          sep = "";
          put("}");
          indent = saveIndent;
          return;
        }
      }
      if (objType === 'symbol')
        return put(obj.description);
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
        return put(str);
      }
      if (objType === 'bigint') {
        return put(`${String(obj)}n`);
      }
      return put(String(obj));
    }
  }

  const TO_LISP = Symbol("*lisp-to-lisp*");

  // Turn iterables objects like arrays into lists
  function iterableToList(obj, opts = {}) { 
    opts = { ...lispOpts, ...opts };
    return toList(obj, opts.depth ?? MAX_SMALL_INTEGER);
    function toList(obj, depth) {
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
          return cons(toList(obj, depth-1), go());
        }
        return go();
      }
      return obj;
    }
  }

  function listToArray(obj, depth = MAX_SMALL_INTEGER) {
    if (depth <= 0) return obj;
    if (obj === NIL) return [];
    if (!(typeof obj === 'object' && obj[IS_CONS])) return obj;
    let arr = [];
    while (typeof obj === 'object' && obj[IS_CONS]) {
      arr.push(listToArray(obj.car), depth-1);
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
    opts = { ...lispOpts, ...opts };
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
      if (token().type === 'atom' || token().type === 'string' ||
          token().type === 'number') {
        let thisToken = consumeToken();
        return thisToken.value;
      }

      if (token().type === '(') {
        let newPrompt = promptStr + promptMore;
        replHints.currentPrompt = newPrompt;
        consumeToken();
        return parseListBody();
        function parseListBody() {
          if (token().type === ')') {
            replHints.currentPrompt = promptStr;
            consumeToken();
            return NIL;
          } else if (token().type === '.') {
            consumeToken();
            let val = parseExpr(newPrompt);
            if (token().type !== ')')
              throw new ParseError(unParesedInput());
            consumeToken();
            return val;
          }
          if (token().type === 'end') throw new ParseError(`Reached end of input`);
          let first = parseExpr(newPrompt);
          let rest = parseListBody();
          return cons(first, rest);
        }
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
          if (token().type === 'end') throw new ParseError(`Reached end of input`);
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
            if (token().type === 'end') throw new ParseError(`Reached end of input`);
          }
          if (!gotIt)
            throw new ParseError(unParesedInput());
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
      throw new ParseError(unParesedInput());
    }

    // I'm old enough to be fond of the EvalQuote REPL.
    // So, as a special case, transform "symbol(a b c)" into "(symbol (quote a) (quote b) (quote c))"
    let expr;
    if (token().type === 'atom' && token(1).type === '(') {
      function quoteArgs(args) {
        if (!(typeof args === 'object' && args[IS_CONS])) return NIL;
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
    throw new ParseError(unparsed);
  }

  function lispREPL(readline, opts = {}) {
    opts = { ...lispOpts, ...opts };
    // readline(prompt) => str | nullish
    let name = opts.name ?? "Jisp";
    let prompt = opts.prompt ?? name + " > ";
    let print = opts.print ?? (x => console.log(name + ":", lispToString(x)));
    let reportLispError = opts.reportLispError ?? (x => console.log(String(x)));;
    let reportSystemError = opts.reportSystemError ?? (x => console.log(name + " internal error:", String(x), x));;
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
          reportSystemError(error);
      }
    }
  }

  // Export functions and symbols here
  this.lispREPL = lispREPL;
  this.Atom = Atom;
  this.NIL = NIL;
  this.cons = cons;
  this.isCons = obj => typeof obj === 'object' && obj[IS_CONS];
  this.car = car;
  this.cdr = cdr;
  this.defineGlobalSymbol = defineGlobalSymbol;
  this.LispError = LispError;
  this.ParseError = ParseError;
  this.EvalError = EvalError;
  this.LogicError = LogicError;
  this.GlobalEnv = GlobalEnv;
  this.eval = lispEval;
  this.toBool = lispToBool;
  this.stringFor = lispToString;
  this.listToArray = listToArray;
  this.arrayToList = iterableToList;
  this.iterableToList = iterableToList;
  this.TO_LISP_SYMBOL = TO_LISP;
} // End of Jisp class/function

let lisp = Jisp();

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
      console.log(`Jisp 1.1 REPL. Type "." to exit.`);
      let buffer = Buffer.alloc(2000);
      function getLine(prompt) {
        process.stdout.write(prompt);
        let read = fs.readSync(inputFd, buffer);
        let line = buffer.slice(0, read).toString();
        while (line.endsWith('\n') || line.endsWith('\r'))
          line = line.substr(0, line.length-1);
        return line;
      }
      lisp.lispREPL(getLine);
    }
  } finally {
    if (closeFd !== undefined)
      fs.closeSync(closeFd);
    if (oldRawMode !== undefined)
      process.stdin.setRawMode(oldRawMode);
  }
}

console.log("done");  // breakpoint spot