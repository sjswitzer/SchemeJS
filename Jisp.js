//
// JavaScript Lisp
//
// Copyright 2021 Stan Switzer
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

"use strict";
// TODO: make this a JS module

//
// Creates a Lisp instance, independent of any others.
// They are distinct to the bones; they do not even recognize each other's
// Cons cells or NIL values. This is by design.
//
function newLisp(lispOpts = {}) {
  let sanityTest = lispOpts.sanityTest;

  const IS_CONS = Symbol("*lisp-isCons*"), CAR = Symbol("*car*"), CDR = Symbol("*cdr*");

  class Cons {
    // Creating a Cons should be as cheap as possible, so no subclassing
    // or calls to super. This means that identifying the "class" of Cons
    // cells can't use "instanceof AbstractCons".
    constructor(car, cdr) {
      [CAR]; [CDR];
      this[CAR] = car;
      this[CDR] = cdr;
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

  //
  // You create a new scope with "new Object(oldScope)".
  // Thus, a scope chain is a prototype chain and resolving
  // a symbol is as simple as "scope[sym]".
  //
  class Scope {
    // Careful defining methods or properties here; they
    // automatically become part of the API.
  };
  const GlobalScope = new Scope();

  //
  // Encapsulating everything in a function scope because JITs
  // can resolve lexically-scoped references most easily.
  // Since "this" is used as the current scope, a Lisp instance
  // is the global scope itself!
  //
 function exportDefinition(name, value, ...aliases) {
   GlobalScope[name] = value;
   for (let alias in aliases)
     GlobalScope[alias] = value;
 }

  class LispError extends Error {};
  LispError.prototype.name = "LispError";
  exportDefinition("LispError", LispError);

  class EvalError extends LispError {};
  EvalError.prototype.name = "EvalError";
  exportDefinition("EvalError", EvalError);

  class CompileError extends LispError {};
  CompileError.prototype.name = "CompileError";
  exportDefinition("CompileError", CompileError);

  class ParseError extends LispError {};
  ParseError.prototype.name = "ParseError";
  exportDefinition("ParseError", ParseError);

  class ParseExtraTokens extends ParseError {};
  ParseExtraTokens.prototype.name = "ParseExtraTokens";
  exportDefinition("ParseExtraTokens", ParseExtraTokens);

  class ParseIncomplete extends ParseError {};
  ParseIncomplete.prototype.name = "ParseIncomplete";
  exportDefinition("ParseIncomplete", ParseIncomplete);

  const LogicError = Error;
  exportDefinition("LogicError", LogicError);

  // Atoms are Symbols
  const ATOMS = {};

  function Atom(name) {
    if (typeof name === 'symbol') {
      // If they pass in an atom, just return it
      if (ATOMS[name.description] !== name)
        throw new LogicError(`Symbol "${name.description}" is not an atom`);
      return name;
    }
    let atom = ATOMS[name];
    if (atom !== undefined) return atom;
    atom = Symbol(name);
    ATOMS[name] = atom;
    return atom;
  }
  exportDefinition("Atom", Atom);

  function AliasAtom(name, ...aliases) {
    let atom = Atom(name);
    for (let alias of aliases)
      ATOMS[alias] = atom;
    return atom;
  }

  const LAMBDA_ATOM = AliasAtom("lambda", "\\", "\u03BB");
  const SLAMBDA_ATOM = AliasAtom("special-lambda", "\\\\", "\u03BB");
  
  function nextCons() {
    let current = this._current, done = current === NIL, value = undefined;
    if (!done) {
      this._current = current[CDR];
      value = current[CAR];
    }
    return { done, value };
  }

  // Just a sketch of lazy evaluation.
  class LazyCarCons {
    // User inplements "get car()" in a subclass and ideally seals the object
    cdr;
    constructor(cdr) {
      this[CDR] = cdr;
    }
    toString() {
      return lispToString(this);
    }
    *[Symbol.iterator]() { return { next: nextCons, _current: this } }
  }
  LazyCarCons.prototype[IS_CONS] = true;

  class LazyCdrCons {
    // User inplements "get cdr()" in a subclass and ideally seals the object
    car;
    constructor(car) {
      this[CAR] = car;
    }
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
  function _bool(val) {
    // Give priority here to actual true and false values
    if (val === true) return true;
    if (val === false || val === NIL || val == null) // The val == null is _intended_ as a nullish test
      return false;
    return true;
  }
  exportDefinition("toBool", _bool);

  const cons = (car, cdr) => new Cons(car, cdr);
  const car = cons => cons[CAR];
  const cdr = cons => cons[CDR];
  // Beware the order here; in JS it's in reversed
  const caaar = cons => cons[CAR][CAR][CAR];
  const caadr = cons => cons[CDR][CAR][CAR];
  const caar = cons => cons[CAR][CAR];
  const cadar = cons => cons[CAR][CDR][CAR];
  const caddr = cons => cons[CDR][CDR][CAR];
  const cadr = cons => cons[CDR][CAR];
  const cdaar = cons => cons[CAR][CAR][CDR];
  const cdadr = cons => cons[CDR][CAR][CDR]
  const cdar = cons => cons[CAR][CDR];
  const cddar = cons => cons[CAR][CDR][CDR];
  const cdddr = cons => cons[CDR][CDR][CDR];
  const cddr = cons => cons[CDR][CDR];
  exportDefinition("NIL", NIL);
  exportDefinition("Car", car);
  exportDefinition("Cdr", cdr);
  exportDefinition("Cons", cons, "cons");
  exportDefinition("IsCons", obj => typeof obj === 'object' && obj[IS_CONS], "isCons");

  function list(...elements) {  // easy list builder
    let val = NIL;
    for (let i = elements.length; i > 0; --i)
      val = cons(elements[i-1], val);
    return val;
  }
  exportDefinition("list", list);

  const EVAL_ARGS = Symbol("*lisp-eval-args*");
  const LIFT_ARGS = Symbol("*lisp-lift-args*");
  const COMPILE_HOOK = Symbol("*lisp-compile-hook*");
  const MAX_SMALL_INTEGER = 2**30-1;  // Presumably allows JIT to do small-int optimizations

  function defineGlobalSymbol(name, val, ...aliases) {
    let opts = {};
    if (typeof aliases[0] === 'object')
      opts = aliases.shift();
    if (typeof val === 'function') {
      // Always set these props because JITs are more likely to optimize found props than unfound ones.
      val[EVAL_ARGS] = opts.evalArgs ?? MAX_SMALL_INTEGER;;
      val[LIFT_ARGS] = opts.lift ?? MAX_SMALL_INTEGER;
      // XXX TODO: make sure that every defn with EVAL_ARGS !== MAX_SMALL_INTEGER has a compile hook.
      if (opts.compileHook) val[COMPILE_HOOK] = opts.compileHook;
    }
    let atom = typeof name === 'symbol' ? name : Atom(String(name));
    GlobalScope[atom] = val;
    for (let alias of aliases)
    GlobalScope[alias] = val;
    return atom;
  }
  exportDefinition("defineGlobalSymbol", defineGlobalSymbol);

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
  defineGlobalSymbol("null?", a => a === NIL);  // XXX scheme clained it first. Maybe rethink the naming here.
  defineGlobalSymbol("nullJS?", a => a === null);
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

  defineGlobalSymbol("eval", eval_);
  function eval_(expr, scope) { // XXX TODO: can this just be "eval"?
    if (scope == null) scope = this;
    else if (rest === NIL) scope = GlobalScope;
    if (typeof expr === 'string')
      expr = parseSExpr(expr);
    return _eval(expr, scope);
  }
  exportDefinition("eval", eval_);

  defineGlobalSymbol("apply", apply);
  function apply(fn, args, scope) {
    if (scope == null) scope = this;
    else if (rest === NIL) scope = GlobalScope;
    return _apply(fn, args, scope);
  }
  // Pokemon gotta catch 'em' all!
  defineGlobalSymbol("!", a => !_bool(a), "not");
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
  defineGlobalSymbol("<", lt, { evalArgs: 1 }, "lt");
  function lt(a, ...rest) {
    if (rest.length === 0) return false; // not less than itself?
    for (let b of rest) {
      b = _eval(b, this);
      if (!(a < b)) return false;
      a = b;
    }
    return true;
  }

  defineGlobalSymbol("<=", le, { evalArgs: 1 }, "le");
  function le(a, ...rest) {
    if (rest.length === 0) return true; // equal to itself?
    for (let b of rest) {
      b = _eval(b, this);
      if (!(a <= b)) return false;
      a = b;
    }
    return true;
  }

  defineGlobalSymbol(">", gt, { evalArgs: 1 }, "gt");
  function gt(a, ...rest) {
    if (rest.length === 0) return false;
    for (let b of rest) {
      b = _eval(b, this);
      if (!(a > b)) return false;
      a = b;
    }
    return true;
  }

  defineGlobalSymbol(">=", ge, { evalArgs: 1 }, "ge");
  function ge(a, ...rest) {
    if (rest.length === 0) return true;
    for (let b of rest) {
      b = _eval(b, this);
      if (!(a >= b)) return false;
      a = b;
    }
    return true;
  }

  defineGlobalSymbol("==", equal, { evalArgs: 1 }, "equal?");
  function equal(a, ...rest) {
    if (rest.length === 0) return true;
    for (let b of rest) {
      b = _eval(b, this);
      if (!(a == b)) return false;
    }
    return true;
  }

  defineGlobalSymbol("===", eq, { evalArgs: 1 }, "eq?");
  function eq(a, ...rest) {
    if (rest.length === 0) return true;
    for (let b of rest) {
      b = _eval(b, this);
      if (!(a === b)) return false;
    }
    return true;
  }

  defineGlobalSymbol("!=", ne, { evalArgs: 1 }, "ne");
  function ne(a, ...rest) {  // all not equal to first
    if (rest.length === 0) return false;
    for (let b of rest) {
      b = _eval(b, this);
      if (!(a != b)) return false;
    }
    return true;
  }

  defineGlobalSymbol("!==", neq, { evalArgs: 1 }, "neq");
  function neq(a, ...rest) {  // all not equal to first
    if (rest.length === 0) return false;
    for (let b of rest) {
      b = _eval(b, this);
      if (!(a !== b)) return false;
    }
    return true;
  }

  defineGlobalSymbol("max", (val, ...rest) => {
    for (let b of args)
      if (b > val)
        val = b;
    return val;
  }, { lift: 1 });

  defineGlobalSymbol("min", (val, ...rest) => {
    for (let b of args)
      if (b < val)
        val = b;
    return val;
  }, { lift: 1 });

 // logical & conditional

  defineGlobalSymbol("&&", and, { evalArgs: 0 }, "and");
  function and(...args) {
    let a = true;
    for (a of args) {
      a = _eval(b, this);
      if (!_bool(a)) return a;
    }
    return a;
  }

  defineGlobalSymbol("||", or, { evalArgs: 0 }, "or");
  function or(...args) {
    let a = false;
    for (a of args) {
      a = _eval(b, this);
      if (_bool(a)) return a;
    }
    return a;
  }

  // XXX TODO: What happens if more that 3 args?
  defineGlobalSymbol("?", ifelse, { evalArgs: 1, lift: 3, compileHook: ifelseHook }, "if");
  function ifelse(p, t, f) { return _bool(p) ? _eval(t, this) : _eval(f, this); }

  // (begin form1 form2 ...)
  defineGlobalSymbol("begin", begin, { evalArgs: 0 });function begin(...forms) {
    let res = NIL;
    for (let form of forms)
      res = _eval(form, this);
    return res;
  }

  // (prog1 form1 form2 form3 ...)
  defineGlobalSymbol("prog1", prog1, { evalArgs: 0 });
  function prog1(...forms) {
    let res = NIL, first = true;
    for (let form of forms) {
      let val = _eval(form, this);
      if (first)
        res = val;
      first = false;
    }
    return res;
  }

  // (cond clause1 clause2 ...)  -- clause is (predicate-expression form1 form2 ...)
  defineGlobalSymbol("cond", cond, { evalArgs: 0 });
  function cond(...clauses) {
    for (let clause of clauses) {
      if (!(typeof clause === 'object' && clause[IS_CONS]))
        throw new EvalError(`Bad clause in "cond" ${lispToString(clause)}`);
      let pe = clause[CAR], forms = clause[CDR];
      let evaled = _eval(pe, this);
      if (_bool(evaled)) {
        let res = NIL;
        while (typeof forms === 'object' && forms[IS_CONS]) {
          res = _eval(forms[CAR], this);
          forms = forms[CDR];
        }
        return res;
      }
    }
    return NIL;
  }

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

  defineGlobalSymbol("require", _require);
  function _require(path, force) {
    let sym = Atom(`*${path}-loaded*`);
    if (_bool(force) || !_bool(GlobalScope[sym])) {
      try {
        // todo: For now, nodejs-specific
        const fs = require('fs');
        let fileContent = fs.readFileSync(path);
        fileContent = fileContent.toString();
        // The REPL can handle this!
        this.REPL(function readline() {
          let line = fileContent;
          fileContent = null;
          return line;
        });
        GlobalScope[sym] = true;
      } catch (error) {
        console.log(`"require" failed`, String(error));  // todo: abstract reporting
        return false;
      }
    }
    return sym;
  }
  exportDefinition("require", _require);

  defineGlobalSymbol("append", (...args) => {
    let res = NIL;
    function zip(list) {
      if (list === NIL) return;
      if (typeof list === 'object' && list[IS_CONS]) {
        zip(list[CDR]);
        res = cons(list[CAR], res);
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
      let next = list[CDR];
      if (next === NIL)
        return list[CAR];
      list = next;
    }
    return list;  // list was empty or not terminated with NIL
  });

  defineGlobalSymbol("length", (list) => {
    let n = 0;
    while (typeof list === 'object' && list[IS_CONS]) {
      n += 1;
      list = list[CDR];
    }
    return n;
  });

  defineGlobalSymbol("list", (...args) => {
    let res = NIL;
    for (let i = args.length; i > 0; --i)
      res = cons(args[i-1], res);
    return res;
  });

  defineGlobalSymbol("reverse", reverseList);
  function reverseList(list) {
    let res = NIL;
    while (typeof list === 'object' && list[IS_CONS]) {
      res = cons(list[CAR], res)
      list = list[CDR];
    }
    return res;
  }

  defineGlobalSymbol("butlast", (list) => {
    let res = NIL;
    if (!(typeof list === 'object' && list[IS_CONS])) return NIL;
    function zip(list) {
      let next = list[CDR];
      if (typeof next === 'object' && next[IS_CONS]) {
        zip(list[CDR]);
        res = cons(list[CAR], res);
      }
    }
    zip(list);
    return res;
  });

  // (member key list)
  //     Returns the portion of the list where the car is equal to the key, or () if none found.
  defineGlobalSymbol("member", member);
  function member(key, list) {
    while (typeof list === 'object' && list[IS_CONS]) {
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
    while (typeof list === 'object' && list[IS_CONS]) {
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
    while (index > 0 && typeof list === 'object' && list[IS_CONS]) {
      index -= 1;
      list = list[CDR];
    }
    if (typeof list === 'object' && list[IS_CONS])
      return list[CAR];
    return NIL;
  }


  // (apropos substring) -- Returns a list of all symbols containing the given substring
  defineGlobalSymbol("apropos", apropos);
  function apropos(substring) {
    if (!substring) substring = "";
    substring = substring.toLowerCase();
    let matches = [], scope = this;
    while (scope && scope !== Object) {
      let symbols = Object.getOwnPropertySymbols(scope);
      for (let symbol of symbols) {
        let name = lispToString(symbol);
        if (name.toLowerCase().includes(substring))
          matches.push(symbol);
      }
      if (scope === GlobalScope) break;
      scope = Object.getPrototypeOf(scope);
    }
    matches.sort((a,b) => a.description < b.description ? -1 : a.description > b.description ? 1 : 0);
    return iterableToList(matches);
  }

  // (mapcar fn list1 list2 ...)
  defineGlobalSymbol("mapcar", mapcar);
  function mapcar(fn, lists) {
    // Actually, this will work for any iterables, and lists are iterable.
    // It's possible to write this recursively to create the list in order, but that
    // uses unbounded stack and is slower than just constructing the list backwards then
    // reversing it.
    let rev = NIL;
    for (let list of lists)
      for (let item of list)
        rev = cons(fn.call(this, item), rev);
    return reverseList(rev);
  }

  // Same as mapcar but results in an Array
  defineGlobalSymbol("map->arrqay", map_to_array);
  function map_to_array(fn, lists) {
    let res = [];
    let rev = NIL;
    for (let list of lists)
      for (let item of list)
        res.push(fn.call(this, item));
    return res;
  }

  // (let (binding1 binding2 ...) form1 form2 ...) -- let* behavior
  //     (let ((x 10)
  //      (y 20))
  //      (+ x y))
  // Because of this implementation uses a scope chain instead
  // of an environment, each kind of let is as piwerful as "letrec".
  //
  // TODO: Make a shape-shifting Scope object that mimics a Cons
  // and transforms into an environment on demand, conversely
  // functions that take an "env" param should transform it back into
  // a Scope. That will maintain Scheme API compatibility while
  // still benefiting from Scopes internally.
  defineGlobalSymbol("letrec", letrec, { evalArgs: 0, lift: 1 }, "let", "let*");
  function letrec(bindings, forms) {
    let scope = new Object(this);
    while (typeof bindings === 'object' && bindings[IS_CONS]) {
      let binding = bindings[CAR];
      if (!(typeof binding === 'object' && binding[IS_CONS]))
        throw new EvalError(`Bad binding ${lispToString(binding)}`);
      let boundVar = binding[CAR], bindingForms = binding[CDR];
      if (typeof boundVar !== 'symbol')
        throw new EvalError(`Bad binding ${lispToString(binding)}`);
      let val = NIL;
      while (typeof bindingForms === 'object' && bindingForms[IS_CONS]) {
        val = _eval(bindingForms[CAR], scope);
        bindingForms = bindingForms[CDR];
      }
      scope[boundVar] = val;
      bindings = bindings[CDR];
    }
    let res = NIL;
    while (typeof forms === 'object' && forms[IS_CONS]) {
      res = _eval(forms[CAR], scope);
      forms = forms[CDR];
    }
    return res;
  }

  // SIOD compatibility checklist:
  //
  // TODO benchmark fns -- http://people.delphiforums.com/gjc//siod.html#builtin
  // (realtime)
  //      Returns a double precision floating point value representation of the current realtime number of seconds. Usually precise to about a thousandth of a second.
  // errobj, (error message object)
  // (load fname noeval-flag search-flag)
  //   If the neval-flag is true then a list of the forms is returned otherwise the forms are evaluated.
  //   no use for the search-flag
  // (number->string x base width precision)
  //     Formats the number according to the base, which may be 8, 10, 16 or the symbol e or f.
  //     The width and precision are both optional.
  // (parse-number str)
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

  // (lambda (args) (body1) (body2) ...)
  defineGlobalSymbol(LAMBDA_ATOM, lambda, { evalArgs: 0, lift: 0 });
  function lambda(form) {
    let scope = this;
    let closure = args => _apply(cons(LAMBDA_ATOM, form), args, scope);
    closure[LIFT_ARGS] = 0;
    return closure;
  }

  // (lambda (args) (body1) (body2) ...)
  defineGlobalSymbol(SLAMBDA_ATOM, special_lambda, { evalArgs: 0, lift: 0 });
  function special_lambda(form) {
    let scope = this;
    let closure = args => _apply(cons(SLAMBDA_ATOM, form), args, scope);
    closure[LIFT_ARGS] = 0;
    closure[EVAL_ARGS] = 0;
    return closure;
  }

  //
  // try/catch/filnally. Just a sketch for now.
  //
  class LispThrow extends LispError {
    constructor(tag, value, msg) {
      value;
      super(msg);
      this.tag = tag;
      this.value = value;
    }
    toString() {
      return `${super.toString()} ${this.tag} ${lispToString(this.value)}`;
    }
  };
  LispThrow.prototype.name = "LispThrow";

  // (*throw tag value) -- SIOD style
  defineGlobalSymbol("*throw", (tag, value) => { throw new LispThrow(tag, value)});

  // (*catch tag form ...) -- SIOD style
  defineGlobalSymbol("*catch", lispCatch, { evalArgs: 1, lift: 1 });
  function lispCatch(tag, forms) {  // XXX order of args?
    let val = NIL;
    try {
      while (typeof forms === 'object' && forms[IS_CONS]) {
        val = _eval(forms[CAR], this);
        forms = forms[CDR];
      }
    } catch (e) {
      if (!(e instanceof LispThrow)) throw e;  // rethrow
      if (e.tag !== tag) throw e;
      val = e.value;
    }
    return val;
  }

  // (throw value) -- Java/JavaScript style
  defineGlobalSymbol("throw", value => { throw value});

  // (catch (var [type] forms) forms) -- Java/JavaScript style
  defineGlobalSymbol("catch", lispJSCatch, { evalArgs: 0, lift: 1 });
  function lispJSCatch(catchClause, forms) {
    if (!(typeof catchClause === 'object' && catchClause[IS_CONS]))
      throw new EvalError(`Bad catch clause ${lispToString(catchClause)}`);
    let catchVar = catchClause[CAR], catchForms = catchClause[CDR];
    if (!(typeof catchForms === 'object' && catchForms[IS_CONS]))
      throw new EvalError(`Bad catch clause ${lispToString(catchClause)}`);
    var typeMatch;
    if (typeof catchForms[CAR] === 'string' || typeof catchForms[CAR] === 'function') {
      typeMatch = catchForms[CAR];
      catchForms = catchForms[CDR];
    }
    if (!(typeof catchForms === 'object' && catchForms[IS_CONS]))
      throw new EvalError(`Bad catch clause ${lispToString(catchClause)}`);
    let val = NIL;
    try {
      while (typeof forms === 'object' && forms[IS_CONS]) {
        val = _eval(forms[CAR], this);
        forms = forms[CDR];
      }
    } catch (e) {
      if (!typeMatch || (typeof typeMatch === 'string' && typeof e === typeMatch)
          || e instanceof typeMatch) {
        let scope = new Object(this);
        scope[catchVar] = e;
        while (typeof catchForms === 'object' && catchForms[IS_CONS]) {
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
  defineGlobalSymbol("define", define, { evalArgs: 0, lift: 2 });
  function define(variable, value) {
    let scope = this, name = variable;
    if (typeof variable === 'object' && variable[IS_CONS]) {
      name = variable[CAR];
      let args = variable[CDR];
      value = list(LAMBDA_ATOM, args, value);
    } else {
      value = _eval(value, this);
    }
    if (typeof name === 'string') name = Atom(name);
    if (typeof name !== 'symbol')
      throw new EvalError(`must define symbol or string ${lispToString(variable)}`);
    this[name] = value;  // Or maybe GlobalScope?
    return name;
  }

  //
  // This is where the magic happens
  //

  function _eval(expr, scope) {
    if (expr === NIL) return expr;
    if (typeof expr === 'symbol') {
      let val = scope[expr];
      if (val === undefined) throw new EvalError(`Undefined symbol ${expr.description}`);
      return val;
    }
    if (typeof expr === 'object' && expr[IS_CONS]) {
      let fn = expr[CAR], args = expr[CDR];
      if (typeof fn !== 'function' &&
          !(typeof fn === 'object' && fn[IS_CONS] && (fn[CAR] === LAMBDA_ATOM || fn[CAR] === SLAMBDA_ATOM)))
        fn = _eval(fn, scope);
      return _apply(fn, args, scope);
    }
    // Experimental special eval for JS arrays and objects:
    //   Values that are evaluable are expanded and placed in
    //   a new Object or Array in correspoding position.
    // XXX Investigate Symbol.species (also for mapcar, etc.)
    if (typeof expr === 'object') {
      if (expr instanceof Array) {
        let res = [];
        for (let item of expr) {
          let val = _eval(item, scope);
          res.push(val);
        }
        return res;
      } else {
        let res = {};
        for (let [key, value] of Object.entries(expr)) {
          let val = _eval(value, scope);
          res[key] = val;
        }
        return res;
      }
    }
    return expr;
  }

  function _apply(fn, args, scope) {
    if (typeof fn === 'function') {
      let evalCount = fn[EVAL_ARGS];
      if (evalCount > 0)
        args = evalArgs(args, scope, evalCount);
      let lift = fn[LIFT_ARGS];
      let jsArgs = [], noPad = lift === MAX_SMALL_INTEGER;
      while (lift > 0) {
        // Promote "lift" arguments to JS arguments, filling with NIL
        if (typeof args === 'object' && args[IS_CONS]) {
          jsArgs.push(args[CAR]);
          args = args[CDR];
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
      return fn.apply(scope, jsArgs);  // ??? scope[fn](...jsArgs);
    }
    if (typeof fn === 'object' && fn[IS_CONS]) {
      let opSym = fn[CAR];
      if (opSym === LAMBDA_ATOM || opSym === SLAMBDA_ATOM) {
        let params = fn[CDR][CAR];
        let forms = fn[CDR][CDR];
        if (opSym === LAMBDA_ATOM || opSym === CLOSURE_ATOM)
          args = evalArgs(args, scope);
        scope = new Object(scope);
        let origFormalParams = params;
        while (typeof params === 'object' && params[IS_CONS]) {
          let param = params[CAR];
          if (typeof param !== 'symbol') throw new EvalError(`Param must be a symbol ${param}`);
          if (args !== NIL) {
            scope[param] = args[CAR];
            if (typeof args === 'object' && args[IS_CONS]) args = args[CDR];
          } else {
            scope[param] = NIL;
          }
          params = params[CDR];
        }
        if (typeof params === 'symbol')  // Neat trick for 'rest' params!
          scope[params] = args;
        else if (params !== NIL)
          throw new EvalError(`Bad parameter list ${lispToString(origFormalParams)}`);
        let res = NIL;
        while (typeof forms === 'object' && forms[IS_CONS]) {
          res = _eval(forms[CAR], scope);
          forms = forms[CDR];
        }
        return res;
      }
    }
    throw new EvalError(`Can't apply ${fn}`);
  }
  exportDefinition("apply", _apply);

  function evalArgs(args, scope, evalCount = MAX_SMALL_INTEGER) {
    if (evalCount <= 0 || args === NIL) return args;
    let argv = [];
    let reverse = NIL;
    while (evalCount > 0 && typeof args === 'object' && args[IS_CONS]) {
      argv.push(_eval(args[CAR], scope));
      args = args[CDR];
      evalCount -= 1;
    }
    for (let i = argv.length; i > 0; --i)
      args = cons(argv[i-1], args);
    return args;
  }

  const ESCAPE_STRINGS = { t: '\t', n: '\n', r: '\r', '"': '"', '\\': '\\' }
  const STRING_ESCAPES = (() => {
    let res = {};
    for (let [key, value] of Object.entries(ESCAPE_STRINGS))
      res[value] = key;
    return res;
  })();

  // Implements "toString()" for lists.
  // We can't just implement toString() because it needs to work for
  // non-Object types too.
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
            toString(obj[CAR], maxDepth-1);
            sep = " ";
            obj = obj[CDR];
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
  exportDefinition("stringFor", lispToString);

  const TO_LISP_SYMBOL = Symbol("*lisp-to-lisp*");

  // Recursively turns iterable objects like arrays into lists
  function iterableToList(obj, depth) {
    if (depth <= 0)return obj;
    let iterator;
    if (typeof obj === 'function') // assume it's an iterator
      iterator = obj;
    if (typeof obj === 'object') {
      if (obj[IS_CONS]) return obj;  // Careful; cons is iterable itself
      if (obj[TO_LISP_SYMBOL])  // User can specialize this
        return obj[TO_LISP_SYMBOL](opts);
      let iterator;
      if (obj[Symbol.iterator])
        iterator = obj[Symbol.iterator]();
    }
    if (iterator) {
      function go() {
        let { done, value } = iterator.next();
        if (done) return NIL;
        return cons(iterableToList(obj, depth-1), go());
      }
      return go();
    }
    return obj;
  }
  exportDefinition("iterableToList", iterableToList);
  exportDefinition("arrayToList", iterableToList);
  exportDefinition("TO_LISP_SYMBOL", TO_LISP_SYMBOL);

  // Recursively turn lists into Arrays
  function listToArray(obj, depth = MAX_SMALL_INTEGER) {
    if (depth <= 0) return obj;
    if (obj === NIL) return [];
    if (!(typeof obj === 'object' && obj[IS_CONS])) return obj;
    let arr = [];
    while (typeof obj === 'object' && obj[IS_CONS]) {
      arr.push(listToArray(obj[CAR]), depth-1);
      obj = obj[CDR];
    }
    return arr;
  }
  exportDefinition("listToArray", listToArray);

  //
  // S-epression parser
  //
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
              throw new ParseExtraTokens(unParesedInput());
            consumeToken();
            return val;
          }
          if (token().type === 'end') throw new ParseIncomplete();
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
          if (token().type === 'end') throw new ParseIncomplete();
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
            if (token().type === 'end') throw new ParseIncomplete();
          }
          if (!gotIt)
            throw new ParseExtraTokens(unParesedInput());
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
      throw new ParseExtraTokens(unParesedInput());
    }

    // I'm old enough to be fond of the EvalQuote REPL.
    // So, as a special case, transform "symbol(a b c)" into "(symbol (quote a) (quote b) (quote c))"
    let expr;
    if (token().type === 'atom' && token(1).type === '(') {
      function quoteArgs(args) {
        if (!(typeof args === 'object' && args[IS_CONS])) return NIL;
        let quoted = args[CAR];
        quoted = cons(QUOTE_ATOM, cons(quoted, NIL))
        return cons(quoted, quoteArgs(args[CDR]));
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
    throw new ParseExtraTokens(unparsed);
  }

  function compile(expr) {
    let scope = GlobalScope;
    let [args, body] = compileLambda(expr, scope);
    let res = new Function(...[ ... args, body ]);
    if (expr[CAR] === SLAMBDA_ATOM)
      res[EVAL_ARGS] = 0;
    return res;
  }

  function analyzeJSFunction(fn, result, args) {
    // Super janky passer. It only has to recoginze things that fn.toSting() can return.
    // But it's conservative in that it doesn't recognize any functions that
    // don't end in a particular sequence of tokens or that use "return" more than once.
    let str = fn.toString(), pos = 0, token = "";
    let functionName, params = [], restParam, resultVal, body;
    if (sanityTest) console.log('TEST analyzeJSFunction', str, result, args);
    nextToken();
    if (token === 'function') {
      nextToken();
      if (token !== '(') {
        functionName = token;
        nextToken();
      }
      if (token !== '(')
        return {};
      nextToken();
      for (;;) {
        if (token === "...") {
          nextToken();
          restParam = token;
          nextToken()
        } else {
          params.push(token);
          nextToken();
        }
        if (token === ')') {
          nextToken();
          break;
        }
        if (token !== ",")
          return {};
        nextToken();
      }
      if (token === '{') {
        while (WSNL[str[pos]]) ++pos;
        let bodyPos = pos, returnPos = bodyPos;
        while (token && token !== 'return') {
          returnPos = pos;
          nextToken();
        }
        if (token === "return") {
          // If there's a return, it has to be followed by a variable,
          // an optional semicolon, an "}", and NOTHING FURTHER.
          // We capture the name of the return variable and clip the body
          // prior to the return.
          nextToken();
          resultVal = token;
          nextToken();
          while (token === ';') nextToken();
          if (token !== '}')
            return {};
          nextToken();
          if (token !== "")
            return {};
          body = str.substring(bodyPos, returnPos);
        }
      }
    } else {
      if (token === '(') {
        nextToken();
        for (;;) {
          if (token === "...") {
            nextToken();
            restParam = token;
            nextToken();
          } else {
            params.push(token);
            nextToken();
          }
          if (token === ')') {
            nextToken();
            break;
          }
          if (token !== ",")
            return {};
          nextToken();
        }
      } else {
        params.push(token);
        nextToken();
      }
      if (!(token === '=>'))
        return {};
      while (WSNL[str[pos]]) ++pos;
      resultVal = str.substr(pos);
    }
    if (!resultVal)
      return {};

    /*
      let evalCount = fn[EVAL_ARGS] ?? MAX_SMALL_INTEGER;
      if (evalCount > 0)
        args = evalArgs(args, scope, evalCount);
      let lift = fn[LIFT_ARGS] ?? MAX_SMALL_INTEGER;
      let jsArgs = [], noPad = lift === MAX_SMALL_INTEGER;
      while (lift > 0) {
        // Promote "lift" arguments to JS arguments, filling with NIL
        if (typeof args === 'object' && args[IS_CONS]) {
          jsArgs.push(args[CAR]);
          args = args[CDR];
        } else {
          // Don't let cons, etc, be seeing any undefined parmaters
          if (noPad) // but not infinitely many of them!
            break;
          jsArgs.push(NIL);
        }
        --lift;
      }
      if (args !== NIL)  // "rest" arg; however NIL shows up as "undefined" in this one case
        jsArgs.push(args);
      return fn.apply(scope, jsArgs);  // "this" is the scope!
    */
    if (sanityTest) console.log('TEST analyzeJSFunction (analyzed)', params, restParam, functionName, resultVal, body);
    let lift = fn[LIFT_ARGS] ?? MAX_SMALL_INTEGER, noPad = lift === MAX_SMALL_INTEGER;
    let decompiled = `let ${result}; {${functionName ? "// " + functionName : ""}\n`;
    let paramsCopy = [...params], argsCopy = [...args];
    // See corresponding code in _apply!
    while (lift > 0) {
      if (paramsCopy.length > 0) {
        let param = paramsCopy.shift();
        let arg = argsCopy.shift();
        decompiled += `let ${param} = ${arg};\n`
      } else {
        if (noPad) break;
        decompiled += `let ${param} = NIL;\n`
      }
      lift -= 1;
    }
    if (restParam) {
      // Q: Are "cons" and "NIL" in scope?
      // A: No, not inherently, but the top-level function will hoist them from
      // the scope ("this") object. So "no" and "yes."
      decompiled += `let ${restParam} = `;
      if (args.length > 0) {
        let lng = args.length;
        for (let i = 0; i < lng; ++i)
          decompiled += `cons(${args[i]}, `;
        decompiled += `NIL`;
        for (let i = 0; i < lng; ++i)
          decompiled += `)`;
        decompiled += `;\n`;
      } else {
        decompiled += `let ${restParam} = undefined;\n`;
      }
    }
      
    if (body)
      decompiled += `${body}\n`;
    decompiled += `${result} = ${resultVal};\n}\n`
    if (sanityTest) console.log("TEST analyzeJSFunction (decompiled)", decompiled);
    return { result, name: functionName, params, restParam, body, decompiled, fn };

    function nextToken() {
      // Super janky tokenizer.
      // Most of what it returns is garbage, but it returns anything we actually care about.
      // The assumption is that what JS returns is well-formed, so it takes a lot of liberties.
      if (pos >= str.length) return token = "";
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
        return token = 'quoted-stuff';
      }
      return token = str[pos++];
    }
  }
 
  if (sanityTest) {
    let resultTmp = "tmp$15", args = ["tmp$1", "tmp$4", "tmp$3", "tmp$7"];
    analyzeJSFunction(x => x * x, resultTmp, args);
    analyzeJSFunction((x) => x * x, resultTmp, args);
    analyzeJSFunction((x, y) => x * y, resultTmp, args);
    analyzeJSFunction((x, ...y) => x * y, resultTmp, args);
    analyzeJSFunction((x, y, ...z) => x * y, resultTmp, args);
    analyzeJSFunction((...x) => x * x, resultTmp, args);
    analyzeJSFunction(function (a) { a = 2 * a; return a; }, resultTmp, args);
    analyzeJSFunction(function (a, b, c) { a = 2 * a; return a; }, resultTmp, args);
    analyzeJSFunction(function someFunction(a) { a = 2 * a; return a; }, resultTmp, args);
    analyzeJSFunction(function someFunction(a, b, c) { a = 2 * a; return a; }, resultTmp, args);
    analyzeJSFunction(function (a, ...rest) { return a; }, resultTmp, args);
    analyzeJSFunction(function (a, b, c, ...rest) { return a; }, resultTmp, args);
    analyzeJSFunction(function someFunction(a, ...rest) { return a; }, resultTmp, args);
    analyzeJSFunction(function someFunction(a, ...rest) { return a; }, resultTmp, ["tmp$1"]);
    analyzeJSFunction(function someFunction(a, b, c, ...rest) { return a; }, resultTmp, args);
    analyzeJSFunction(function someFunction(...rest) { return a; }, resultTmp, args);
    analyzeJSFunction(function (...rest) { return a; }, resultTmp, args);
  }

  function compileLambda(expr, contaningScope) {
    let args, body, _tvar = 0;
    function newTvar() { return `t$(_tvar++})` }
    function emit(fn, args) { // => tvar
      var tvar = newTvar();
      var fstr = String(fn);
      let evalCount = fn[EVAL_ARGS] ?? MAX_SMALL_INTEGER;
      let lift = fn[LIFT_ARGS] ?? MAX_SMALL_INTEGER;
      // var $tvar; {
      //   let p1 = a1, p2 = a2, p3 = a3 (maybe array);
      //   tvar = ...;
    
      body += `var ${tvar}; {\n`;
      // p1 => ...
      // (p1, p2, [...]p3) => ...

      body += `}`;
      return tvar;
    }
    if (typeof expr === 'object' && expr[IS_CONS]) {
      let fn = expr[CAR];
      if (fn === LAMBDA_ATOM || fn === SLAMBDA_ATOM) {
        let paramsAndForms = expr[CDR];
        if (typeof paramsAndForms === 'object' && paramsAndForms[IS_CONS]) {
          let params = paramsAndForms[CAR], forms = paramsAndForms[CDR];
          if (params === NIL || (typeof params === 'object' && params[IS_CONS])) {
            if (forms === NIL || (typeof forms === 'object' && forms[IS_CONS])) {
              // We've got something to work with here
              let scope = new Object(contaningScope);
              while (typeof params === 'object' && params[IS_CONS]) {
                let param = params[CAR], jsVar = toJSvariable(param);
                scope[param] = jsVar;
                args.push(jsVar);
                param = param[CDR];
              }
              let res;
              while (typeof forms === 'object' && forms[IS_CONS]) {
                res = compileForm(forms[CAR], scope);
                forms = forms[CDR];
              }
              return null; // XXX something!
            }
          }
        }
      }
    }
    throw new CompileError(`Not a lambda ${lispToString(expr)}`)
  }
  
  const JS_IDENT_REPLACEMENTS  = {
    '~': '$tilde', '!': '$bang', '@': '$at', '#': '$hash', '$': '$cash', '%': '$pct', '^': '$hat',
    '&': '$and', '|': '$or', '*': '$star', '+': '$plus', '-': '$minus', '=': '$eq', '<': '$lt',
    '>': '$gt', '/': '$stroke', '\\': '$bs', '?': '$wut'
  };

  function toJSvariable(name) {
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
    if (sanityTest) console.log("TEST toJSvariable", name, newName);
    return newName;
  }

  if (sanityTest) {
    toJSvariable("aNormal_name0234");
    toJSvariable("aname%with&/specialChars?");
    toJSvariable("_begins_with_underscore");
    toJSvariable("number?");
    toJSvariable("&&");
    toJSvariable("?");
  }

  function ifelseHook(args, scope, newTemp) {
    if (args.length < 3) return {}; // XXX what happens when more than 3?
    let val = newTemp(), emit = "";
    let { val: p, emit: emitted } = compileEval(args[0], scope, newTemp);
    if (!p) return {};
    emit += emitted;
    emit += `let ${val};\nif (${p} === true || !(${p} === false || ${p} === NIL || ${p} == null)) {\n`;
    let { val: tval, emit: t_emitted } = compileEval(args[1], scope, newTemp);
    if (!tval) return {};
    emit += t_emitted + `${val} = ${tval};\n} else {\n`;
    let { val: fval, emit: f_emitted } = compileEval(args[2], scope, newTemp);
    if (!fval) return {};
    emit += f_emitted + `${val} = ${fval};\n}\n`;
    return { val, emit };
  }

  function leHook(args, scope, newTemp) {
    let val = newTemp(), emit = "";
    let { val: val0, emit: emit0 } = compileEval(args[0], scope, newTemp);
    if (!val0) return {};
    emit += emit0;
    if (args.length < 2)
      return { val: "true", emit };  // eval first arg but return true regardless (unless exception)
    let emitAfter = `let ${val} = true &&`;
    for (let i = 1; i < args.length; ++i) {
      let { val: val_n, emit: emit_n } = compileEval(args[1], scope, newTemp);
      if (!val_n) return {};
      emit += emit_n;
      emitAfter += `&& ${val0} <= ${val_n}`;
      val0 = val_n;
    }
    emitAfter += ';\n';
    emit += emitAfter;
    return { val, emit };
  }

  function REPL(readline, opts = {}) {
    let scope = this;
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
        let evaluated = _eval(expr, scope);
        print (evaluated);
      } catch (error) {
        if (error instanceof LispError)
          reportLispError(error);
        else
          reportSystemError(error);
      }
    }
  }
  exportDefinition("REPL", REPL);

  return GlobalScope;
} // Whew!

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
      newLisp({ sanityTest: true });  // XXX silly debugging hack
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
      let lisp = newLisp();
      // getLine("Attach debugger and hit return! ");  // uncomment to do what it says
      lisp.eval('(define (test) (require "test.scm" true))');
      lisp.REPL(getLine);
    }
  } finally {
    if (closeFd !== undefined)
      fs.closeSync(closeFd);
    if (oldRawMode !== undefined)
      process.stdin.setRawMode(oldRawMode);
  }
}

console.log("done");  // breakpoint spot