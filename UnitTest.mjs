//
// SchemeJS Unit Tests
//
// Copyright 2021 Stan Switzer -- (sjswitzer [at] gmail [dot] com)
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

import * as SchemeJS from './SchemeJS.mjs';

export let succeeded = 0, failed = 0;

let string; // So that TestFailure Error can access it

export class TestFailureError extends Error {
  constructor(message, test, result, expected, report) {
    super(`${string(test)}; ${message}: ${string(result)}, expected: ${string(expected)}`);
    this.test = test;
    this.result = result;
    this.expected = expected;
    this.report = report;
  }
}
TestFailureError.prototype.name = "TestFailureError";

export function run(opts = {}) {
  let throwOnError = opts.throwOnError ?? true;
  let reportTestFailed = opts.reportTestFailed ?? ((message, test, result, expected, report) =>
      console.error("FAILED", message, test, result, expected, report));
  let reportTestSucceeded = opts.reportTestSucceeded ?? ((test, result, expected) =>
      console.info("SUCCEEDED", test, result, expected));

  let globalScope = SchemeJS.createInstance(opts);
  let testScope = globalScope;
  const setGlobalScope = globalScope._setGlobalScope_test_hook_ ?? required();
  const NIL = globalScope.NIL ?? required();
  string = globalScope.string ?? required();
  const newScope = globalScope.newScope ?? required();
  const equal = globalScope.equal ?? required();
  const isList = globalScope.isList ?? required();
  const isClosure = globalScope.isClosure ?? required();
  const SchemeEvalError = globalScope.SchemeEvalError ?? required();
  const SchemeCompileError = globalScope.SchemeCompileError ?? required();
  const parseSExpr = globalScope.parseSExpr ?? required();
  const list = globalScope.list ?? required();
  const Atom = globalScope.Atom ?? required();
  const compile_lambda = globalScope.compile_lambda ?? required();
  function required() { throw "required" }
  const justTestJIT = opts.justTestJIT;

  const isCompileOrEvalError = e => (e instanceof SchemeCompileError) || (e instanceof SchemeEvalError);
  const evalString = str => testScope.eval_string(str);
  let evalTestString = evalString;

  if (justTestJIT) {
    testSuite();
    return { succeeded, failed };
  }

  // Test Internals
  internalsSuite();

  // Test Interpreter
  testSuite();

  // Test Compiler
  evalTestString = compileThenEvalString;
  testSuite();

  // This is kinda hacky, but the only way to change the JIT params is instantiate a new Scheme instance.
  // Calling run() recursively, with new parameters, accomplishes that. But it should probably be reworked.

  let jitThreshold = 0;  // Means: JIT immediately (zero iterations of the interpreter)
  run({ justTestJIT: true, jitThreshold });

  if (testScope !== globalScope) throw new Error("Unpaired begin/endTestScope() calls");
  console.info("UNIT TESTS COMPLETE", "Succeeded:", succeeded, "Failed:", failed);
  return { succeeded, failed };

  function testSuite() {
    //
    // Builtins tests
    // Get the fundamentals sorted before testing any scenarios!
    // Also useful because you can set a breakpoint in a builtin and not have
    // to skip a lot of tests that use it incidentally.
    //

    EXPECT(` '(a .7) `, ` '(a 0.7) `)  // Make sure the dot notation doesn't get tripped up with numbers
    EXPECT(` '(. a) `, ` 'a `);
    EXPECT(` (cons 1 2) `, ` '(1 . 2) `);
    EXPECT(` (car '(1 . 2)) `, ` 1 `);
    EXPECT(` (cdr '(1 . 2)) `, 2);
    EXPECT(` (car '(1 2 3)) `, ` '1 `);
    EXPECT(` (car '[1 2 3]) `, ` '1 `);
    EXPECT(` (cdr '(1 2 3)) `, ` '(2 3) `);
    EXPECT(` (cdr '[1 2 3]) `, ` '(2 3) `);
    EXPECT_ERROR( ` (car nil) `, SchemeEvalError );
    EXPECT_ERROR( ` (cdr nil) `, SchemeEvalError );
    const testList = ` '(((aaa.daa).(ada.dda)).((aad.dad).(add.ddd))) `;
    EXPECT(` (caaar ${testList}) `, ` 'aaa `);
    EXPECT(` (cdaar ${testList}) `, ` 'daa `);
    EXPECT(` (cadar ${testList}) `, ` 'ada `);
    EXPECT(` (cddar ${testList}) `, ` 'dda `);
    EXPECT(` (caadr ${testList}) `, ` 'aad `);
    EXPECT(` (cdadr ${testList}) `, ` 'dad `);
    EXPECT(` (caddr ${testList}) `, ` 'add `);
    EXPECT(` (cdddr ${testList}) `, ` 'ddd `);
    EXPECT(` (caar ${testList}) `, ` '(aaa.daa) `);
    EXPECT(` (cdar ${testList}) `, ` '(ada.dda) `);
    EXPECT(` (cadr ${testList}) `, ` '(aad.dad) `);
    EXPECT(` (cddr ${testList}) `, ` '(add.ddd) `);

    EXPECT(` (sqrt 2) `, Math.sqrt(2));
    EXPECT(` NaN `, isNaN);
    EXPECT(` (NaN? 1) `, false);
    EXPECT(` (nan? NaN) `, true);
    EXPECT(` Infinity `, Infinity);
    EXPECT(` (- Infinity) `, -Infinity);
    EXPECT(` (finite? NaN) `, false);
    EXPECT(` (finite? (/ 3 4)) `, true);
    EXPECT(` (finite? (/ 3 0)) `, false);
    EXPECT(` (finite? Infinity) `, false);
    EXPECT(` (finite? NaN) `, false);
    EXPECT(` (finite? "string") `, false);
    EXPECT(` (abs 3) `, 3);
    EXPECT(` (abs -3) `, 3);
    EXPECT(` (abs 3n) `, 3n);
    EXPECT(` (abs -3n) `, 3n);

    EXPECT(` (intern "abc") `, ` 'abc `);
    EXPECT(` (Symbol  "a") `, x => typeof x === 'symbol' && x.description === "a");
    EXPECT(` (Number "10") `, 10);
    EXPECT(` (Number "foo") `, isNaN);
    EXPECT(` (BigInt "10") `, 10n);
    EXPECT(` (BigInt 10) `, 10n);
    EXPECT_ERROR(` (BigInt "foo") `, SyntaxError);  // This is a weird JavaScript thing; why not TypeError?

    EXPECT(` (cos 3) `, Math.cos(3));   // Functions imported directly from JavaScript
    EXPECT(` (atan2 3 4) `, Math.atan2(3, 4));

    EXPECT(` (apply + '(1 2)) `, 3);

    EXPECT(` (! true) `, false);
    EXPECT(` (! false) `, true);
    EXPECT(` (! nil) `, true);
    EXPECT(` (! 'a) `, false);
    EXPECT(` (! '(a)) `, false);
    EXPECT(` (! nil) `, true);
    EXPECT(` (! 0) `, false); // different from JS
    EXPECT(` (! 0n) `, false); // different from JS
    EXPECT(` (! "") `, false); // different from JS
    EXPECT(` (! 1) `, false);
    EXPECT(` (! 1n) `, false);
    EXPECT(` (! null) `, true);
    EXPECT(` (! {}) `, false);
    EXPECT(` (! []) `, true); // [] is NIL!
    EXPECT(` (! [1]) `, false); 
    EXPECT(` (~ 3) `, ~3);
    EXPECT(` (** 5 7) `, 5**7);
    EXPECT(` (% 1235 37) `, 1235%37);
    EXPECT(` (<< 234 4) `, 234 << 4);
    EXPECT(` (>> 345 3) `, 345 >> 3);
    EXPECT(` (>> -345 3) `, -345 >> 3);
    EXPECT(` (>>> -1 4) `, -1 >>> 4);
    EXPECT(` (>>> 1 4) `, 1 >>> 4);
    EXPECT(` (in "a" '{"a": 1}) `, true);
    EXPECT(` (in "b" '{"a": 1}) `, false);
    EXPECT(` (in 'a '{a: 1}) `, true);
    EXPECT(` (in 'b '{a: 1}) `, false);
    EXPECT(` (in "a" '{a: 1}) `, false);
    EXPECT(` (in 'a '{"a": 1}) `, false);
    EXPECT(` (new RangeError) `, res => res instanceof RangeError);
    EXPECT(` (@ '[a b c d e] 3) `, ` 'd `);
    EXPECT(` (@? '[a b c d e] 3) `, ` 'd `);
    EXPECT_ERROR(` (@ (void) 3) `, TypeError);
    EXPECT(` (@? (void) 3) `, undefined);
    EXPECT(` (void) `, undefined);
    EXPECT(` (undefined? (void)) `, true);
    EXPECT(` (void 1 2 3) `, undefined);
    // Args are evaled, but undefined is returned; just like in JavaScript.
    // This is one way to deliberately manifest an "undefined" value.
    EXPECT_ERROR(` (void 1 2 (xyz q)) `, SchemeEvalError);

    EXPECT(` (to-lower-case "AbCd") `, expectString("abcd"));
    EXPECT(` (to-lower-case "AbCd" ["en-US"]) `, expectString("abcd"));
    EXPECT(` (to-lower-case "AbCd" "en-US") `, expectString("abcd"));
    EXPECT(` (to-upper-case "AbCd") `, expectString("ABCD"));
    EXPECT(` (to-upper-case "AbCd" ["en-US"]) `, expectString("ABCD"));
    EXPECT(` (to-upper-case "AbCd" "en-US") `, expectString("ABCD"));
    // TODO: Make parser accept unicode escapes \uxxxx and \u{xxxxxx},
    // then test Turkish locale:
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase

    { // Unbound variables in functions
      let savedScope = beginTestScope();
      EXPECT(` (define (a x) (+ x y)) `, ` 'a `);
      EXPECT_ERROR(` (a 3) `, SchemeEvalError);
      EXPECT(` (define y 5) `, ` 'y `);
      EXPECT(` (a 3) `, 8);
      EXPECT(` (define (a x y) (plus* x y)) `, ` 'a `);
      EXPECT_ERROR(` (plus 4 5) `, SchemeEvalError);
      EXPECT(` (define plus* +) `, ` 'plus* `);
      EXPECT(` (plus* 4 5) `, 9)
      endTestScope(savedScope);
    }

    { // Object and Array literals
      let savedScope = beginTestScope();
      EXPECT(` (define a 2) `, ` 'a `);
      EXPECT(` { [a]: 3, "b": 4, "c": a } `, ` '{ "2": 3, "b": 4, "c": 2 } `)
      EXPECT(` [ a, 3 ] `, ` '[2, 3] `);
      endTestScope(savedScope);
    }

    EXPECT(` (+) `, isNaN);  // Sure. Why not?
    EXPECT(` (+ 1) `, isClosure);
    EXPECT(` ((+ 1) 2) `, 3);
    EXPECT(` (+ 1 2) `, 3);
    EXPECT(` (+ 1 2 3) `, 6);
    EXPECT(` (+ 1n 2n) `, 3n);
    EXPECT_ERROR(` (+ 1 2n) `, TypeError);
    EXPECT(` (-) `, isNaN);
    EXPECT(` (- 3) `, -3);
    EXPECT(` (- 3n) `, -3n);
    EXPECT(` (- 100 2 5 10) `, 83);
    EXPECT(` (*) `, isNaN);
    EXPECT(` (* 1) `, isClosure);
    EXPECT(` (* 1 2) `, 2);
    EXPECT(` (* 1 2 3) `, 6);
    EXPECT(` (* 1 2 3 4) `, 24);
    EXPECT(` (* 300n 200n) `, 60000n);
    EXPECT(` (/) `, isNaN);
    EXPECT(` (/ 5) `, 1/5);
    EXPECT(` (/ 0) `, Infinity);
    EXPECT(` (/ -1 0) `, -Infinity);
    EXPECT(' (/ 3 7) ', 3/7);
    EXPECT(' (/ 100000 10 10 10) ', 100);

    EXPECT(` (?) `, false);
    EXPECT(` (? true) `, true);
    EXPECT(` (? false) `, false);
    EXPECT(` (? true 1) `, 1);
    EXPECT(` (? false 1) `, false);
    EXPECT(` (? true 1 2) `, 1);
    EXPECT(` (? false 1 2) `, 2);
    EXPECT(` (? (< 3 5) (+ 3 4) (* 3 4)) `, 7);
    EXPECT(` (? (> 3 5) (+ 3 4) (* 3 4)) `, 12);
    EXPECT(` (? nil true false) `, false );
    EXPECT(` (? null true false) `, false );
    EXPECT(` (? (void) true false) `, false );
    EXPECT(` (? false true false) `, false );
    EXPECT(` (? true true false) `, true );
    EXPECT(` (? 'a true false) `, true );
    EXPECT(` (? 1 true false) `, true );
    EXPECT(` (? 0 true false) `, true );
    EXPECT(` (? "str" true false) `, true );
    EXPECT(` (? "" true false) `, true );
    EXPECT(` (? cons true false) `, true );
    EXPECT(` (? {a: 1} true false) `, true );
    EXPECT(` (? {} true false) `, true );
    EXPECT(` (? [] true false) `, false ); // [] is nil
    EXPECT(` (? [1] true false) `, true );
    EXPECT(` (? [1 2 3] true false) `, true );
    EXPECT(` (? true 1 2 (oops!)) `, 1);
    EXPECT(` (? false 1 2 (oops!)) `, 2);
    EXPECT(` (? true 1 (oops!)) `, 1);
    EXPECT(` (? false (oops!) 2) `, 2);
    EXPECT_ERROR(` (? false 1 (oops!)) `, SchemeEvalError);
    EXPECT_ERROR(` (? true (oops!) 2) `, SchemeEvalError);
    EXPECT_ERROR(` (? (oops!) 1 2) `, SchemeEvalError);

    EXPECT(` (bigint?) `, false);
    EXPECT(` (bigint? 1n) `, true);
    EXPECT(` (bigint? 1) `, false);
    EXPECT(` (bigint? "str") `, false);
    EXPECT(` (bigint? 1n "foo") `, expectString("foo"));
    EXPECT(` (bigint? 1 "foo") `, false);
    EXPECT(` (bigint? 1n 2n 2) `, 2n);
    EXPECT(` (bigint? 1 2n 2) `, 2);

    EXPECT(` (<) `, false);
    EXPECT(` (< 5) `, isClosure);
    EXPECT(` (< 5 3) `, false);
    EXPECT(` (< 3 5) `, true);
    EXPECT(` (< 3 3) `, false);
    EXPECT(` (< 1 2 3 4 5 6) `, true);  // each less than the previous
    EXPECT(` (< 1 2 3 4 4 5 6) `, false);
    EXPECT(` (< 1 2 3 10 4 5 6) `, false);
    EXPECT_ERROR(` (< 1 2 3 4 5 6 (oops!)) `, SchemeEvalError);
    EXPECT(` (< 1 2 3 4 4 5 6 (oops!)) `, false); // Short-circuits on false
    EXPECT(` (< 1 2 3 10 4 5 6 (oops!)) `, false);
    EXPECT(` (<=) `, false);
    EXPECT(` (<= 5) `, isClosure)
    EXPECT(` (<= 5 3) `, false);
    EXPECT(` (<= 3 5) `, true);
    EXPECT(` (<= 3 3) `, true);
    EXPECT(` (<= 1 2 3 4 5 6) `, true);  // each less or equal to than the previous
    EXPECT(` (<= 1 2 3 4 4 5 6) `, true);
    EXPECT(` (<= 1 2 3 10 4 5 6) `, false);
    EXPECT_ERROR(` (<= 1 2 3 4 5 6 (oops!)) `, SchemeEvalError);
    EXPECT_ERROR(` (<= 1 2 3 4 4 5 6 (oops!)) `, SchemeEvalError);
    EXPECT(` (< 1 2 3 10 4 5 6 (oops!)) `, false); // Short-circuits on false
    EXPECT(` (>) `, false);
    EXPECT(` (> 5) `, isClosure);
    EXPECT(` (> 5 3) `, true);
    EXPECT(` (> 3 5) `, false);
    EXPECT(` (> 3 3) `, false);
    EXPECT(` (> 6 5 4 3 2 1) `, true);  // each greater than the previous
    EXPECT(` (> 6 5 4 4 3 2 1) `, false);
    EXPECT(` (> 6 5 4 10 3 2 1) `, false);
    EXPECT_ERROR(` (> 6 5 4 3 2 1 (oops!)) `, SchemeEvalError);
    EXPECT(` (> 6 5 4 10 3 2 1 (oops!)) `, false); // Short-circuits on false
    EXPECT(` (> 6 5 4 10 3 2 1 (oops!)) `, false);
    EXPECT(` (>=) `, false);
    EXPECT(` (>= 5) `, isClosure);
    EXPECT(` (>= 5 3) `, true);
    EXPECT(` (>= 3 5) `, false);
    EXPECT(` (>= 3 3) `, true);
    EXPECT(` (>= 6 5 4 3 2 1) `, true);  // each greater than or equal to the previous
    EXPECT(` (>= 6 5 4 4 3 2 1) `, true);
    EXPECT(` (>= 6 5 4 10 3 2 1) `, false);
    EXPECT_ERROR(` (>= 6 5 4 3 2 1 (oops!)) `, SchemeEvalError);
    EXPECT_ERROR(` (>= 6 5 4 4 3 2 1 (oops!)) `, SchemeEvalError);
    EXPECT(` (>= 6 5 4 10 3 2 1 (oops!)) `, false); // Short-circuits on false
    EXPECT(` (=) `, true);   // "nothing" is equal to itself
    EXPECT(` (= 5) `, isClosure);
    EXPECT(` (= 5 3) `, false);
    EXPECT(` (= 3 5) `, false);
    EXPECT(` (= 3 3) `, true);
    EXPECT(` (= 3 3 3 3 3 3) `, true);  // all equal
    EXPECT(` (= 3 3 3 3 4 3) `, false); // not all equal
    EXPECT_ERROR(` (= 3 3 3 3 3 3 (oops!)) `, SchemeEvalError);
    EXPECT(` (= 3 3 3 3 4 3 (oops!)) `, false); // Short-circuits on false
    EXPECT(` (= '(1 2 [ 3 4 5 ] 6) '(1 2 [ 3 4 5] 6)) `, true);
    EXPECT(` (= '(1 2 [ 3 7 5 ] 6) '(1 2 [ 3 4 5] 6)) `, false);
    EXPECT(` (==) `, true);   // "nothing" is equal to itself
    EXPECT(` (== 5) `, isClosure);
    EXPECT(` (== 5 3) `, false);
    EXPECT(` (== 3 5) `, false);
    EXPECT(` (== 3 3) `, true);
    EXPECT(` (== 3 3 3 3 3 3) `, true);  // all equal
    EXPECT(` (== 3 3 3 3 4 3) `, false); // not all equal
    EXPECT_ERROR(` (== 3 3 3 3 3 3 (oops!)) `, SchemeEvalError);
    EXPECT(` (== 3 3 3 3 4 3 (oops!)) `, false); // Short-circuits on false
    EXPECT(` (!=) `, false);  // nothing isn't equal to itself
    EXPECT(` (!= 5) `, isClosure);
    EXPECT(` (!= 5 3) `, true);
    EXPECT(` (!= 3 5) `, true);
    EXPECT(` (!= 3 3) `, false);
    EXPECT(` (!= 3 3 3 3 3 3) `, false);  // all equal
    EXPECT(` (!= 3 3 3 3 4 3) `, true);   // not all equal
    EXPECT_ERROR(` (!= 3 3 3 3 3 3 (oops!)) `, SchemeEvalError);
    EXPECT(` (!= 3 3 3 3 4 3 (oops!)) `, true); // Short-circuits on false
    let list1 = `(a b (c d) 2 3)`, list2 = `(1 2 (7 3) x)`;
    // TODO: Revist these. Make sure they match SIOD.
    // EXPECT(` (== == eq?) `, true);
    // EXPECT(` (eq? '${list1} '${list1}) `, false);
    // EXPECT(` (equal? '${list1} '${list1}) `, true);
    // EXPECT(` (eq? '${list1} '${list2}) `, false);
    // EXPECT(` (equal? '${list1} '${list2}) `, false);

    EXPECT(` (&) `, 0);
    EXPECT(` (& 76134) `, isClosure);
    EXPECT(` ((& 76134) 14345)`, 76134 & 14345);
    EXPECT(` (& 0b1001101011 0b1110101011) `, 0b1001101011 & 0b1110101011);
    EXPECT(` (& 0b1001101011 0b1110101011 0b11110111101111) `, 0b1001101011 & 0b1110101011 & 0b11110111101111);
    EXPECT(` (|) `, 0);
    EXPECT(` (| 76134) `, isClosure);
    EXPECT(` (| 0b1001101011 0b1110101011) `, 0b1001101011 | 0b1110101011);
    EXPECT(` (| 0b1001101011 0b1110101011 0b11110111101111) `, 0b1001101011 | 0b1110101011 | 0b11110111101111);
    EXPECT(` (^) `, 0);
    EXPECT(` (^ 76134) `, isClosure);
    EXPECT(` (^ 0b1001101011 0b1110101011) `, 0b1001101011 ^ 0b1110101011);
    EXPECT(` (^ 0b1001101011 0b1110101011 0b11110111101111) `, 0b1001101011 ^ 0b1110101011 ^ 0b11110111101111);


    EXPECT(` (max) `, undefined);
    EXPECT(` (max 5) `, isClosure);
    EXPECT(` (max 3 7 9 2 4) `, 9);
    EXPECT(` (min) `, undefined);
    EXPECT(` (min 5) `, isClosure);
    EXPECT(` (min 3 7 9 2 4) `, 2);

    EXPECT(` (&&) `, true);
    EXPECT(` (&& 1) `, 1);
    EXPECT(` (&& 1 2) `, 2);
    EXPECT(` (&& 1 false 2) `, false);
    EXPECT(` (&& 1 false (oops!)) `, false);  // short-circuits
    EXPECT_ERROR(` (&& 1 true (oops!)) `, SchemeEvalError);
    EXPECT(` (||) `, false);
    EXPECT(` (|| 1) `, 1);
    EXPECT(` (|| 1 2) `, 1);
    EXPECT(` (|| nil null (void) false 2 3) `, 2); 
    // Only false, nil, null, and undefined are false; specifically, 0 and "" are NOT false
    EXPECT(` (|| nil null (void) false 0 2 3) `, 0);
    EXPECT(` (|| nil null (void) false "" 2 3) `, `""`);
    EXPECT(` (|| 5 (oops!)) `, 5);  // short-circuits
    EXPECT_ERROR(` (|| nil null (void) false (oops!)) `, SchemeEvalError);
    EXPECT(` (??) `, undefined);
    EXPECT(` (?? 1) `, 1);
    EXPECT(` (?? 1 2) `, 1);
    EXPECT(` (?? null) `, null);
    EXPECT(` (?? (void)) `, undefined);
    EXPECT(` (?? null 1) `, 1);
    EXPECT(` (?? null 1 (oops!)) `, 1);
    EXPECT(` (?? (void) 1) `, 1);
    EXPECT(` (?? null (void) nil false 2 3) `, NIL); 

    EXPECT(` (begin) `, NIL);
    EXPECT(` (begin 1) `, 1);
    EXPECT(` (begin 1 2 3) `, 3);
    EXPECT(` (begin (+ 3 4) (* 3 4)) `, 12);
    EXPECT(` (prog1) `, NIL);
    EXPECT(` (prog1 1) `, 1);
    EXPECT(` (prog1 1 2 3) `, 1);
    EXPECT(` (prog1 (+ 3 4) (* 3 4)) `, 7);

    EXPECT(` (cond) `, NIL);
    EXPECT_ERROR(` (cond a) `, isCompileOrEvalError);
    EXPECT_ERROR(` (cond 1) `, isCompileOrEvalError);
    EXPECT_ERROR(` (cond ()) `, isCompileOrEvalError);
    EXPECT_ERROR(` (cond (true) 1) `, isCompileOrEvalError);
    EXPECT(` (cond ((< 4 5) (+ 5 6))) `, 11);
    EXPECT(` (cond ((< 4 5) (+ 5 6) (* 5 6))) `, 30);
    EXPECT(` (cond ((> 4 5) (+ 5 6) (* 5 6))
                    ((< 4 5) (+ 2 9) (* 5 3))) `, 15);

    EXPECT(` (append) `, NIL);
    EXPECT(` (append '(a b c)) ` , ` '(a b c) `);
    EXPECT(` (append '(a b c) '(d e f)) ` , ` '(a b c d e f) `);
    EXPECT(` (append '(a b c) '(d e f)) ` , ` '(a b c d e f) `);
    EXPECT(` (append '(a b c) '(d e f) '(g h i)) ` , ` '(a b c d e f g h i) `);
    EXPECT(` (append '[a b c]) ` , ` '(a b c) `);
    EXPECT(` (append '[a b c] '[d e f]) ` , ` '(a b c d e f) `);
    EXPECT(` (append '(a b c) '(d e f)) ` , ` '(a b c d e f) `);
    EXPECT(` (append '(a b c) '[d e f] "ghi") ` , ` '(a b c d e f "g" "h" "i") `);
    EXPECT_ERROR(` (last) `, TypeError);
    EXPECT_ERROR(` (last 'a) `, TypeError);
    EXPECT(` (last ()) `, NIL);  // XXX whay should this really do?
    EXPECT(` (last '(a)) `, ` 'a `);
    EXPECT(` (last '(a b)) `, ` 'b `);
    EXPECT(` (last '(a b c)) `, ` 'c `);
    EXPECT(` (last '[]) `, NIL);
    EXPECT(` (last '[a]) `, ` 'a `);
    EXPECT(` (last '[a b]) `, ` 'b `);
    EXPECT(` (last '[a b c]) `, ` 'c `);
    EXPECT(` (last "abc") `, ` "c" `);
    EXPECT_ERROR(` (butlast) `, TypeError);
    EXPECT_ERROR(` (butlast 'a) `, TypeError);
    EXPECT(` (butlast ()) `, NIL);
    EXPECT(` (butlast '(a)) `, NIL );
    EXPECT(` (butlast '(a b)) `, ` '(a) `);
    EXPECT(` (butlast '(a b c)) `, ` '(a b) `);
    EXPECT_ERROR(` (length) `, TypeError);
    EXPECT_ERROR(` (length 'a) `, TypeError);  // Not a list or iterable
    EXPECT_ERROR(` (length 1) `, TypeError);  // Not a list or iterable
    EXPECT(` (length '()) `, 0);
    EXPECT(` (length '(a)) `, 1);
    EXPECT(` (length '(a b)) `, 2);
    EXPECT(` (length '(a b c d)) `, 4);
    EXPECT(` (length '[a b c d]) `, 4);
    EXPECT(` (length "abcd") `, 4);

    EXPECT(` (list) `, NIL);
    EXPECT(` (list 'a) `, ` '(a) `);
    EXPECT(` (list 'a 'b) `, ` '(a b) `);
    EXPECT(` (list 'a 'b 'c) `, ` '(a b c) `);
    EXPECT(` (list 'a '(b c) 'd) `, ` '(a (b c) d) `);
    EXPECT(` (reverse) `, NIL);
    EXPECT_ERROR(` (reverse 'a) `, TypeError);
    EXPECT(` (reverse '(a)) `, ` '(a) `);
    EXPECT(` (reverse '(a b)) `, ` '(b a) `);
    EXPECT(` (reverse '(a b c)) `, ` '(c b a) `);
    EXPECT(` (reverse '(a b c) '(d e f)) `, ` '(f e d c b a) `);

    EXPECT(` (memq) `, NIL);
    EXPECT(` (memq 'a) `, isClosure);
    EXPECT(` (memq 'a 1) `, NIL);
    EXPECT(` (memq 'c '(a b c d e f g)) `, ` '(c d e f g) `);
    EXPECT(` (memq 'z '(a b c d e f g)) `, NIL);
    EXPECT_ERROR(` (nth) `, TypeError);
    EXPECT(` (nth 'a) `, isClosure);
    EXPECT(` (nth 4 '(a b c d e f g)) `, ` 'e `);
    EXPECT_ERROR(` (nth 4.5 '(a b c d e f g)) `, TypeError);
    EXPECT(` (nth 4 '[a b c d e f g]) `, ` 'e `);
    EXPECT(` (nth 4 "abcde") `, ` "e" `);
    EXPECT(` (nth 0 '(a b c d e f g)) `, ` 'a `);
    EXPECT(` (nth 0 '[a b c d e f g]) `, ` 'a `);
    EXPECT(` (nth 0 "abcde") `, ` "a" `);
    EXPECT(` (nth 6 '(a b c d e f g)) `, ` 'g `);
    EXPECT(` (nth 6 '[a b c d e f g]) `, ` 'g `);
    EXPECT(` (nth 6 "abcdefg") `, ` "g" `);
    EXPECT_ERROR(` (nth -1 '(a b c d e f g)) `, RangeError);
    EXPECT_ERROR(` (nth -1 '[a b c d e f g]) `, RangeError);
    EXPECT_ERROR(` (nth -1 "abcdefg") `, RangeError);
    EXPECT_ERROR(` (nth 7 '(a b c d e f g)) `, RangeError);
    EXPECT_ERROR(` (nth 7 '[a b c d e f g]) `, RangeError);
    EXPECT_ERROR(` (nth 7 "abcdefg") `, RangeError);

    EXPECT(` (apropos "c") `, isList);  // weak test but gets some coverage
    EXPECT(` (map) `, NIL);
    EXPECT(` (map (lambda (x) (* 2 x)) '(1 2 3)) `, ` '(2 4 6) `);
    EXPECT(` (map (lambda (x) (* 2 x)) '[1 2] '(3)) `, ` '(2 4 6) `);
    EXPECT(` (map (lambda (x) (* 2 x))) `, NIL);
    EXPECT(` (array-map (lambda (x) (* 2 x)) '(1 2) '[3]) `, ` '[2 4 6] `);
    EXPECT(` (filter (< 3) '(4 2 6 5 1 7)) `, ` '(4 6 5 7) `);

    // TODO: let needs a lot more testing
    EXPECT(` (let ((x 10)
                  (y 20))
                (+ x y)) `, 30);
    EXPECT(` (let ((x 10)
                  (y 20)))`, isClosure);
    // Partially applying "let" returns a function that evaluates
    // its arguments in the "let" scope.
    EXPECT(` ((let ((x 10)
                    (y 20)))
                (+ x y)) `, 30);

    EXPECT(` (*catch "foo" (+ 2 (* 5 (*throw "foo" "ha!")))) `, expectString("ha!"));
    EXPECT(` (catch (e (+ "thrown: " e))
               (+ 1 2)
               (+ 3 (throw "ha ha!"))
               ) `, expectString("thrown: ha ha!"));

    EXPECT_ERROR(` (sort) `, TypeError);
    EXPECT(` (sort '(6 4 5 7 6 8 3)) `, ` '(3 4 5 6 6 7 8) `);
    EXPECT(` (sort '[6 4 5 7 6 8 3]) `, ` '[3 4 5 6 6 7 8] `);
    EXPECT(` (sort '(6 4 5 7 6 8 3) >) `, ` '(8 7 6 6 5 4 3) `);
    EXPECT(` (sort '[6 4 5 7 6 8 3] >) `, ` '[8 7 6 6 5 4 3] `);
    EXPECT(` (qsort '((3 a) (2 b)) < car)	`, ` '((2 b) (3 a)) `);
    EXPECT(` (sort '(6 4 5 7 35 193 6 23 29 15 89 23 42 8 3)) `,
        result => globalScope.apply(globalScope.le, result));
    EXPECT(` (sort '(6 4 5 7 35 193 6 23 29 15 89 23 42 8 3) >)`,
        result => globalScope.apply(globalScope.ge, result));

    { // "Rest" parameters
      let savedScope = beginTestScope();
      EXPECT(` (define (foo a b . c) c) `, ` 'foo `)
      EXPECT(` (foo 1 2 3 (+ 2 2) 5) `, ` '(3 4 5) `);
      endTestScope(savedScope);
    }

    { // "Rest" parameters, compiled
      let savedScope = beginTestScope();
      EXPECT(` (compile (foo a b . c) c) `, ` 'foo `)
      EXPECT(` (foo 1 2 3 (+ 2 2) 5) `, ` '(3 4 5) `);
      endTestScope(savedScope);
    }

    { // optional paramaters
      let savedScope = beginTestScope();
      EXPECT(` (define (opt a b (? c (+ 2 3))) (list a b c)) `, ` 'opt `)
      EXPECT(` (opt 1 2 3) `, ` '(1 2 3) `);
      EXPECT(` (opt 1 2) `, ` '(1 2 5) `);
      EXPECT(` (opt 1) `, isClosure);
      EXPECT(` ((opt 1) 8) `, ` '(1 8 5) `);
      endTestScope(savedScope);
    }

    { // optional paramaters, compiled
      let savedScope = beginTestScope();
      EXPECT(` (compile (opt a b (? c (+ 2 3))) (list a b c)) `, ` 'opt `)
      EXPECT(` (opt 1 2 3) `, ` '(1 2 3) `);
      EXPECT(` (opt 1 2) `, ` '(1 2 5) `);
      EXPECT(` (opt 1) `, isClosure);
      EXPECT(` ((opt 1) 8) `, ` '(1 8 5) `);
      endTestScope(savedScope);
    }

    { // special lambdas
      let savedScope = beginTestScope();
      EXPECT(` (define a 1) `, ` 'a `);
      EXPECT(` (define b 2) `, ` 'b `);
      EXPECT(` (define c 3) `, ` 'c `);
      EXPECT(` (define d 4) `, ` 'd `);
      EXPECT(` (define e 5) `, ` 'e `);
      // \\ Because of string literal syntax. It's really \#
      EXPECT(` (define x (\\# 2 (p1 p2 p3 p4 p5) (list p1 p2 p3 p4 p5))) `, ` 'x `)
      EXPECT(` (x a b c d e) `, ` '(1 2 c d e) `);
      EXPECT(` (define y (\\# 1 (p1 p2 p3 p4 p5) (list p1 p2 p3 p4 p5))) `, ` 'y `)
      EXPECT(` (y a b c d e) `, ` '(1 b c d e) `);
      EXPECT(` (define z (\\# 0 (p1 p2 p3 p4 p5) (list p1 p2 p3 p4 p5))) `, ` 'z `)
      EXPECT(` (z a b c d e) `, ` '(a b c d e) `);
      endTestScope(savedScope);
    }

    // TODO: test compiling special lambdas; need a good API for that.

    { // Partial application returning closures
      let savedScope = beginTestScope();
      EXPECT(` (define mul-by-5 (* 5)) `, ` 'mul-by-5 `);
      EXPECT(` mul-by-5 `, isClosure);
      EXPECT(` (mul-by-5 3) `, 15);
      EXPECT(` (define (_add a b) (+ a b)) `, ` '_add `);
      EXPECT(` (_add 5 6) `, 11);
      EXPECT(` (define add-4 (_add 4)) `, ` 'add-4 `);
      EXPECT(` add-4 `, isClosure);
      EXPECT(` (add-4 3) `, 7);
      // While we're at it, test notation
      EXPECT(` (define (increment-by n) (\\x.(+ x n))) `, ` 'increment-by `);
      EXPECT(` (define increment-by-3 (increment-by 3)) `, ` 'increment-by-3 `);
      EXPECT(` increment-by-3 `, isClosure);
      EXPECT(` (increment-by-3 4) `, 7);
      endTestScope(savedScope);
    }

    { // Partial application returning closures, compiled
      let savedScope = beginTestScope();
      EXPECT(` (compile (increment-by n) (\\x.(+ x n))) `, ` 'increment-by `);
      EXPECT(` (define increment-by-3 (increment-by 3)) `, ` 'increment-by-3 `);
      EXPECT(` increment-by-3 `, isClosure);
      EXPECT(` (increment-by-3 4) `, 7);
      endTestScope(savedScope);
    }

    //
    // Scenario tests
    //

    {
      let savedScope = beginTestScope();
      EXPECT(`
        (define (factoral x)
          (? (<= x 1) 
            (? (bigint? x) 1n 1)
            (* x (factoral (- x (? (bigint? x) 1n 1))))
        ))`,
        ` 'factoral `);
      EXPECT(` (factoral 10) `, 3628800);
      EXPECT(` (factoral 10n) `, 3628800n);
      EXPECT(` (factoral 171) `, Infinity);
      EXPECT(` (factoral 171n) `, 1241018070217667823424840524103103992616605577501693185388951803611996075221691752992751978120487585576464959501670387052809889858690710767331242032218484364310473577889968548278290754541561964852153468318044293239598173696899657235903947616152278558180061176365108428800000000000000000000000000000000000000000n);
      endTestScope(savedScope);
      // Factoral should be undefined now
      EXPECT_ERROR(` (factoral 10) `, SchemeEvalError);
    }

    {
      let savedScope = beginTestScope();
      EXPECT(`
        (compile (factoral x)
          (? (<= x 1) 
            (? (bigint? x) 1n 1)
            (* x (factoral (- x (? (bigint? x) 1n 1))))
        ))`,
        ` 'factoral `);
      EXPECT(` factoral `, obj => typeof obj === 'function');
      EXPECT(` (factoral 10) `, 3628800);
      EXPECT(` (factoral 10n) `, 3628800n);
      EXPECT(` (factoral 171) `, Infinity);
      EXPECT(` (factoral 171n) `, 1241018070217667823424840524103103992616605577501693185388951803611996075221691752992751978120487585576464959501670387052809889858690710767331242032218484364310473577889968548278290754541561964852153468318044293239598173696899657235903947616152278558180061176365108428800000000000000000000000000000000000000000n);
      endTestScope(savedScope);
      // Factoral should be undefined now
      EXPECT_ERROR(` (factoral 10) `, SchemeEvalError);
    }

    { // Test that when a when a bound function changes, the JIT's guards catch it.
      let savedScope = beginTestScope();
      EXPECT(` (define op +) `, ` 'op `);
      EXPECT(` (define (run-op a b c) (op a b c))`, ` 'run-op `);
      EXPECT(` (run-op 2 3 4) `, 9);
      EXPECT(` (define op *) `, ` 'op `);
      EXPECT(` (run-op 2 3 4) `, 24);
      endTestScope(savedScope);
     }
  }

  function internalsSuite() {
    //
    // Internals tests
    //

    { // Used by the compiler to convert wild Scheme identifiers into valid JavaScript identifiers
      const toJavaScriptIdentifier = globalScope.toJavaScriptIdentifier;
      const testToJavaScriptIdentifier = name => () => toJavaScriptIdentifier(name);
      EXPECT(testToJavaScriptIdentifier("aNormal_name0234"), expectString("_aNormal_name0234"));
      EXPECT(testToJavaScriptIdentifier("aname%with&/specialChars?"), expectString("_aname$pct_with$and$stroke_specialChars$q"));
      EXPECT(testToJavaScriptIdentifier("_begins_with_underscore"), expectString("__begins_with_underscore"));
      EXPECT(testToJavaScriptIdentifier("number?"), expectString("_number$q"));
      EXPECT(testToJavaScriptIdentifier("&&"), expectString("$and$and"));
      EXPECT(testToJavaScriptIdentifier("$"), expectString("$cash"));
      EXPECT(testToJavaScriptIdentifier("?"), expectString("$q"));
    }

    { // Used to determine how to call (and display) JavaScript functions
      const analyzeJSFunction = globalScope.analyzeJSFunction;
      const testAnalyze = (fn) => () => analyzeJSFunction(fn);
      const trimCompare = (a, b) => a.trim() === b.trim();
      EXPECT(testAnalyze(x => x * x),
        { name: '', params: ['x'], restParam: undefined, valueTemplate: 'x * x', printParams: 'x',
          bodyTemplate: undefined, printBody: undefined, native: false, requiredCount: 1, usesThis: false });
      EXPECT(testAnalyze((x) => x * x),
        { name: '', params: ['x'], restParam: undefined, valueTemplate: 'x * x', printParams: '(x)',
          bodyTemplate: undefined, printBody: undefined, native: false, requiredCount: 1, usesThis: false });
      EXPECT(testAnalyze((x, y) => x * y),
        { name: '', params: ['x', 'y'], restParam: undefined, valueTemplate: 'x * y', printParams: '(x, y)',
          bodyTemplate: undefined, printBody: undefined, native: false, requiredCount: 2, usesThis: false });
      EXPECT(testAnalyze((x, ...y) => x * y),
        { name: '', params: ['x'], restParam: 'y', valueTemplate: 'x * y', printParams: '(x, ...y)',
          bodyTemplate: undefined, printBody: undefined, native: false, requiredCount: 1, usesThis: false });
      EXPECT(testAnalyze((x, y, ...z) => x * y),
        { name: '', params: ['x','y'], restParam: 'z', valueTemplate: 'x * y', printParams: '(x, y, ...z)',
          bodyTemplate: undefined, printBody: undefined, native: false, requiredCount: 2, usesThis: false });
      EXPECT(testAnalyze((...x) => x * x),
        { name: '', params: [], restParam: 'x', valueTemplate: 'x * x', printParams: '(...x)',
          bodyTemplate: undefined, printBody: undefined, native: false, requiredCount: 0, usesThis: false });
      EXPECT(testAnalyze((...x) => { let res = x * x; return res }),
        { name: '', params: [], restParam: 'x', valueTemplate: 'res', printParams: '(...x)',
          bodyTemplate: 'let res = x * x;', printBody: undefined, native: false, requiredCount: 0, usesThis: false });
      EXPECT(testAnalyze((x, y, a, b = [], c, d) => x * y),
        { name: '', params: ['x', 'y', 'a', 'b', 'c', 'd'], restParam: undefined, valueTemplate: 'x * y',
          printParams: '(x, y, a, b = [], c, d)',
          bodyTemplate: undefined, printBody: undefined, native: false, requiredCount: 3, usesThis: false });
      EXPECT(testAnalyze(function (a) { a = 2 * a; return a; }),
        { name: '', params: ['a'], restParam: undefined, valueTemplate: 'a', printParams: '(a)',
          bodyTemplate: 'a = 2 * a;', printBody: ' { a = 2 * a; return a; }', native: false, requiredCount: 1, usesThis: false },
        { stringCompare: trimCompare });
      EXPECT(testAnalyze(function(a, b, c) { a = 2 * a; return a; }),
        { name: '', params: ['a','b','c'], restParam: undefined, valueTemplate: 'a', printParams: '(a, b, c)',
          bodyTemplate: 'a = 2 * a;', printBody: ' { a = 2 * a; return a; }', native: false, requiredCount: 3, usesThis: false },
        { stringCompare: trimCompare });
      EXPECT(testAnalyze(function fn(a) { a = 2 * a; return a; }),
        { name: 'fn', params: ['a'], restParam: undefined, valueTemplate: 'a', printParams: '(a)',
          bodyTemplate: 'a = 2 * a;', printBody: ' { a = 2 * a; return a; }', native: false, requiredCount: 1, usesThis: false });
      EXPECT(testAnalyze(function fn(a, b, c) { a = 2 * a; return a; }),
        { name: 'fn', params: ['a','b','c'], restParam: undefined, valueTemplate: 'a', printParams: '(a, b, c)',
          bodyTemplate: 'a = 2 * a;', printBody: ' { a = 2 * a; return a; }', native: false, requiredCount: 3, usesThis: false });
      EXPECT(testAnalyze(function (a, ...rest) { return a; }),
        { name: '', params: ['a'], restParam: 'rest', valueTemplate: 'a', printParams: '(a, ...rest)',
          bodyTemplate: '', printBody: ' { return a; }', native: false, requiredCount: 1, usesThis: false }, { stringCompare: trimCompare });
      EXPECT(testAnalyze(function (a, b, c, ...rest) { return a; }),
        { name: '', params: ['a','b','c'], restParam: 'rest', valueTemplate: 'a', printParams: '(a, b, c, ...rest)',
          bodyTemplate: '', printBody: ' { return a; }', native: false, requiredCount: 3, usesThis: false }, { stringCompare: trimCompare });
      EXPECT(testAnalyze(function foo(a, ...rest) { return a; }),
        { name: 'foo', params: ['a'], restParam: 'rest', valueTemplate: 'a', printParams: '(a, ...rest)',
          bodyTemplate: '', printBody: ' { return a; }', native: false, requiredCount: 1, usesThis: false });
      EXPECT(testAnalyze(function bar(a, b, c, ...rest) { return a; }),
        { name: 'bar', params: ['a','b','c'], restParam: 'rest', valueTemplate: 'a', printParams: '(a, b, c, ...rest)',
          bodyTemplate: '', printBody: ' { return a; }', native: false, requiredCount: 3, usesThis: false });
      EXPECT(testAnalyze(function baz(...rest) { return a; }),
        { name: 'baz', params: [], restParam: 'rest', valueTemplate: 'a', printParams: '(...rest)',
          bodyTemplate: '', printBody: ' { return a; }', native: false, requiredCount: 0, usesThis: false });
      EXPECT(testAnalyze(function (...rest) { return a; }),
        { name: '', params: [], restParam: 'rest', valueTemplate: 'a', printParams: '(...rest)',
          bodyTemplate: '', printBody: ' { return a; }', native: false, requiredCount: 0, usesThis: false }, { stringCompare: trimCompare });
      EXPECT(testAnalyze(function bar(a, b, c = {}, d, e = 1, ...rest) { return a; }),
        { name: 'bar', params: ['a','b','c', 'd', 'e'], restParam: 'rest', valueTemplate: 'a',
          printParams: '(a, b, c = {}, d, e = 1, ...rest)',
          bodyTemplate: '', printBody: ' { return a; }', native: false, requiredCount: 2, usesThis: false });
      EXPECT(testAnalyze(function fn(a, b, c) { a = 2 * a; return this; }),
        { name: 'fn', params: ['a','b','c'], restParam: undefined, valueTemplate: 'this', printParams: '(a, b, c)',
          bodyTemplate: 'a = 2 * a;', printBody: ' { a = 2 * a; return this; }', native: false, requiredCount: 3, usesThis: true });
      EXPECT(testAnalyze(function fn(a, b, c) { a = 2 * this; return a; }),
        { name: 'fn', params: ['a','b','c'], restParam: undefined, valueTemplate: 'a', printParams: '(a, b, c)',
          bodyTemplate: 'a = 2 * this;', printBody: ' { a = 2 * this; return a; }', native: false, requiredCount: 3, usesThis: true });
      
      // Safari and Firefox format the "native code" part differently than V8.
      //  EXPECT(testAnalyze([].sort),
      //    { name: 'sort', params: [], restParam: undefined, valueTemplate: undefined,
      //      bodyTemplate: undefined, printBody: ' { [native code] }', native: true });
    }
  }

  //
  // A specialized tiny unit test framework that evaluates SchemeJS expressions
  //

  function testSucceeded(test, result, expected) {
    reportTestSucceeded(test, result, expected);
    succeeded += 1;
  }

  function testFailed(message, test, result, expected, report) {
    reportTestFailed(message, test, result, expected, report);
    failed += 1;
    if (throwOnError)
      throw new TestFailureError(message, test, result, expected, report);
  }

  function EXPECT(test, expected, report = {}) {
    let result, ok = false, savedScope;
    if (testScope === globalScope)
      savedScope = beginTestScope();
    // TODO: This doesn't work since Node always catches errors so there's no such
    // thing as an uncaught error.
    if (false /* throwOnError */) { // do outside of try block so errors will be uncaught
      runTest();
    } else {
      try {
        runTest();
      } catch (error) {
        if (error instanceof TestFailureError) throw error;
        testFailed("exception", test, error, expected);
        return;
      } finally {
        if (savedScope)
          endTestScope(savedScope);
      }
    }
    if (ok) {
      testSucceeded(test, result, expected);
    } else {
      testFailed(test, test, result, expected, report);
    }
    function runTest() {
      if (typeof test === 'function')
        result = test.call(testScope);
      else if (typeof test === 'string')
        result = evalTestString(test);
      else
        testFailed("test is neither function nor string", test, undefined, expected, report);
      try {
        if (subclassOf(expected, Error)) {
          ok = result instanceof expected;
        } else if (typeof expected === 'function') {
          ok = expected.call(testScope, result);
        } else {
          if (typeof expected === 'string')
            expected = evalString(expected);
          ok = equal(result, expected, 100, 10000, report);
        }
      } catch (error) {
        testFailed("expectation exception", test, error, expected, report);
      } finally {
        if (savedScope)
          endTestScope(savedScope);
      }
    }
  }

  function EXPECT_ERROR(test, expected) {
    let result, ok = false, report = {}, savedScope;
    if (testScope === globalScope) // Isolate this test
      savedScope = beginTestScope();
    if (!(typeof test === 'function' || typeof test === 'string')) {
      testFailed("test is neither function nor string", test, result, expected, report);
    } else {
      try {
        if (typeof test === 'function') {
          result = test.call(testScope);
        } else if (typeof test === 'string') {
          result = evalTestString(test);
        }
        testFailed("expected exception", test, result, expected, report);
      } catch (error) {
        try {
          if (typeof expected === 'string')
            expected = evalString(expected);
          if (subclassOf(expected, Error)) {
            if (error instanceof expected)
              ok = true;
          } else if (typeof expected === 'function') {
            // Has to come second because an exception is a function
            ok = expected.call(this, error);
          }
          if (ok)
            testSucceeded(test, error, expected);
          else
            testFailed("wrong exception", test, error, expected);
        } catch (error) {
          if (error instanceof TestFailureError) throw error;
          testFailed("expectation exception", test, error, expected, report);
        }
      } finally {
        if (savedScope)
          endTestScope(savedScope);
      }
    }
  }

  function subclassOf(cls, supercls) {
    while (cls != null) {
      if (cls === supercls) return true;
      cls = cls.__proto__;
    }
    return false;
  }

  // For test isolation and grouping sets not isolated from each other
  function beginTestScope() {
    let scopeStr = testScope === globalScope ? "test-isolation-scope" : "test-grouping-scope";
    testScope = newScope(testScope, scopeStr);
    return setGlobalScope(testScope);
  }

  function endTestScope(scope) {
    testScope = scope;
    setGlobalScope(scope);
  }

  // A string is typically evaluated, so this function lets you expect a string result.
  function expectString(expected) {
    return result => result === expected;
  }

  function compileThenEvalString(str) {
    let expr = parseSExpr.call(testScope, str);
    let lambda = list(Atom("lambda"), NIL, expr);
    let compiled = compile_lambda.call(testScope, Atom('testcase'), "testcase", lambda);
    return compiled.call(testScope);
  }
}
