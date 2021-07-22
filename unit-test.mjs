//
// SchemeJS Unit Tests
//
// Copyright 2021 Stan Switzer
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

import * as SchemeJS from './SchemeJS.mjs';

let succeeded = 0, failed = 0, throwOnError = true;;

let globalScope = SchemeJS.createInstance({ unitTest: true }); // XXX get rid of unitTest option
let testScope = globalScope;
let string = globalScope.string;
let newScope = globalScope.newScope;
let deep_eq = globalScope.deep_eq;
let setGlobalScope = globalScope._setGlobalScope_test_hook_;

EXPECT(` (cons 1 2) `, ` '(1 . 2) `);
EXPECT(` (car '(1 . 2)) `, ` 1 `);
EXPECT(` (cdr '(1 . 2)) `, 2);
EXPECT(` (car '(1 2 3)) `, ` '1 `);
EXPECT(` (cdr '(1 2 3)) `, ` '(2 3) `);
EXPECT_ERROR( ` (car nil) `, EvalError );
EXPECT_ERROR( ` (cdr nil) `, EvalError );
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
EXPECT(` NaN `, NaN);
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
EXPECT(` (Number "foo") `, NaN);
EXPECT(` (BigInt "10") `, 10n);
EXPECT(` (BigInt 10) `, 10n);
EXPECT_ERROR(` (BigInt "foo") `, SyntaxError);  // This is a weird JavaScript thing

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
EXPECT(` (! []) `, false);
EXPECT(` (~ 3) `, ~3);
EXPECT(` (** 5 7) `, 5**7);
EXPECT(` (% 1235 37) `, 1235%37);
EXPECT(` (<< 234 4) `, 234 << 4);
EXPECT(` (>> 345 3) `, 345 >> 3);
EXPECT(` (>> -345 3) `, -345 >> 3);
EXPECT(` (>>> -1 4) `, -1 >>> 4);
EXPECT(` (>>> 1 4) `, 1 >>> 4);
EXPECT(` (in "a" {a: 1}) `, true);
EXPECT(` (in "b" {a: 1}) `, false);
EXPECT(` (new RangeError) `, res => res instanceof RangeError);
EXPECT(` (@ 3 '[a b c d e]) `, ` 'd `);
EXPECT(` (?@ 3 '[a b c d e]) `, ` 'd `);
EXPECT_ERROR(` (@ 3 (void)) `, TypeError);
EXPECT(` (?@ 3 (void)) `, undefined);
EXPECT(` (void) `, undefined);
EXPECT(` (undefined? (void)) `, true);
EXPECT(` (void 1 2 3) `, undefined);
  // Args are evaled, but undefined is returned;
  // this is one way to deliberately materialize an "undefined" value
EXPECT_ERROR(` (void 1 2 (xyz q)) `, EvalError);



//
// A specialized tiny unit test framework that evaluates SchemeJS expressions
//

console.info("UNIT TESTS COMPLETE", "Succeeded:", succeeded, "Failed:", failed);

function testSucceeded(test, result, expected) {
  console.info("SUCCEEDED", test, result, expected);
  succeeded += 1;
}

function testFailed(message, test, result, expected, report) {
  console.info("FAILED", test, result, expected, report);
  failed += 1;
  if (throwOnError)
    throw new TestFailureError(message, test, result, expected, report);
}

function EXPECT(test, expected) {
  let result, ok = false, report = {}, savedScope;
  if (testScope === globalScope)
    savedScope = beginTestScope();
  try {
    if (typeof test === 'function')
      result = test.call(testScope);
    else if (typeof test === 'string')
      result = testScope.evalString(test);
    else
      testFailed("test is neither function nor string", test, undefined, expected, report);
    try {
      if (typeof expected === 'function') {
        ok = expected.call(testScope, result);
      } else {
        if (typeof expected === 'string')
          expected = testScope.evalString(expected);
        ok = deep_eq(result, expected, 100, report);
      }
    } catch (error) {
      testFailed("expectation exception", test, error, expected, report);
    }
  } catch (error) {
    if (error instanceof TestFailureError) throw error;
    testFailed("exception", test, error, expected);
    return;
  } finally {
    if (savedScope)
      endTestScope(savedScope);
  }
  if (ok) {
    testSucceeded(test, result, expected);
  } else {
    testFailed(test, test, result, expected, report);
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
        result = testScope.evalString(test);
      }
      testFailed("expected exception", test, result, expected, report);
    } catch (error) {
      try {
        if (typeof expected === 'string')
          expected = testScope.evalString(expected);
        if (error === expected || error instanceof expected) {
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

class TestFailureError extends SchemeJSError {
  constructor(message, test, result, expected, report) {
    super(`${string(test)}; ${message}: ${string(result)}, expected: ${string(expected)}`);
    this.test = test;
    this.result = result;
    this.expected = expected;
    this.report = report;
  }
}
TestFailureError.prototype.name = "TestFailureError";

