//
// SchemeJS Unit Tests
//
// Copyright 2021 Stan Switzer
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

import * as SchemeJS from './SchemeJS.mjs';

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

//
// A specialized tiny unit test framework that evaluates SchemeJS expressions
//

function testSucceeded(test, result, expected) {
  console.info("SUCCEEDED", test, result, expected);
}

function testFailed(message, test, result, expected, report) {
  console.info("FAILED", test, result, expected, report);
  throw new TestFailureError(message, test, result, expected, report);
}

function EXPECT(test, expected) {
  let result, ok = false, report = {}. savedScope;
  if (testScope === globalScope)
    savedScope = beginTestScope();
  try {
    if (typeof test === 'function')
      result = test.call(testScope);
    else if (typeof test === 'string')
      result = testScope.evalString(test);
    else
      reportTestFailed("test is neither function nor string", test, undefined, expected, report);
    try {
      if (typeof expected === 'function') {
        ok = expected.call(testScope, result);
      } else {
        if (typeof expected === 'string')
          expected = testScope.evalString(expected);
        ok = deep_eq(result, expected, 100, report);
      }
    } catch (error) {
      reportTestFailed("expectation exception", test, error, expected, report);
    }
  } catch (error) {
    if (error instanceof TestFailureError) throw error;
    reportTestFailed("exception", test, error, expected);
    return;
  } finally {
    if (savedScope)
      endTestScope(savedScope);
  }
  if (ok) {
    reportTestSucceeded(test, result, expected);
  } else {
    reportTestFailed(test, test, result, expected, report);
  }
}

function EXPECT_ERROR(test, expected) {
  let result, ok = false, report = {}, savedScope;
  if (testScope === testGlobalScope)
    savedScope = beginTestScope();
  if (!(typeof test === 'function' || typeof test === 'string')) {
    reportTestFailed("test is neither function nor string", test, result, expected, report);
  } else {
    try {
      if (typeof test === 'function') {
        result = test.call(testScope);
      } else if (typeof test === 'string') {
        result = testScope.evalString(test);
      }
      reportTestFailed("expected exception", test, result, expected, report);
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
          reportTestSucceeded(test, error, expected);
        else
          reportTestFailed("wrong exception", test, error, expected);
      } catch (error) {
        if (error instanceof TestFailureError) throw error;
        reportTestFailed("expectation exception", test, error, expected, report);
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

