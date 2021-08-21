//
// SchemeJS: Scheme in JavaScript
//
// Copyright 2021 Stan Switzer -- (sjswitzer [at] gmail [dot] com)
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

import * as SchemeJS from './SchemeJSCore.mjs';

export const VERSION = SchemeJS.VERSION;
export const LogicError = SchemeJS.LogicError;
const isArray = Array.isArray;


// So that optional parameters show up pretty when printed
const optional = undefined;

//
// Creates a SchemeJS instance.
//
export function createInstance(schemeOpts = {}) {
  let globalScope = SchemeJS.createInstance(schemeOpts);
  const readFile = schemeOpts.readFile;
  const latin1 = schemeOpts.latin1 ?? false;
  const supplemental = schemeOpts.supplemental ?? false;
  const dumpIdentifierMap = schemeOpts.dumpIdentifierMap ?? false;
  const jitThreshold = schemeOpts.jitThreshold ?? undefined;
  const TRACE_INTERPRETER = !!(schemeOpts.traceInterpreter ?? false);
  const TRACE_COMPILER = !!(schemeOpts.traceCompiler ?? false);
  const TRACE_COMPILER_CODE = !!(schemeOpts.traceCompilerCode ?? false);
  const _reportError = schemeOpts.reportError = error => console.log(error); // Don't call this one
  const reportSchemeError = schemeOpts.reportSchemeError ?? _reportError; // Call these instead
  const reportSystemError = schemeOpts.reportError ?? _reportError;
  const reportLoadInput = schemeOpts.reportLoadInput ?? (expr => undefined);
  const reportLoadResult = schemeOpts.reportLoadResult ?? ((result, expr) => console.log(string(result)));
  const linePrinter = schemeOpts.linePrinter ?? (line => console.log(line));
  const arraysAreLists = schemeOpts.arraysAreLists ?? true;
  const generatorsAreLists = schemeOpts.generatorsAreLists ?? true;
  const standardIteratorsAreLists = schemeOpts.standardIteratorsAreLists ?? true;
  const allIteratorsAreLists = schemeOpts.standardIteratorsAreLists ?? false;
  const lambdaStr = schemeOpts.lambdaStr ?? "\\";
  const firstName = schemeOpts.firstName ?? "first";
  const restName = schemeOpts.firstName ?? "rest";
  const nilName = schemeOpts.nilName ?? "NIL";

  let exportAPI = globalScope.exportAPI ?? required();
  let defineGlobalSymbol = globalScope.defineGlobalSymbol ?? required();
  let isList = globalScope.isList ?? required();
  let isNil = globalScope.isNil ?? required();
  let iterateAsList = globalScope.iterateAsList ?? required();
  let moreList = globalScope.moreList ?? required();
  let LIST = globalScope.LIST ?? required();
  let MORELIST = globalScope.MORELIST ?? required();
  let FIRST = globalScope.FIRST ?? required();
  let REST = globalScope.REST ?? required();
  let _eval = globalScope._eval ?? required();
  let compare_hooks = globalScope.compare_hooks ?? required();
  function required() { throw "required" }

  //
  // Equality
  //
  // Because lists can be circular and stack depths are finite, equal
  // must have bounds on its recursion and looping. The bounds are large and
  // user-optionble, but they are finite. If the bounds are exceeded, deep_eq
  // returns "undefined," which is "falsey" but distinguishable from
  // "false."
  //
  // A client can pass in a "report" object which is filled-in with a report
  // on where and how objects differ. This has proved useful for unit testing
  // but can be generally useful. The report can also contain a strCmp
  // property that determines how strings are compared, for instance you can ignore case
  // or leading and trailing whitespace; sometimes you're playing horseshoes.
  // You can also opt that NaNs are considered equal.
  //
  // An EQUAL_FUNCTION can be attached to any object or class to define how equal
  // compares the object.
  //
  let EQUAL_FUNCTION = Symbol("SchemeJS-EQUAL-FUNCTION");
  exportAPI("EQUAL_FUNCTION", EQUAL_FUNCTION);

  exportAPI("equal", equal);
  defineGlobalSymbol("equal?", equal, { usesDynamicScope: false, dontInline: true });
  function equal(a, b, maxDepth = 10000, maxLength = 10000000, report = {}) {
    if (a === b) return true;
    let stringCompare = report.stringCompare ?? ((a, b) => a === b);
    let NaNsEqual = report.NaNsEqual;
    let res = deep_eq(a, b, 0, 0), originalReport = report;
    if (report !== originalReport)
      originalReport.reason = report;
    return res;
    function deep_eq(a, b, depth, length) {
      if (a === b) return true;
      if (depth > maxDepth) {
        report.maxedOut = report.maxDepth = depth;
        return undefined;
      }
      if (length > maxLength) {
        report.maxedOut = report.maxLength = length;
        return undefined;
      }
      // Any object or class can have an EQUAL_FUNCTION
      // The EQUAL_FUNCTION can deem objects of different types equal if it chooses,
      // So this precedes the type check.
      let equalFunction = (a != null && a[EQUAL_FUNCTION]) ?? (b != null && b[EQUAL_FUNCTION]);
      if (equalFunction) {
        let res = equalFunction(a, b);
        // If nullish, continue with other tests!
        if (res != null) {
          report.a = a, report.b = b;
          report.equalFunction = equalFunction;
          return res;
        }
      }
      if (typeof a !== typeof b) {
        report.a = a, report.b = b;
        report.typesDiffer = true;
        return false;
      }
      // Both types same now so no need to test typeof b
      if (typeof a === 'string') {
        let res = stringCompare(a, b);
        if (!res) {
          report.a = a, report.b = b;
          report.stringsDiffer = true;
        }
        return res;
      }
      // Normally NaNs are not equal to anything, but we can opt that they are
      if (typeof a === 'number' && NaNsEqual && isNaN(a) && isNaN(b))
        return true;
      if ((a == null || b == null) || typeof a !== 'object') { // this includes Functions, which are strangely not 'object'
        let res = a === b;
        if (!res) {
          report.a = a, report.b = b;
          report.valuesDiffer = true;
        }
        return res;
      }
      if (iterateAsList(a)) {
        if (!iterateAsList(b)) {
          report.a = a, report.b = b;
          report.valuesDiffer = true;
          return false;
        }
        let i = 0, aRest = a, bRest = b;
        for ( ; moreList(aRest) && moreList(bRest); ++i, ++length, aRest = aRest[REST], bRest = bRest[REST]) {
          let res = deep_eq(aRest[FIRST], bRest[FIRST], depth+1, length);
          if (!res) {
            report.list = a, report.a = aRest[FIRST], report.b = bRest[FIRST];
            report.elementsDiffer = i;
            report = { reason: report };
            return res;
          }
        }
        if (isNil(aRest) && isNil(bRest))
          return true;
        return deep_eq(aRest, bRest, depth+1, length+1);
      } else if (iterateAsList(b)) {
        report.a = a, report.b = b;
        report.valuesDiffer = true;
        return false;
      }
      if (Object.getPrototypeOf(a) !== Object.getPrototypeOf(b)) {
        report.a = a, report.b = b;
        report.prototypesDiffer = true;
        return false;
      }
      // Since the protos are the same; if either is an array, both are.
      // But I might change my mind about the prototype check, so leave the additional
      // tests in for now.
      if (isArray(a)) {
        if (a.length != b.length) {
          report.a = a, report.b = b;
          report.aVal = a.length, report.bVal = b.length;
          report.property = 'length';
          report.valuesDiffer = true;
          return false;
        }
        // Array elements are also properties so fall through to Object comparison.
        // This also distinguishes Arrays that have missing elements.
      }
      // Object compare, symbols first (since Scheme is symbol-oriented), then names.
      let res = compareProps(Object.getOwnPropertySymbols(a), Object.getOwnPropertySymbols(b));
      if (!res) return res;
      return compareProps(Object.getOwnPropertyNames(a), Object.getOwnPropertyNames(b));

      function compareProps(aProps, bProps) {
        for (let property of aProps) {
          if (!b.hasOwnProperty(property)) {
            report.a = a, report.b = b;
            report.aVal = a[property];
            report.bMissingProperty = property;
            report = { reason: report };
            return false;
          }
        }
        for (let property of bProps) {
          if (!a.hasOwnProperty(property)) {
            report.a = a, report.b = b;
            report.bVal = b[property];
            report.aMissingProperty = property;
            report = { reason: report };
            return false;
          }
          let res = deep_eq(a[property], b[property], depth+1, length);
          if (!res) {
            report.a = a, report.b = b;
            report.aVal = a[property], report.bVal = b[property];
            report.elementsDiffer = property;
            report.valuesDiffer = true;
            report = { reason: report };
            return res;
          }
        }
        return true;
      }
    }
  }

  defineGlobalSymbol("=", scheme_eq, { evalArgs: 2, compileHook: scheme_eq_hook, group: "compare-op" });
  function scheme_eq(a, b, ...rest) {
    let i = 0, restLength = rest.length;
    for (;;) {
      if (!equal(a, b)) return false;
      if (i >= restLength) break;
      a = b;
      b = _eval(rest[i++], this);
    }
    return true;
  }
  function scheme_eq_hook(args, ssaScope, tools) {
    if (args.length < 2) return 'true';
    tools.use(tools.bind(equal, "equal"));
    return compare_hooks(args, ssaScope, tools, 'equal(A, B)', 'scheme_eq');
  }

  return globalScope;

}