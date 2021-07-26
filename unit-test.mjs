//
// SchemeJS Unit Tests
//
// Copyright 2021 Stan Switzer
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

import * as SchemeJS from './SchemeJS.mjs';

let succeeded = 0, failed = 0, throwOnError = true;;

const globalScope = SchemeJS.createInstance();
let testScope = globalScope;
const setGlobalScope = globalScope._setGlobalScope_test_hook_ || required();
const NIL = globalScope.NIL || required();
const string = globalScope.string || required();
const newScope = globalScope.newScope || required();
const deep_eq = globalScope.deep_eq || required();
const is_cons = globalScope.is_cons || required();
const is_closure = globalScope.is_closure || required();
const EvalError = globalScope.EvalError || required();

function required() { throw "required" }
class TestFailureError extends Error {
  constructor(message, test, result, expected, report) {
    super(`${string(test)}; ${message}: ${string(result)}, expected: ${string(expected)}`);
    this.test = test;
    this.result = result;
    this.expected = expected;
    this.report = report;
  }
}
TestFailureError.prototype.name = "TestFailureError";

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
// This is one way to deliberately materialize an "undefined" value.
EXPECT_ERROR(` (void 1 2 (xyz q)) `, EvalError);

{
  let savedScope = beginTestScope();
  EXPECT(` (define a 2) `, ` 'a `);
  EXPECT(` { [a]: 3 } `, ` '{"2": 3} `)
  endTestScope(savedScope);
}

EXPECT(` (+) `, NaN);
EXPECT(` (+ 1) `, is_closure);
EXPECT(` (+ 1 2) `, 3);
EXPECT(` (+ 1 2 3) `, 6);
EXPECT(` (+ 1n 2n) `, 3n);
EXPECT_ERROR(` (+ 1 2n) `, TypeError);
EXPECT(` (-) `, NaN);  // Sure. Why not?
EXPECT(` (- 3) `, -3);
EXPECT(` (- 3n) `, -3n);
EXPECT(` (- 100 2 5 10) `, 83);
EXPECT(` (*) `, NaN);
EXPECT(` (* 1) `, is_closure);
EXPECT(` (* 1 2) `, 2);
EXPECT(` (* 1 2 3) `, 6);
EXPECT(` (* 1 2 3 4) `, 24);
EXPECT(` (* 300n 200n) `, 60000n);
EXPECT(` (/) `, NaN);
EXPECT(` (/ 5) `, 1/5);
EXPECT(` (/ 0) `, Infinity);
EXPECT(` (/ -1 0) `, -Infinity);
EXPECT(' (/ 3 7) ', 3/7);
EXPECT(' (/ 100000 10 10 10) ', 100);

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
  EXPECT_ERROR(` (factoral 10) `, EvalError);
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
  EXPECT_ERROR(` (factoral 10) `, EvalError);
}

EXPECT(` (&) `, 0);
EXPECT(` (& 76134) `, is_closure);
EXPECT(` (& 0b1001101011 0b1110101011) `, 0b1001101011 & 0b1110101011);
EXPECT(` (& 0b1001101011 0b1110101011 0b11110111101111) `, 0b1001101011 & 0b1110101011 & 0b11110111101111);
EXPECT(` (|) `, 0);
EXPECT(` (| 76134) `, is_closure);
EXPECT(` (| 0b1001101011 0b1110101011) `, 0b1001101011 | 0b1110101011);
EXPECT(` (| 0b1001101011 0b1110101011 0b11110111101111) `, 0b1001101011 | 0b1110101011 | 0b11110111101111);
EXPECT(` (^) `, 0);
EXPECT(` (^ 76134) `, is_closure);
EXPECT(` (^ 0b1001101011 0b1110101011) `, 0b1001101011 ^ 0b1110101011);
EXPECT(` (^ 0b1001101011 0b1110101011 0b11110111101111) `, 0b1001101011 ^ 0b1110101011 ^ 0b11110111101111);

EXPECT(` (<) `, false);
EXPECT(` (< 5) `, is_closure);
EXPECT(` (< 5 3) `, false);
EXPECT(` (< 3 5) `, true);
EXPECT(` (< 3 3) `, false);
EXPECT(` (< 1 2 3 4 5 6) `, true);  // each less than the previous
EXPECT(` (< 1 2 3 4 4 5 6) `, false);
EXPECT(` (< 1 2 3 10 4 5 6) `, false);
EXPECT_ERROR(` (< 1 2 3 4 5 6 (oops!)) `, EvalError);
EXPECT(` (< 1 2 3 4 4 5 6 (oops!)) `, false); // Short-circuits on false
EXPECT(` (< 1 2 3 10 4 5 6 (oops!)) `, false);
EXPECT(` (<=) `, false);
EXPECT(` (<= 5) `, is_closure)
EXPECT(` (<= 5 3) `, false);
EXPECT(` (<= 3 5) `, true);
EXPECT(` (<= 3 3) `, true);
EXPECT(` (<= 1 2 3 4 5 6) `, true);  // each less or equal to than the previous
EXPECT(` (<= 1 2 3 4 4 5 6) `, true);
EXPECT(` (<= 1 2 3 10 4 5 6) `, false);
EXPECT_ERROR(` (<= 1 2 3 4 5 6 (oops!)) `, EvalError);
EXPECT_ERROR(` (<= 1 2 3 4 4 5 6 (oops!)) `, EvalError);
EXPECT(` (< 1 2 3 10 4 5 6 (oops!)) `, false); // Short-circuits on false
EXPECT(` (>) `, false);
EXPECT(` (> 5) `, is_closure);
EXPECT(` (> 5 3) `, true);
EXPECT(` (> 3 5) `, false);
EXPECT(` (> 3 3) `, false);
EXPECT(` (> 6 5 4 3 2 1) `, true);  // each greater than the previous
EXPECT(` (> 6 5 4 4 3 2 1) `, false);
EXPECT(` (> 6 5 4 10 3 2 1) `, false);
EXPECT_ERROR(` (> 6 5 4 3 2 1 (oops!)) `, EvalError);
EXPECT(` (> 6 5 4 10 3 2 1 (oops!)) `, false); // Short-circuits on false
EXPECT(` (> 6 5 4 10 3 2 1 (oops!)) `, false);
EXPECT(` (>=) `, false);
EXPECT(` (>= 5) `, is_closure);
EXPECT(` (>= 5 3) `, true);
EXPECT(` (>= 3 5) `, false);
EXPECT(` (>= 3 3) `, true);
EXPECT(` (>= 6 5 4 3 2 1) `, true);  // each greater than or equal to the previous
EXPECT(` (>= 6 5 4 4 3 2 1) `, true);
EXPECT(` (>= 6 5 4 10 3 2 1) `, false);
EXPECT_ERROR(` (>= 6 5 4 3 2 1 (oops!)) `, EvalError);
EXPECT_ERROR(` (>= 6 5 4 4 3 2 1 (oops!)) `, EvalError);
EXPECT(` (>= 6 5 4 10 3 2 1 (oops!)) `, false); // Short-circuits on false
EXPECT(` (==) `, true);   // nothing is equal to itself
EXPECT(` (== 5) `, is_closure);
EXPECT(` (== 5 3) `, false);
EXPECT(` (== 3 5) `, false);
EXPECT(` (== 3 3) `, true);
EXPECT(` (== 3 3 3 3 3 3) `, true);  // all equal
EXPECT(` (== 3 3 3 3 4 3) `, false); // not all equal
EXPECT_ERROR(` (== 3 3 3 3 3 3 (oops!)) `, EvalError);
EXPECT(` (== 3 3 3 3 4 3 (oops!)) `, false); // Short-circuits on false
EXPECT(` (!=) `, false);  // nothing isn't equal to itself
EXPECT(` (!= 5) `, is_closure);
EXPECT(` (!= 5 3) `, true);
EXPECT(` (!= 3 5) `, true);
EXPECT(` (!= 3 3) `, false);
EXPECT(` (!= 3 3 3 3 3 3) `, false);  // all equal
EXPECT(` (!= 3 3 3 3 4 3) `, true);   // not all equal
EXPECT_ERROR(` (!= 3 3 3 3 3 3 (oops!)) `, EvalError);
EXPECT(` (!= 3 3 3 3 4 3 (oops!)) `, true); // Short-circuits on false
let list1 = `(a b (c d) 2 3)`, list2 = `(1 2 (7 3) x)`;
// TODO: Revist these. Make sure they match SIOD.
// EXPECT(` (== == eq?) `, true);
// EXPECT(` (eq? '${list1} '${list1}) `, false);
// EXPECT(` (equal? '${list1} '${list1}) `, true);
// EXPECT(` (eq? '${list1} '${list2}) `, false);
// EXPECT(` (equal? '${list1} '${list2}) `, false);

EXPECT(` (max) `, undefined);
EXPECT(` (max 5) `, is_closure);
EXPECT(` (max 3 7 9 2 4) `, 9);
EXPECT(` (min) `, undefined);
EXPECT(` (min 5) `, is_closure);
EXPECT(` (min 3 7 9 2 4) `, 2);

EXPECT(` (&&) `, undefined);
EXPECT(` (&& 1) `, 1);
EXPECT(` (&& 1 2) `, 2);
EXPECT(` (&& 1 false 2) `, false);
EXPECT(` (&& 1 false (oops!)) `, false);  // short-circuits
EXPECT_ERROR(` (&& 1 true (oops!)) `, EvalError);
EXPECT(` (||) `, undefined);
EXPECT(` (|| 1) `, 1);
EXPECT(` (|| 1 2) `, 1);
EXPECT(` (|| nil null (void) false 2 3) `, 2); 
// Only false, nil, null, and undefined are false; specifically, 0 and "" are NOT false
EXPECT(` (|| nil null (void) false 0 2 3) `, 0);
EXPECT(` (|| nil null (void) false "" 2 3) `, `""`);
EXPECT(` (|| 5 (oops!)) `, 5);  // short-circuits
EXPECT_ERROR(` (|| nil null (void) false (oops!)) `, EvalError);
EXPECT(` (?) `, undefined); // Why not?
EXPECT(` (? true) `, is_closure);
EXPECT(` (? false) `, is_closure);
EXPECT(` (? true 1) `, is_closure);
EXPECT(` (? false 2) `, is_closure);
EXPECT(` (? true 1 2) `, 1);
EXPECT(` (? false 1 2) `, 2);
EXPECT(` (? true 1 2 (oops!)) `, 1);
EXPECT(` (? false 1 2 (oops!)) `, 2);
EXPECT(` (? true 1 (oops!)) `, 1);
EXPECT_ERROR(` (? false 1 (oops!)) `, EvalError);
EXPECT_ERROR(` (? true (oops!) 2) `, EvalError);
EXPECT(` (? false (oops!) 2) `, 2);
EXPECT_ERROR(` (? (oops!) 1 2) `, EvalError);
EXPECT(` (? (< 3 5) (+ 3 4) (* 3 4)) `, 7);
EXPECT(` (? (> 3 5) (+ 3 4) (* 3 4)) `, 12);

EXPECT(` (begin) `, undefined);
EXPECT(` (begin 1) `, 1);
EXPECT(` (begin 1 2 3) `, 3);
EXPECT(` (begin (+ 3 4) (* 3 4)) `, 12);
EXPECT(` (prog1) `, undefined);
EXPECT(` (prog1 1) `, 1);
EXPECT(` (prog1 1 2 3) `, 1);
EXPECT(` (prog1 (+ 3 4) (* 3 4)) `, 7);

EXPECT(` (cond) `, NIL);
EXPECT_ERROR(` (cond a) `, EvalError);
EXPECT_ERROR(` (cond 1) `, EvalError);
EXPECT_ERROR(` (cond ()) `, EvalError);
EXPECT(` (cond (true) 1) `, NIL);
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
EXPECT(` (last) `, TypeError);
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
EXPECT(` (reverse 'a) `, NIL); // not a list. XXX maybe should be exception?
EXPECT(` (reverse '(a)) `, ` '(a) `);
EXPECT(` (reverse '(a b)) `, ` '(b a) `);
EXPECT(` (reverse '(a b c)) `, ` '(c b a) `);

EXPECT(` (memq) `, NIL);
EXPECT(` (memq 'a) `, is_closure);
EXPECT(` (memq 'a 1) `, NIL);
EXPECT(` (memq 'c '(a b c d e f g)) `, ` '(c d e f g) `);
EXPECT(` (memq 'z '(a b c d e f g)) `, NIL);
EXPECT_ERROR(` (nth) `, TypeError);
EXPECT(` (nth 'a) `, is_closure);
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

EXPECT(` (apropos "c") `, is_cons);  // weak test but gets coverage
EXPECT(` (mapcar) `, NIL);
EXPECT(` (mapcar (lambda (x) (* 2 x)) '(1 2 3)) `, ` '(2 4 6) `);
EXPECT(` (mapcar (lambda (x) (* 2 x)) '[1 2] '(3)) `, ` '(2 4 6) `);
EXPECT(` (mapcar (lambda (x) (* 2 x))) `, NIL);
EXPECT(` (map->array (lambda (x) (* 2 x)) '(1 2) '[3]) `, ` '[2 4 6] `);

EXPECT(` (let ((x 10)
                (y 20))
            (+ x y)) `, 30);

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
          
{
  const toJSname = globalScope.toJSname;
  const testToJSname = name => () => toJSname(name);
  EXPECT(testToJSname("aNormal_name0234"), expectString("_aNormal_name0234"));
  EXPECT(testToJSname("aname%with&/specialChars?"), expectString("_aname$pct_with$and$stroke_specialChars$q"));
  EXPECT(testToJSname("_begins_with_underscore"), expectString("__begins_with_underscore"));
  EXPECT(testToJSname("number?"), expectString("_number$q"));
  EXPECT(testToJSname("&&"), expectString("$and$and"));
  EXPECT(testToJSname("$"), expectString("$cash"));
  EXPECT(testToJSname("?"), expectString("$q"));
}

{
  const analyzeJSFunction = globalScope.analyzeJSFunction;
  const testAnalyze = (fn) => () => analyzeJSFunction(fn);
  EXPECT(testAnalyze(x => x * x),
    { name: '', params: ['x'], restParam: undefined, value: 'x * x',
      body: undefined, printBody: undefined, native: false });
  EXPECT(testAnalyze((x) => x * x),
    { name: '', params: ['x'], restParam: undefined, value: 'x * x',
      body: undefined, printBody: undefined, native: false });
  EXPECT(testAnalyze((x, y) => x * y),
    { name: '', params: ['x', 'y'], restParam: undefined, value: 'x * y',
      body: undefined, printBody: undefined, native: false });
  EXPECT(testAnalyze((x, ...y) => x * y),
    { name: '', params: ['x'], restParam: 'y', value: 'x * y',
      body: undefined, printBody: undefined, native: false });
  EXPECT(testAnalyze((x, y, ...z) => x * y),
    { name: '', params: ['x','y'], restParam: 'z', value: 'x * y',
      body: undefined, printBody: undefined, native: false });
  EXPECT(testAnalyze((...x) => x * x),
    { name: '', params: [], restParam: 'x', value: 'x * x',
      body: undefined, printBody: undefined, native: false });
  EXPECT(testAnalyze((...x) => { let res = x * x; return res }),
    { name: '', params: [], restParam: 'x', value: 'res',
      body: 'let res = x * x;', printBody: undefined, native: false });
  EXPECT(testAnalyze(function (a) { a = 2 * a; return a; }),
    { name: '', params: ['a'], restParam: undefined, value: 'a',
      body: 'a = 2 * a;', printBody: ' { a = 2 * a; return a; }', native: false });
  EXPECT(testAnalyze(function (a, b, c) { a = 2 * a; return a; }),
    { name: '', params: ['a','b','c'], restParam: undefined, value: 'a',
      body: 'a = 2 * a;', printBody: ' { a = 2 * a; return a; }', native: false });
  EXPECT(testAnalyze(function fn(a) { a = 2 * a; return a; }),
    { name: 'fn', params: ['a'], restParam: undefined, value: 'a',
      body: 'a = 2 * a;', printBody: ' { a = 2 * a; return a; }', native: false });
  EXPECT(testAnalyze(function fn(a, b, c) { a = 2 * a; return a; }),
    { name: 'fn', params: ['a','b','c'], restParam: undefined, value: 'a',
      body: 'a = 2 * a;', printBody: ' { a = 2 * a; return a; }', native: false });
  EXPECT(testAnalyze(function (a, ...rest) { return a; }),
    { name: '', params: ['a'], restParam: 'rest', value: 'a',
      body: '', printBody: ' { return a; }', native: false });
  EXPECT(testAnalyze(function (a, b, c, ...rest) { return a; }),
    { name: '', params: ['a','b','c'], restParam: 'rest', value: 'a',
      body: '', printBody: ' { return a; }', native: false });
  EXPECT(testAnalyze(function foo(a, ...rest) { return a; }),
    { name: 'foo', params: ['a'], restParam: 'rest', value: 'a',
      body: '', printBody: ' { return a; }', native: false });
  EXPECT(testAnalyze(function bar(a, b, c, ...rest) { return a; }),
    { name: 'bar', params: ['a','b','c'], restParam: 'rest', value: 'a'
    , body: '', printBody: ' { return a; }', native: false });
  EXPECT(testAnalyze(function baz(...rest) { return a; }),
    { name: 'baz', params: [], restParam: 'rest', value: 'a',
      body: '', printBody: ' { return a; }', native: false });
  EXPECT(testAnalyze(function (...rest) { return a; }),
    { name: '', params: [], restParam: 'rest', value: 'a',
      body: '', printBody: ' { return a; }', native: false });
  EXPECT(testAnalyze([].sort),
    { name: 'sort', params: [], restParam: undefined, value: undefined,
      body: undefined, printBody: ' { [native code] }', native: true });
}

//
// A specialized tiny unit test framework that evaluates SchemeJS expressions
//

if (testScope !== globalScope) throw new Error("Unpaired begin/endTestScope() calls");

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
      result = testScope.eval_string(test);
    else
      testFailed("test is neither function nor string", test, undefined, expected, report);
    try {
      if (typeof expected === 'function') {
        ok = expected.call(testScope, result);
      } else {
        if (typeof expected === 'string')
          expected = testScope.eval_string(expected);
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
        result = testScope.eval_string(test);
      }
      testFailed("expected exception", test, result, expected, report);
    } catch (error) {
      try {
        if (typeof expected === 'string')
          expected = testScope.eval_string(expected);
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

// A string is typically evaluated, so this function lets you expect a string result.
function expectString(expected) {
  return result => result === expected;
}