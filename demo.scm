;; SchemeJS tests

(define (factoral x)
  (? (<= x 1) 
     (? (bigint? x) 1n 1)
     (* x (factoral (- x (? (bigint? x) 1n 1))))
  ))
factoral

(factoral 100)
(factoral 100n)
(factoral 171)
(factoral 171n)

// Simpler version using conditional predicate notation
(define (_factoral x)
  (? (<= x 1) 
     (bigint? x 1n 1)
     (* x (_factoral (- x (bigint? x 1n 1))))
  ))

(_factoral 50)
(_factoral 50n)

// Compile it
(compile (factoral x)
  (? (<= x 1) 
     (? (bigint? x) 1n 1)
     (* x (factoral (- x (? (bigint? x) 1n 1))))
  ))
factoral

// JavaScript's String function shows JavaScript's view of the object
(String factoral)

(factoral 50)
(factoral 50n)

// Compile with conditional predicates
(compile (_factoral x)
  (? (<= x 1) 
     (bigint? x 1n 1)
     (* x (factoral (- x (bigint? x 1n 1))))
  ))
(_factoral 50)
(_factoral 50n)

// Optional parameters
(define (opt a b (? c (+ 2 3))) (list a b c))
opt
(opt 1 2 3)
(opt 1 2)
(opt 1)
((opt 1) 8)

// Rest paramaters
(define (foo a b . c) c)
foo
(foo 1 2 3 4 5 6)

(begin (+ 1 2 3 4) (* 1 2 3 4))
(prog1 (+ 1 2 3 4) (* 1 2 3 4))
(cond
  ((< 6 4) "a" "b")
  ((< 4 4) "c" "d")
  ((< 3 4) "e" "f")
  ((< 2 4) "g" "h"))

// Lazy lists
(define a (list-view (apropos)))
a
(nth 5 a)
(nth 15 a)
a
(length a)
a

/*
 * Lazy maps
 */
(define b (lazy-map to-string (apropos)))
b
(nth 8 b)
b
(nth 18 b)
b
(length b)
b

// (define (increment-by n) (\(x) (+ x n)))
(define (increment-by n) (\x.(+ x n)))  // Curry notation is allowed for a single parameter
increment-by
(define increment-by-3 (increment-by 3))
increment-by-3
(increment-by-3 4)

// Parser test; note mix of Scheme and JavaScript literals
'(1 2 3  345  32 345 3245 235 325 325 325 345 3245 32 53245 325 325 325 325 325 325 35 35 353
    () (a b .c ) (a.b)(a b . (c)) (a.2)
    324532 325 35 235 325 325 32 325 6 436
    { foo: 1, bar: 2, baz: 4, sfa: "asd" etc: 234 sadf:23 cwkjc: "sdfsa" kjsfj: "klhsadfhkjsdhfjhsfkaj",
      sadfsahjkasdfksadfhj: 2981347, 2: "a" }
    6436 436 3423 1324 124 235 23 (21342 3535 32 52
       [ 3254 53 23 35 325 5 325 325 325 3245 35] 32452346 )
     3425 464 634 7457 )

// Special Object and Array literals
(define a 2)
[ 1 2 3 4 5 6 7 a 3 4]
'[ 1 2 3 4 5 6 7 a 3 4]
{ foo: 1, bar: a }
'{ foo: 1, bar: a }

// Indexing Arrays
(@ ['a 'b 'c 'd] 2)

// Test "let"
(let ((a 1 2 3)  ;; assigns last value
     (b (+ a 5)))
  (- a b)
  (* a b))  ;; results last value

// SIOD-style throw/catch
(*catch "foo" (+ 2 (* 5 (*throw "foo" "ha!"))))

// JavaScript-style throw/catch
(catch (e (+ "thrown: " e))
  (+ 1 2)
  (+ 3 (throw "ha ha!"))
)