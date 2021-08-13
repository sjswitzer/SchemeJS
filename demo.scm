;; SchemeJS Demo

(define (factoral x)
  (? (<= x 1) 
     (? (bigint? x) 1n 1)
     (* x (factoral (- x (? (bigint? x) 1n 1))))
  ))

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

// What it looks like to SchemeJS
factoral

// What it looks like to JavaScript
(println (String factoral))

(factoral 50)
(factoral 50n)

// Compile using conditional predicates
(compile (_factoral x)
  (? (<= x 1) 
     (bigint? x 1n 1)
     (* x (_factoral (- x (bigint? x 1n 1))))
  ))

// What it looks like to SchemeJS
_factoral

// What it looks like to JavaScript
(println (String _factoral))

(_factoral 50)
(_factoral 50n)

// Optional parameters
(define (opt a b (? c (+ 2 3))) (list a b c))
opt
(opt 1 2 3)
(opt 1 2)
(opt 1)  // Note the binding in the closure scope
(println (String (opt 1)))  // Let's see that in JavaScript
((opt 1) 8)

// Rest paramaters
(define (foo a b . c) c)
foo
(foo 1 2 3 4 5 6)

// Let's compile a function with optional parameters
(define (opt a b (? c (+ 2 3))) (list a b c))
// And see what we get
(println (String opt))

// And it still works
opt
(opt 1 2 3)
(opt 1 2)
(opt 1)  // Note the binding in the closure scope
(println (String (opt 1)))  // What's that in JavaScript?
((opt 1) 8)

// Sequential evaluation
(begin (+ 1 2 3 4) (* 1 2 3 4))
(prog1 (+ 1 2 3 4) (* 1 2 3 4))

// "Switch statement"
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
(nth 18 b)
b
(length b)
b

// Partial evaluation binds a closure
//   (define (increment-by n) (\(x) (+ x n))) // You can define it like this
(define (increment-by n) (\x.(+ x n)))        // But I'm using Curry notation here
increment-by
(define increment-by-3 (increment-by 3))
increment-by-3  // Again, note the binding in the closure scope
(println (String increment-by-3)) // And again as JavaScript
(increment-by-3 4)

// Partial evaluation (bound closures) of ordinary builtins
(define times-3 (* 3))
times-3
(println (String times-3))  // You know you wanna
(times-3 5)

// Define some variables used in literals below
(define a "foo")
(define b 20)

// Array literal
[ 1 2 3 4 5 6 7 a 3 b 4]

// Object literal
{ "a": "foo", "b": "bar" [a]: [ 1, 2, 3 ]}

// Indexing Arrays
(@ ['a 'b 'c 'd] 2)

// Accessing elements of Objects
(@ { "a": "foo", "b": "bar"} "a")
(@ { a: "foo", b: "bar"} 'b)

// Let
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