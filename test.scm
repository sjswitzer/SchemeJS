;; Jisp tests

(define (factoral x)
  (? (<= x 1) 
     (? (bigint? x) 1n 1)
     (* x (factoral (- x (? (bigint? x) 1n 1))))
  ))

(factoral 100)
(factoral 100n)

(begin (+ 1 2 3 4) (* 1 2 3 4))
(prog1 (+ 1 2 3 4) (* 1 2 3 4))
(cond
  ((< 6 4) '"a" '"b")
  ((< 4 4) '"c" '"d")
  ((< 3 4) '"e" '"f")
  ((< 2 4) '"g" '"h"))

(define (increment-by n) (\ (x) (+ x n)))
(define increment-by-3 (increment-by 3))
(increment-by-3 4)

