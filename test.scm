;; Jisp tests

(define (factoral x)
  (? (<= x 1) 
     (? (bigint? x) 1n 1)
     (* x (factoral (- x (? (bigint? x) 1n 1))))
  ))

(factoral 100)
(factoral 100n)
