(let
  (fac x)
  (x 5)
  (fac (lambda (n) 
    (if (eq n 0) 
        1 
        (* n (fac (- n 1))))))
)

(let
  (odd? 37 even? odd?)
  (even? (lambda (n even? odd?) (if (eq 0 n) true (odd? (- n 1) even? odd?))))
  (odd? (lambda (n even? odd?) (if (eq 0 n) false (even? (- n 1) even? odd?))))
)

(let
  (test x)
  (x 7)
  (y 9)
  (z (lambda (b) (+ b b)))
  (test (lambda (c) (+ c c)))
)
