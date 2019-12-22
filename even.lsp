(let
  (odd? 37 even? odd?)
  (even? (lambda (n even? odd?) (if (eq 0 n) true (odd? (- n 1) even? odd?))))
  (odd? (lambda (n even? odd?) (if (eq 0 n) false (even? (- n 1) even? odd?))))
)

(letrec
     (is-odd? 11)
     (is-even? (lambda (n) (or (eq 0 n) (is-odd? (- n 1)))))
     (is-odd? (lambda (n) (and (not (eq 0 n)) (is-even? (- n 1)))))
)