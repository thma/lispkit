(letrec
     (is-odd? 11)
     (is-even? (lambda (n) (or (eq 0 n) (is-odd? (- n 1)))))
     (is-odd? (lambda (n) (and (not (eq 0 n)) (is-even? (- n 1)))))
)