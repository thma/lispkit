(let
    (a 3 4)
    (a (lambda (n m)
            (if (eq n 0)
                (+ m 1)
                (if (eq m 0)
                    (a (- n 1) 1)
                    (a (- n 1) (a n (- m 1))))))))