(let (fac 10)
  (fac (lambda (n)
            (if (eq n 0)
                1
                (* n (fac (- n 1)))))))