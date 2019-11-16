(let (fib 3)
    (fib (lambda (n)
        (if (eq n 0)
            0
            (if (eq n 1)
                1
                (+ (fib (- n 1)
                   (fib (- n 2)))))))))