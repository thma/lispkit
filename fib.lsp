(let (fib 5)
    (fib (lambda (n)
            (if (leq n 0)
                0
                (if (leq n 1)
                    1
                    (+ (fib (- n 1)
                       (fib (- n 2)))))))))

(let (test 3)
    (test (lambda (n)
            (if (leq n 0)
                0
                (+
                    (test (- n 2))
                    (test (- n 1)))))))