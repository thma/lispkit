(let (sum '(1 2 3 4 5 6 7))
    (plus (lambda (x) (lambda (y) (+ x y))))
    (sum (lambda (z) (foldright plus 0 z))) 
    (foldright (lambda (f b xs) 
        (if (null xs)
            b
            (f (car xs) (foldright f b (cdr xs)))))))