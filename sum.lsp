(let (sum '())
    (plus (lambda (x y) (+ x y)))

    (foldright (lambda (f b xs) 
        (if (null xs)
            b
            (f (car xs) (foldright f b (cdr xs))))))
    (sum (lambda (z) (foldright plus 0 z))))