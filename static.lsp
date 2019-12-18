(let 
    (let
        (let 
            (g 5)
            (x 3))
        (g (lambda (z) (* x z))))
    (x 7))


LApp (LAbs "x" (LApp (LAbs "g" (LApp (LAbs "x" (LUnyOp "g" (LInt 5)) [("x", LInt 3)]) []) [("g", LAbs "z" (LBinPrimOp "*" undefined (LVar "x") (LVar "z")) [])]) []) [("x", LInt 7)]) []