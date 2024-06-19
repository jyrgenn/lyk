(require 'regtests)

(test-not "progn 0." (progn))
(test-is "progn 1." (progn 3) 3)
(test-is "progn 1a." (progn (car (cdr '(z x y)))) "x")
(test-is "progn 2." (progn (car (cdr '(z x y)))
                           19) "19")
(test-is "progn 3." (progn (car (cdr '(z x y)))
                           19
                           (cons (cons 17 4) nil)) "((17 . 4))")

(done-testing)
