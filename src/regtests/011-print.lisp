(require 'regtests)

(test-is "print string" (print "dubidubidu?" devnull) "dubidubidu?")
(test-is "print symbol" (print 'a devnull) 'a)
(test-is "print number" (print 3.1415926589793 devnull) 3.1415926589793)
(test-is "print cons" (print (cons (cons 3 4) nil) devnull) "((3 . 4))")
(test-is "princs string" (princs "lala2") "lala2")
(test-is "princs number" (princs 3) "3")
(test-is "princs symbol" (princs 'sisismi) 'sisismi)
(test-is "princs cons" (princs (cons (cons 5 6) nil)) "((5 . 6))")

(done-testing)
