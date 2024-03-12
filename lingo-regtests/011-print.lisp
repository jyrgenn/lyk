(require 'regtests)

(test-is "print string" (print "dubidubidu?") "dubidubidu?")
(test-is "print symbol" (print 'a) 'a)
(test-is "print number" (print 3.1415926589793) 3.1415926589793)
(test-is "print cons" (print (cons (cons 3 4) nil)) "((3 . 4))")
(test-is "print-to-string string" (print-to-string "lala2") "\"lala2\"")
(test-is "print-to-string number" (print-to-string 3) "3")
(test-is "print-to-string symbol" (print-to-string 'sisismi) 'sisismi)
(test-is "print-to-string cons" (print-to-string (cons (cons 5 6) nil)) "((5 . 6))")

(done-testing)
