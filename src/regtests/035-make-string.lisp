(require 'regtests)

(test-is "make-string 10" (make-string 10) "          ")
(test-is "make-string init" (make-string 13 "Bye! ")
         "Bye! Bye! Bye")
(test-is "make-string empty" (make-string 0) "")

(test-is "make-string func 1" (make-string 12 (let ((n 1))
                                                 (lambda ()
                                                   (incf n))))
         "234567891111")
(test-err "make-string func \"\"" (make-string 12 (lambda () ""))
         #/initializer function .* returns an empty string/)

(done-testing)
