(require 'regtests)

(test-is "run-program blablabla" (let ((out (make-string-output-stream))
                                       (command "echo lalala | sed s/l/bl/g"))
                                   (run-program command :output out)
                                   (get-output-stream-string out))
         "blablabla\n")

(done-testing)
