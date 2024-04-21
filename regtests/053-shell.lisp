(require 'regtests)

(test-is "run-program blablabla" (let ((out (make-string-output-stream))
                                       (command "echo lalala | sed s/l/bl/g"))
                                   (run-program command :output out)
                                   (get-output-stream-string out))
         "blablabla\n")

(test-is "run-program mutiline" (let ((out (make-string-output-stream))
                                      (command "echo lalala | sed s/l/bl/g;
                                                printf humpti"))
                                  (run-program command
                                               :output out
                                               :raise-error t)
                                  (get-output-stream-string out))
         "blablabla\nhumpti")

(test-err "not in shell" (run-program "exit 15")
          #/IOError: .* Cannot run program/)

(test-is "run-program error 1" (run-program "exit 15" :in-shell t)
         15)

(test-err "run-program error 2" (run-program "exit 15" :in-shell t
                                             :raise-error t)
         #/command exit status 15/)

(test-is "run-program signal" (run-program "kill -15 $$" :in-shell t)
         143)


(done-testing)
