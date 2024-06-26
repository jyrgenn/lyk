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

(load "backquote")
(test-is "test-run-program" (backquote "scripts/test-run-program.sh")
         (list 0 "o2:lulu liebt sich
o3:huhu hält sich
o5:gaga gibt sich
" "e1:lala legt sich
e4:momo müht sich
"))

(defvar random-tag (string 'VT (random 1000000000 t)))
(test-is "run-program env TERM" (let ((env (process-env))
                                      (out (make-string-output-stream)))
                                  (table-put env "TERM" random-tag)
                                  (run-program "echo $TERM"
                                               :env env
                                               :output out)
                                  (string-trim t
                                               (get-output-stream-string out)))
         random-tag)

(done-testing)
