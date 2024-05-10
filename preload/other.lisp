;; smallish things that don't belong elsewhere

(defun arg-number-is-one (arg)
  "Iff ARG (or the number of its elements) is 1, return true."
  (let ((n (cond ((numberp arg) arg)
                 ((sequencep arg) (length arg)))))
    (= n 1)))
  
(defun plural-s (arg)
  "Return a plural-s or an empty string as appropriate for ARG.
If ARG is a number, return \"s\" if it is zero or greater than 1, but
an empty string if it is 1.
If ARG is a sequence, do the same for the number of it elements."
  (if (arg-number-is-one arg)
      ""
    "s"))

(defun plural-ies (arg)
  "Return a plural \"ies\" or singular \"y\" as appropriate for ARG.
If ARG is a number, return \"ies\" if it is zero or greater than 1, but
\"y\" if it is 1.
If ARG is a sequence, do the same according to the number of it elements."
  (if (arg-number-is-one arg)
      "y"
    "ies"))

(defun function-symbols ()
  "Return a list of all function symbols."
  (filter #'fboundp (all-symbols)))


(defun get-program-output (command &key capture-all input error-output
                                     (raise-error t)
                                     (in-shell t))
  "Run program `command` and return its standard output as a string.
Raise an error if the program returned a non-zero exit status and the
keyword argument :raise-error is true.

If &key argument :capture-all is true, capture the error output,
too, and return a list of exit status, standard output and error output
of the program run as strings."
  (let ((out (make-string-output-stream)))
    (if capture-all
        (let* ((err (make-string-output-stream))
               (status (run-program command
                                    :input input
                                    :output out
                                    :error-output err
                                    :raise-error raise-error
                                    :in-shell in-shell)))
          (list status
                (get-output-stream-string out)
                (get-output-stream-string err)))
        (run-program command
                     :input input
                     :output out
                     :error-output error-output
                     :raise-error raise-error
                     :in-shell in-shell)
        (let ((result (get-output-stream-string out)))
          result))))
          
         
    
