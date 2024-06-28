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


(defvar affirmative-words
  (string-split "yes y sure ja j jou si on oui t true  aye 1 affirmative")
  "Words, letters, and a number meaning \"true\" or \"yes\"")

(defvar negative-words
  (string-split "no n  nope nein nee   off non f false nay 0 negative")
  "Words, letters, and a number meaning \"false\" or \"no\"")

(defun boolish (string &optional default)
  "Return t if the `string` is an affirmative word or starts with one;
nil if negative. If it is neither, return `default`."
  (let ((word (string-downcase (car (string-split string)))))
    (cond ((find word affirmative-words) t)
          ((find word negative-words) nil)
          (t default))))

(defun yes-or-no-p (&optional format-control &rest arguments)
  "Ask the user a \"yes or no\" question and return the boolean result."
  (princ (string (apply #'format nil format-control arguments)
                 " (yes or no) "))
  (boolish (read-line *terminal-io*)))

(defun all-symbols (&optional comparison-function)
  "Return a list of all symbols, alphabetically sorted.
If optional `comparison-function` is supplied , use it to sort the list.
if `comparison-function` is t, sort the list alphabetically."
  (let (l)
    (do-symbols (sym)
      (push sym l))
    (cond ((null comparison-function) l)
          ((eq comparison-function t) (sort l #'<))
          ((functionp comparison-function) (sort l comparison-function))
          (t (error "all-symbols: comparison-function ~S is not t or a function"
                    comparison-function)))))

(defun function-symbols ()
  "Return a list of all function symbols."
  (filter #'fboundp (all-symbols t)))


(defmacro no-warnings-as-errors (&rest bodyforms)
  "Evaluate `bodyforms` with *warnings-as-errors* set to nil.
After that, restore the original value."
  `(let ((*previous-warnings-as-errors* (warnings-as-errors nil)))
     (unwind-protect
          (progn
            ,@bodyforms)
       (warnings-as-errors *previous-warnings-as-errors*))))


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
          
(defun select-string-from-prefix (prefix selections)
  "If `prefix` is a prefix of just one of `selections`, return that one.
Otherwise, return nil if it matches none, a list if it matches more than one."
  (let (matches)
    (dolist (item selections)
      (when (string-starts-with item prefix)
        (push item matches)))
    (cond ((null matches) nil)          ;no match
          ((null (cdr matches))         ;just one?
           (car matches))               ;  so return as an atom
          (t matches))))                ;otherwise, all of them

(defun example-startup-hook-function (load-files expr-list other-args)
  "Print the arguments passed to lyk.
This is an example for a startup hook function."
  (format t "; startup: load ~A -e ~A args ~A~%"
          load-files expr-list other-args))

;; (set-hook-function '*startup-hook* #'example-startup-hook-function)
                   

;;; EOF
