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
          

(defmacro repl-short-command (name docstring &rest bodyforms)
  `(define-short-command ,name
       (lambda () ,docstring
         ,@bodyforms)))


(repl-short-command :replsyms
                    "print the repl's special symbols and their values"
                    (dolist (sym '(* ** *** + ++ +++ / // ///))
                      (format t " ~3@A: ~A~%" sym (symbol-value sym))))

(defvar exploring-help
  "There are a few tools to help explore lyk:

  - `apropos` prints all known symbols that match a substring or a
    regular expression, together with the information if the symbol
    is bound to a function (builtin, lambda, or macro), has a
    variable binding, or properties. Call it like this:

        (apropos \"substring\") or (apropos #/regexp/)

  - `doc` prints a function documentation for a symbol (or a
    function object) with a synopsis, description of the function,
    and the place where it is defined. Example:

        (doc 'apropos)

  - `describe` return a alist with an object's (the sole argument)
    attributes.

  - The directory `doc/` of the source and installation directories
    contains a few texts describing some aspects of lyk.

  - `DOCSTRINGS.md` is built in the `generated/` subdirectory of the
    source repository and installed in the `doc/` subdirectory of
    the installation directory. It contains the docstrings of all
    functions, same as those printed interactively with

        (doc 'function-name)
")

(repl-short-command :explore
                    "show hints for exploring the system"
                    (print exploring-help))
                    
