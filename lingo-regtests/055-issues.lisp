(require 'regtests)

;; Bug: string-literal in defun is swallowed even if return value #26
(test-is "swallowed docstring" (progn (defun version () "1.3.29f-14")
                                       (version))
         "1.3.29f-14")

;; Bug: quasiquote misses non-list unquote #25
(defparameter unquoted-var "shoo")

(defmacro my-macro ()
  `,unquoted-var)

(defun use-macro ()
  (my-macro))

(test-is "missed unquote" #'use-macro
         "#<function use-macro () \"shoo\">")

(done-testing)
