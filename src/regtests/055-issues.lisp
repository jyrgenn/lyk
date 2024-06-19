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

(test-is "missed unquote" (function-definition #'use-macro)
         "(defun use-macro nil \"shoo\")")

;; Bug: subseq handled missing end argument wrong
(test-is "subseq start nil list" (subseq '(3 4 5 6 7 8 9) 3)
         '(6 7 8 9))
(test-is "subseq start nil vec" (subseq #(3 4 5 6 7 8 9) 3)
         #(6 7 8 9))
(test-is "subseq start nil str" (subseq "3456789" 3)
         "6789")

(test-is "subseq start end list" (subseq '(3 4 5 6 7 8 9) 3 5)
         '(6 7))
(test-is "subseq start end vec" (subseq #(3 4 5 6 7 8 9) 3 5)
         #(6 7))
(test-is "subseq start end str" (subseq "3456789" 3 5)
         "67")

;; Bug: lyc threw an error with an empty stack and a command that
;; wanted 0 items from the stack
(test-is "subseq 0 of nil" (subseq nil 0) nil)
(test-is "subseq 0 0 of nil" (subseq nil 0 0) nil)
(test-is "subseq 0 of \"\"" (subseq "" 0) "")
(test-is "subseq 0 0 of \"\"" (subseq "" 0 0) "")
(test-is "subseq 0 of #()" (subseq #() 0) #())
(test-is "subseq 0 0 of #()" (subseq #() 0 0) #())


;; Bug: while evaluates the condition twice due to a botched variable
;; assignment inside bi_while(). In many cases this might not even be
;; detected, but when the condition e.g. includes reading from a
;; stream, like here, it is.

(test-is "while cond eval" (with-input-from-string (s "3 4 +")
                             (let (exp values)
                               (while (setf exp (read s))
                                 (push exp values))
                               values))
         '(+ 4 3))


(done-testing)
