(require 'regtests)

(defun test-this (a b c d)
  (cond ((eq a 'this) (+ 17 4))
        ((= b 110) (string-concat "elf" "zig"))
        ((and (numberp d) (= d 1024)) (* 3 4) nil)
        ((= b c) (* b c))
        ((= c 123) (incf c) (incf c) c)
        (d (error "too eager evaluation"))
        (t "das war sauber")))

(test-is "cond 1" (test-this 'this 4 'nonumber t) "21")
(test-is "cond 2" (test-this 'that 110 110 'gong) "elfzig")
(test-is "cond 3" (test-this 'that 119 119 nil) "14161")
(test-is "cond 4" (progn (errset (test-this 'that 119 118 t)
                                  nil)
                         *last-error*)
         "Error: too eager evaluation")
(test-is "cond 5" (test-this 'that 119 118 nil) "das war sauber")
(test-is "cond 6" (test-this 0 0 123 0) 125)
(test-is "cond 7" (test-this 'that 119 119 1024) nil)

;; CLHS: If there are no forms in that clause, the primary value of
;; the test-form is returned by the cond form. Otherwise, the forms
;; associated with this test-form are evaluated in order, left to
;; right, as an implicit progn, and the values returned by the last
;; form are returned by the cond form.
;; http://clhs.lisp.se/Body/m_cond.htm

(test-is "cond no forms" (cond (10)) 10)

(done-testing)
