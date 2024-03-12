(require 'regtests)

;;; as list-bind is obsolete now with let offering destructuring bind,
;;; we replace it with exactly that -- here, too

(defun aaa (&rest args)
  (let (((a b c) args))
    (list a b c)))

(defun aab (&rest args)
  (let (((a b . c) args))
    (list a b c)))

(test-is "list-bind 1" (let (((a b) '(3 4 5)))
                         (list a b))
         '(3 4))
(test-is "list-bind 1f" (aaa 3 4 5 6) '(3 4 5))

(test-is "list-bind 2" (let (((a b c) '(3 4)))
                         (list a b c))
         '(3 4 nil))
(test-is "list-bind 2f" (aaa 3 4) '(3 4 nil))

(test-is "list-bind 3" (let (((a b . c) '(3 4 5 6)))
                         (list a b c))
         '(3 4 (5 6)))
(test-is "list-bind 3f" (aab 3 4 5 6) '(3 4 (5 6)))
(test-is "list-bind 3g" (aab 3 4 5) '(3 4 (5)))
(test-is "list-bind 3h" (aab 3 4) '(3 4 nil))

(done-testing)
