(require 'regtests)

(defvar l '(3 4 5))
(test-is "push 0" (progn (push (* 3 4) l) l) '(12 3 4 5))
(test-is "push 1" (push (* 3 4) l) '(12 12 3 4 5))

(test-is "pop 0" (pop l) 12)
(test-is "pop 1" l '(12 3 4 5))
(setq l ())
(test-is "pop 2" (pop l) nil)

(done-testing)
