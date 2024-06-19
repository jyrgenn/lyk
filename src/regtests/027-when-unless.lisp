(require 'regtests)

(defun false ()
  nil)

(defun true ()
  t)

(test-is "when 0" (when (false) (+ 13 14) (* 15 16) 332) nil)
(test-is "when 1" (when (true) (+ 13 14) (* 15 16) 332) 332)
(test-is "when 2" (let (a) (when (true) (setq a 115) 19) a) 115)
(test-is "unless 0" (unless (false) (+ 13 14) (* 15 16) 332) 332)
(test-is "unless 1" (unless (true) (+ 13 14) (* 15 16) 332) nil)
(test-is "unless 2" (let (a) (unless (false) (setq a 115) 19) a) 115)

(done-testing)
