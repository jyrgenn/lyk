(require 'regtests)

(defun opt1 (&optional laber fasel)
  (list laber fasel))
(test-is "&optional 1" (opt1) "(nil nil)")
(test-is "&optional 2" (opt1 3) "(3 nil)")
(test-is "&optional 3" (opt1 3 4) "(3 4)")
(test-err "&optional 4" (opt1 3 4 5)
          #/too many args for lambda function/)

(defun opt2 (blubber &optional laber fasel)
  (list blubber laber fasel))
(test-err "&optional 5" (opt2)
          #/too few args for lambda function/)
(test-is "&optional 6" (opt2 5) "(5 nil nil)")
(test-is "&optional 7" (opt2 5 6) "(5 6 nil)")
(test-is "&optional 8" (opt2 5 6 7) "(5 6 7)")
(test-err "&optional 9" (opt2 5 6 7 8)
          #/too many args for lambda function/)

(defun opt3 (blubber &optional laber fasel &rest noch)
  (list blubber laber fasel noch))
(test-err "&rest 1" (opt2)
          #/too few args for lambda function/)
(test-is "&rest 2" (opt3 5) "(5 nil nil nil)")
(test-is "&rest 3" (opt3 5 6) "(5 6 nil nil)")
(test-is "&rest 4" (opt3 5 6 7) "(5 6 7 nil)")
(test-is "&rest 5" (opt3 5 6 7 8) "(5 6 7 (8))")
(test-is "&rest 6" (opt3 5 6 7 8 9) "(5 6 7 (8 9))")

(defun opt4 (blubber &rest noch)
  (list blubber noch))
(test-err "&rest 7" (opt2)
          #/too few args for lambda function/)
(test-is "&rest 8" (opt4 5) "(5 nil)")
(test-is "&rest 9" (opt4 5 6) "(5 (6))")
(test-is "&rest 10" (opt4 5 6 7) "(5 (6 7))")

(defun opt5 (&rest noch)
  noch)
(test-is "&rest 11" (opt5) nil)
(test-is "&rest 12" (opt5 5) "(5)")
(test-is "&rest 13" (opt5 5 6) "(5 6)")

;; test that init-forms of &optional parameters are evaluated at the
;; right time and in the right environment

(defparameter two 2)

(defun opt-param (param1 &optional (param2 two))
  (list param1 param2))

(test-is "init-form 1" (opt-param 1) '(1 2))
(test-is "init-form 2" (opt-param 17 18) '(17 18))

(setf two 3)
(test-is "init-form 3" (opt-param 1) '(1 3))
(setf two 4)
(test-is "init-form 4" (let ((two "zwei"))
                         (opt-param 3))
         '(3 4))
(defun opt-param2 (param1 &optional (param2 (cons two 119)))
  (format nil "param1 %s param2 %s" param1 param2))

(test-is "init-form5" (opt-param2 3) "param1 3 param2 (4 . 119)")

(setf two 55)
(test-is "init-form6" (opt-param2 4) "param1 4 param2 (55 . 119)")


(done-testing)
