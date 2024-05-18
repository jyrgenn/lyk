(require 'regtests)

; test if integers are printed as integers

(defun pow (base power)
  (if (zerop power)
      1
    (* base (pow base (1- power)))))

(test-is "nprint 1" (pow 2 32) "4294967296")
(test-is "nprint 2" (format nil "~A" (1+ (pow 2 32))) "4294967297")

(done-testing)
