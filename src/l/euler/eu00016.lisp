;;; Project Euler problem 16

(load "bignum")

(defun eu16 ()
  (let ((zwerg (bign-n2v 1)))
    (dotimes (i 1000 zwerg)
      (setf zwerg (bign-add zwerg zwerg)))
    (apply #'+ (elements zwerg))))

;;; result 1366
