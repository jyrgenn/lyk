;;; Project Euler problem 25

(load "bignum")

(defun eu25 ()
  (let ((f-2 (bign-n2v 1))
        (f-1 (bign-n2v 1))
        f
        (index 2)
        (len 0))
    (while (< len 1000)
      (setf f (bign-add f-1 f-2))      
      (incf index)
      (setf f-2 f-1)
      (setf f-1 f)
      (setf len (bign-len f)))
    index))

;; result 4782
