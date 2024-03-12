;;; Project Euler problem 10

(load "l/factor")

(defun eu10 ()
  (let ((running t)
        (count 0)
        (sum 0))
    (reset-primes)
    (while running
      (let ((p (next-prime)))
        (incf count)
        (when (zerop (mod count 100))
          (print p))
        (if (< p 2000000)
            (incf sum p)
          (setf running nil))))
    sum))

;; result 142913828922
