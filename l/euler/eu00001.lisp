;; Project Euler problem 1

(defun eu1 ()
  (let ((sum 0))
    (dotimes (n 1000 sum)
      (when (or (zerop (% n 3))
                (zerop (% n 5)))
        (incf sum n)))))

;; result 233168
