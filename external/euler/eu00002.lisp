;; Project Euler problem 2

(defun eu2 ()
  (let ((sum 0)
        (fib0 1)
        (fib1 2))
    (while (<= fib1 4000000)
      (when (evenp fib1)
        (incf sum fib1))
      (let ((next (+ fib0 fib1)))
        (setf fib0 fib1)
        (setf fib1 next)))
    sum))

;; result 4613732
