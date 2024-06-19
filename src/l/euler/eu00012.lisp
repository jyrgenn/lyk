;;; Project Euler problem 12

(defun divisors (n)
  "Return a list of all divisors of N."
  (let (divs)
    (for (i 1 n 1 <=)
      (when (zerop (mod n i))
        (push i divs)))
    (nreverse divs)))

(defun eu12 ()
  (let ((n 1)
        (sum 0)
        (running t)
        (lastlen 0))
    (while running
      (incf sum n)
      ;; (format t "%d: %d %s\n" n sum (divisors sum))
      (let ((len (length (divisors sum))))
        (when (> len lastlen)
          (setf lastlen len)
          (print len)
          (when (> len 500)
            (print sum)
            (setf running nil))))
      (incf n))
    sum))

;; result 76576500
