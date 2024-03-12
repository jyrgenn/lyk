;; Euler Project Problem 7

(load "l/factor")

(defun eu7 ()
  (let (p)
    (reset-primes)
    (dotimes (i 10001 p)
      (setf p (next-prime)))))

;; result 104743

