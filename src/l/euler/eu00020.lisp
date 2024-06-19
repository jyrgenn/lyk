;;; Project Euler problem 20

(load "bignum")

(defun eu20 ()
  (apply #'+ (elements (bign-fact 100))))

;; result 648
