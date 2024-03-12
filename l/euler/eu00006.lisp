;; Euler Project Problem 6

(defparameter n-elems 100)

(defun squaresum (n)
  "Sum of the squares of 1..n"
  (let ((sum 0))
    (dotimes (i (1+ n) sum)
      (incf sum (* i i)))))

(defun sumsquare (n)
  "Square of the sum of 1..n"
  (let ((sum 0))
    (dotimes (i (1+ n) (* sum sum))
      (incf sum i))))

(defun eu6 ()
  (- (sumsquare n-elems) (squaresum n-elems)))

;; result 25164150

