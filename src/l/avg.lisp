
(defun avg (l)
  (/ (apply #'+ l) (length l)))

(defun median (l)
  (let* ((len (length l))
         (mid (floor (/ len 2)))
         (sorted (sort l #'<)))
    (if (evenp len)
        (average (elt sorted (1- mid))
                 (elt sorted mid))
      (elt sorted mid))))
