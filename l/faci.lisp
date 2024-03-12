;; this naive recursive implementation is actually the fastest:

(defun fac (n)
  "Return the faculty of N."
  (if (zerop n)
      1
    (* n (fac (1- n)))))
;; 502 pairs 806 evals in 1 ms, 541224 evals/s

;; all others are slightly slower:

(defun fact (n)
  "Return the faculty of N."
  (flet ((helper (lambda (n acc)
                   (if (<= n 1)
                       acc
                     (helper (1- n) (* acc n))))))
    (helper n 1)))
;; 717 pairs 1002 evals in 0 ms, 2002286 evals/s

(defun faci (n)
  "Return the faculty of N."
  (let ((prod 1))
    (while (> n 1)
      (setq prod (* prod n))
      (decf n))
    prod))
;; 498 pairs 999 evals in 0 ms, 1712596 evals/s


(defun facl (n)
  (let ((lvar n)
        (number-list ()))
    (while (> lvar 1)
      (push lvar number-list)
      (decf lvar))
    (apply #'* number-list)))
;; 599 pairs 1299 evals in 0 ms, 1595263 evals/s

(defun facm (n)
  (apply #'* (make-list n (let ((n 0)) (lambda () (incf n))))))
;; 3816 pairs 5119 evals in 6 ms, 799010 evals/s

(defun facs (n)
  (apply #'* (seq 2 n)))
;; 708 pairs 1415 evals in 0 ms, 1908204 evals/s
