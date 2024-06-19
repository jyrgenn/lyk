;;; Project Euler problem 21

(defun divisors (n)
  "Return a list of all divisors of N."
  (let (divs)
    (for (i 1 (/ n 2) 1 <=)
      (when (zerop (mod n i))
        (push i divs)))
    (nreverse divs)))

(defun divs-sum (n)
  (apply #'+ (divisors n)))

(defparameter sums #:()
  "sums by candidate -- n => sum")

(defparameter amicable-numbers '())

(defun make-entry (n sum)
  (let ((cand-entries (or (cand sum) '())))
    (cand sum (cons n cand-entries)))
  (sums n sum))

(defun eu21 ()
  (for (i 1 10000)
    (sums i (divs-sum i)))
  ;; (print sums)
  (dolist (n (table-keys sums))
    (let* ((sum (sums n))
           (sumsum (sums sum)))
      (when (and sumsum
                 (/= n sum)
                 (= n sumsum))
        ;; need to store only one; the other one gets around by its own
        (push n amicable-numbers))))
  (apply #'+ amicable-numbers))

;; result 31626
