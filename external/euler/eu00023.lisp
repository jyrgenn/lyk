;;; Project Euler problem 23

(defun divisors (n)
  "Return a list of all divisors of N."
  (let ((limit (/ n 2))
        divs)
    (for (i 1 limit 1 <=)
      (when (zerop (mod n i))
        (push i divs)))
    (nreverse divs)))

(defun divs-sum (n)
  (apply #'+ (divisors n)))

(defun abundant-p (n)
  (< n (divs-sum n)))

(defvar abundants-list nil)
(defvar abundants-set #:())

(defun register-abundant (n)
  (push n abundants-list)
  (abundants-set n t))

(defparameter limit 28123)

(defun eu23 ()
  ;; find abundant numbers up to limit
  (dotimes1 (i limit)
    (when (abundant-p i)
      (register-abundant i)))
  (print "have abundants")
  ;; reverse the list so it is smallest first
  (setf abundants-list (nreverse abundants-list))
  (print "abundants sorted")
  ;; now find numbers that are not the sum of two abundant numbers
  (let (not-sums)                       ;numbers that are not sum of two ab.nums
    (dotimes1 (i limit)
      (let ((abs abundants-list)
            (not-done t))
        (while (and not-done abs)
          (let ((ab (car abs)))
            (if (< ab i)
                (if (abundants-set (- i ab))
                    ;; is sum of two abundant numbers
                    (setf not-done nil))
              (push i not-sums)
              (setf not-done nil)))
          (setf abs (cdr abs)))))
    (print "checked numbers up to limit\n")
    (apply #'+ not-sums)))

;; result 4179871
