;; function is_prime(n : integer)
;;     if n ≤ 1
;;         return false
;;     else if n ≤ 3
;;         return true
;;     else if n mod 2 = 0 or n mod 3 = 0
;;         return false
;;     let i ← 5
;;     while i×i ≤ n
;;         if n mod i = 0 or n mod (i + 2) = 0
;;             return false
;;         i ← i + 6
;;     return true

(defun primep (n)
  (cond ((<= n 1) nil)
        ((<= n 3) t)
        ((or (zerop (% n 2)) (zerop (% n 3))) nil)
        (t (let ((i 5)
                 (maybe t))
             (while (and maybe (<= (* i i) n))
               (if (or (zerop (% n i)) (zerop (% n (+ i 2))))
                   (setf maybe nil)
                 (incf i 6)))
             maybe))))
