
(defun lslice (l from to)
  "Return a slice of list L, beginning at FROM, ending just before TO.
If one or both of the indexes are outside of the list, return a shorter
list as appropriate."
  (let ((result '())
        (index 0))
    (while (and l (< index from))
      (setq l (cdr l))
      (setq index (1+ index)))
    (while (and l (< index to))
      (setq result (cons (car l) result))
      (setq l (cdr l))
      (setq index (1+ index)))
    (nreverse result)))

(defun ack (m n)
  "Calculate the Ackermann function of M and N."
  (if (zerop m)
      (1+ n)
    (if (zerop n)
        (ack (1- m) 1)
      (ack (1- m) (ack m (1- n))))))

(defun fib (n)
  "Return the Fibonacci number N."
  (if (< n 2)
      n
    (+ (fib (- n 1))
       (fib (- n 2)))))

; (defun mapcar (fun l)
;   (if (null l)
;       ()
;     (cons (fun (car l))
;           (mapcar fun (cdr l)))))

(defun *3 (n)
  "Return the triple of the number N."
  (* 3 n))

(defun nconc (l1 l2)
  "Append lists L1 and L2 and return the result. L1 is modified in the process."
  (if (null l1)
      l2
    (if (null (cdr l1))
        (rplacd l1 l2)
      (cons (car l1)
            (nconc (cdr l1) l2)))))

;;; this is obsolete now that defspecial has been replaced by a real
;;; macro facility
;; (defspecial lcond (&rest clauses)
;;   "For CLAUSES of the for (test-form value-form) evaluate test-form,
;; and for the first one that is non-nil, return the value of evaluating
;; value-form."
;;   (let ((not-done t)
;;         (result))
;;     (while (and clauses not-done)
;;       (let ((test-form (car (car clauses)))
;;             (form (car (cdr (car clauses)))))
;;         (if (eval test-form)
;;             (progn (setq not-done nil)
;;                    (setq result (eval form)))
;;           (setq clauses (cdr clauses)))))
;;     result))
