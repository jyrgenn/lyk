;; Project Euler problem 5

;;; lcm and seq functions now in pre/numeric.lisp
;;
;; (defun lcm (&rest args)
;;   "Return the least common multiple of all arguments."
;;   (let* ((f-merge
;;           #'(lambda(f1 f2)
;;               (cond ((null f1) f2)
;;                     ((null f2) f1)
;;                     (t (let ((c1 (car f1))
;;                              (c2 (car f2)))
;;                          (cond ((< c1 c2) (cons c1 (f-merge (cdr f1) f2)))
;;                                ((< c2 c1) (cons c2 (f-merge f1 (cdr f2))))
;;                                (t (cons c1 (f-merge (cdr f1) (cdr f2))))))))))
;;          (flists (mapcar #'factor args))
;;          (merged))
;;     (while flists
;;       (setf merged (f-merge merged (pop flists))))
;;     (apply #'* merged)))
;;
;; (defun seq (start end)
;;   (let* (result
;;          (step (signum (- end start)))
;;          (cmp (if (< step 0) #'>= #'<=)))
;;     (while (cmp start end)
;;       (push start result)
;;       (incf start step))
;;     (nreverse result)))

(defun eu5 ()
  (apply #'lcm (seq 1 20)))

;; result 232792560

