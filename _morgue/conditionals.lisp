
;; (defmacro unless (condition &rest bodyforms)
;;   "If `condition` yields nil, eval `bodyforms` and return the result of the last."
;;   (cond ((not bodyforms) (setq bodyforms '(nil))))      
;;   `(cond (,condition nil)
;;          (t ,@bodyforms)))

;; (defmacro when (condition &rest bodyforms)
;;   "If `condition` yields true, eval `bodyforms` and return the result of the last."
;;   (cond ((not bodyforms) (setq bodyforms '(nil))))      
;;   `(cond (,condition ,@bodyforms)))

;; (defmacro if (condition then-clause &rest else-clauses)
;;   "If `condition` evals to non-nil, eval `then-clause` and return the value.
;; Otherwise, evaluate `else-clauses` and return the last value."
;;   (cond ((not else-clauses) (setq else-clauses '(nil))))      
;;   `(cond (,condition ,then-clause)
;;          (t ,@else-clauses)))
