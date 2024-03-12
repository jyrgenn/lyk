(defun all-values (symbols)
  (if (null symbols)
      nil
    (let ((sym (car symbols))
          (others (all-values (cdr symbols))))
      (if (boundp sym)
          (cons (cons sym (eval sym))
                others)
        others))))
