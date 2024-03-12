
(defvar *memoize-table* (make-table))

(defun memoize (fun &rest args)
  (let* ((key (princs (list fun args))))
    (if (table-exists *memoize-table* key)
        (table-get *memoize-table* key)
      (let ((value (apply fun args)))
        (table-put *memoize-table* key value)
        value))))


(defun fibmem (n)
  (if (< n 2)
      n
    (+ (memoize #'fibmem (- n 2))
       (memoize #'fibmem (1- n)))))

(defun fib (n)
  (if (< n 2)
      n
    (+ (fib (- n 2))
       (fib (1- n)))))

            
