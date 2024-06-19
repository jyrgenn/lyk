(defun make-seq (start limit &optional incr)
  "make a list of numbers from START to under LIMIT, &optional INCR (or 1)"
  (setq incr (or incr (if (< start limit)
                          1
                        -1)))
  (let ((result ()))
    (flet ((compare (if (< start limit)
                     #'<
                   #'>)))
      (while (compare start limit)
        (push start result)
        (incf start incr))
      (nreverse result))))
