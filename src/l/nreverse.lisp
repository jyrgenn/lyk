;;; nreverse is a builtin, but still

(defun nreverse (l)
  "Reverse a list by modifying the pairs and return the list."
  (let (res)
    (while l
      (let ((p l))
        (setf l (cdr l))
        (setf (cdr p) res)
        (setf res p)))
    res))

