
(defvar *next-tag* 0)

(defun next-symbol ()
  (prog1
      (intern (format nil "%b" *next-tag*))
    (setq *next-tag* (1+ *next-tag*))))


(defun mktree (depth)
  "Create a binary tree of depth DEPTH."
  (if (zerop depth)
      (next-symbol)
    (cons (mktree (1- depth))
          (mktree (1- depth)))))
