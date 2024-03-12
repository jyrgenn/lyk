;;; a pretty-printer (to be)

(defun pp (expr &optional (indent-width 0))
  (princ (make-string indent-width " "))
  (let ((where-are-we indent-width))
    (cond ((null expr) (princ "()"))
          ((symbolp expr) (princ expr))
          
          (t (princ expr))))) 
