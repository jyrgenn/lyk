
(defun fac (n)
  (let ((acc 1))
    (loop
     (setf acc (* acc n))
     (decf n)
     (when (<= n 1)
       (return acc)))))
