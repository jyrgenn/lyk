;;; print a collection of all function and macro docstrings

(let* ((bi (build-info)))
  (flet ((values-from-alist (alist &rest tags)
           (let ((lc (list-collector)))
             (dolist (tag tags (lc))
               (lc (cdr (assoc tag alist)))))))
    (funcall #'format t ";;; All function docstrings for ~%;;; ~A~%~%"
             (join (values-from-alist (build-info)
                                      'program 'version 'kotlin 'built-by)))))

(dolist (f (function-symbols))
  (if (fboundp f)
      (let* ((docstring (doc f t))
             (lines (string-split (string-trim t docstring) "\n")))
        (setf (car lines) (regexp-replace #/(\(.*)/ (car lines) "`$1`" 1))
        (format t "`~A`  ~%~A~%~%" f (join lines "  \n")))))

;; (let ((funsyms (filter #'fboundp (sort (all-symbols) #'<))))
;;   (println (join (map (lambda (f) (string f ":\n" (doc f t)))
;;                       funsyms) "\n")))


