;;; print a collection of all function and macro docstrings

(format t ";;; All function docstrings for ~%;;; ~A~%~%" (build-info t))

(dolist (f (function-symbols))
  (if (fboundp f)
      (let* ((docstring (doc f t))
             (lines (string-split (string-trim t docstring) "\n"))
             (md-lines (join lines "  \n")))
        (format t "`~A`  ~%~A~%~%" f md-lines))))

;; (let ((funsyms (filter #'fboundp (sort (all-symbols) #'<))))
;;   (println (join (map (lambda (f) (string f ":\n" (doc f t)))
;;                       funsyms) "\n")))


