;;; print a collection of all function and macro docstrings

(format t ";;; All function docstrings for ~%;;; ~A~%~%" (build-info t))

(dolist (f (function-symbols))
  (if (fboundp f)
      (format t "~A:~%~A~%" f (doc f t))))

;; (let ((funsyms (filter #'fboundp (sort (all-symbols) #'<))))
;;   (println (join (map (lambda (f) (string f ":\n" (doc f t)))
;;                       funsyms) "\n")))


