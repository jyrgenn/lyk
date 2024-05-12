;;; print a collection of all function and macro docstrings

(format nil ";; %s\n" (build-info t))
(let ((funsyms (filter #'fboundp (sort (all-symbols) #'<))))
  (println (join (map (lambda (f) (string f ":\n" (doc f t)))
                      funsyms) "\n")))


