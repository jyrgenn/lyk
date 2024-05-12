;;; return a string containing all function doc strings

(let ((funsyms (filter #'fboundp (sort (all-symbols) #'<))))
  (println (join (map (lambda (f) (string f ":\n" (doc f t)))
                      funsyms) "\n")))


