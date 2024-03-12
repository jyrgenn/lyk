;;; return a string containing all function doc strings

(print (join "\n" (map (lambda (f)
                         (string-concat f ":\n" (doc f nil t) "\n"))
                       (filter #'fboundp (sys:symbols)))))
(terpri)


