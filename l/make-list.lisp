
(format t "Constant:\n")
(dolist (el '(1 2 3 4 5))
  (apply #'format
         (nconc (list t "make-list-a: evals %d pairs %d seconds %.4f\n")
                (collect-performance-data
                 (null (make-list-a 100000 t)))))
  (apply #'format
         (nconc (list t "make-list-b: evals %d pairs %d seconds %.4f\n")
                (collect-performance-data
                 (null (make-list-b 100000 t)))))
  )
(format t "Function:\n")
(dolist (el '(1 2 3 4 5))
  (apply #'format
         (nconc (list t "make-list-a: evals %d pairs %d seconds %.4f\n")
                (collect-performance-data
                 (null (make-list-a 100000 (lambda () n))))))
  (apply #'format
         (nconc (list t "make-list-b: evals %d pairs %d seconds %.4f\n")
                (collect-performance-data
                 (null (make-list-b 100000 (lambda () n))))))
  )
