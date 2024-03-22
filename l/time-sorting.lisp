(defvar runs 10)
(defvar leng 1000)

(defvar seql (make-list leng #'random))
(defvar seqs (make-string leng "abcdefghijklmnopq"))
(defvar seqv (apply #'vector seql))

(defun measure (funcsym datasym)
  (let ((n runs)
        (func (symbol-function funcsym))
        (data (eval datasym)))
    (apply #'format
           (nconc
            (list t
                  (format nil "\n%d * %d (%s %s) %%d evals %%d pairs %%.4f s\n"
                          runs (length data) funcsym datasym))
            (collect-performance-data
             (while (> n 0)
               (format t ".")
               (func data '<)
               (decf n)))
            ))))

(measure 'sort 'seql)
(measure 'sort 'seqs)
(measure 'sort 'seqv)
