;;; fill buckets with random numbers and see if these are equally filled

(apply #'format
       (nconc (list t "evals %d pairs %d seconds %.4f\n")
              (collect-performance-data
               (setq nbuckets 40)
               (setq buckets (make-vector nbuckets 0))
               (setq nevents 10000000)
 
               (let ((count 0))
                 (while (< count nevents)
                   (let* ((bucket (random nbuckets t))
                          (bcount (vector-get buckets bucket)))
                     (vector-set buckets bucket (1+ bcount))
                     (incf count))))
 
               (let ((count 0)
                     (expect (/ nevents nbuckets)))
                 (while (< count nbuckets)
                   (let ((value (vector-get buckets count)))
                     (format t "%2d: %d %.3f\n" count value (/ value expect)))
                   (incf count)))
               )))
