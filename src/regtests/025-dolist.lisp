(require 'regtests)

(test-is "dolist 0"
         (let (result)
           (dolist (el '(a b c d e f g h i) (nreverse result))
             (push el result)))
         '(a b c d e f g h i))

;; deleted ones: dolist in CL has no start/end

(test-is "doseq 10"
         (let (result)
           (doseq (el "abcdefghi" (apply #'string (nreverse result)))
                  (push el result)))
         "abcdefghi")
(test-is "doseq 11"
         (let (result)
           (doseq (el "abcdefghi" (apply #'string (nreverse result)) 3)
                  (push el result)))
         "defghi")
(test-is "doseq 12"
         (let (result)
           (doseq (el "abcdefghi" (apply #'string (nreverse result)) 3 6)
                  (push el result)))
         "def")

(done-testing)
