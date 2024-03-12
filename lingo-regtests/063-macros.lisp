(require 'regtests)

(test-is "dotimes 1" (let ((sum 0))
                       (dotimes (i 11 sum)
                         (incf sum i)))
         55)
(test-is "dotimes 2" (let ((sum 0))
                       (dotimes (i (+ 7 4) (* sum sum))
                         (incf sum i)))
         3025)

(test-is "dotimes1 1" (let ((sum 0))
                        (dotimes1 (i 10 sum)
                          (incf sum i)))
         55)
(test-is "dotimes1 2" (let ((sum 0))
                        (dotimes1 (i (+ 7 3) (* sum sum))
                          (incf sum i)))
         3025)

;; first with defaults...
(test-is "for 1" (let ((sum 0))
                   (for (i (- 2 2) (+ 5 6))
                     (incf sum i))
                   sum)
         55)

;; ...then without
(test-is "for 2" (let ((sum 0))
                   (for (i (+ 5 5) (- 2 2) (- 1) >)
                     (incf sum i))
                   sum)
         55)


(done-testing)
