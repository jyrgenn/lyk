(require 'regtests)



(let ((checks '((-43 2) (-1 2) (0 2) (1 2) (2 3) (3 5) (19 23) (20 23)
                (21 23) (22 23) (23 29) (100000000000 100000000003))))
  (doseq (check checks)
    (defparameter next-prime-base (car check))     ;why, oh why?
    (let ((goal (car (cdr check))))
      (test-is (format nil "next-prime %d" next-prime-base)
               (next-prime next-prime-base) goal))))

(test-is "prime-numbers -" (prime-numbers -13 13) '(2 3 5 7 11))
(test-is "prime-numbers 0" (prime-numbers 0 13) '(2 3 5 7 11))
(test-is "prime-numbers 1" (prime-numbers 1 13) '(2 3 5 7 11))
(test-is "prime-numbers 2" (prime-numbers 2 13) '(2 3 5 7 11))
(test-is "prime-numbers 3" (prime-numbers 3 13) '(3 5 7 11))
(test-is "prime-numbers 4" (prime-numbers 1000 1020) '(1009 1013 1019))

(done-testing)
