(require 'regtests)

(defun mappend (l1 &rest lists)
  "Append L1 and more LISTS and return the result as a new list."
  (if (null lists)
      l1
    (if (null l1)
        (apply #'mappend lists)
      (cons (car l1)
            (apply #'mappend (cons (cdr l1) lists))))))

(test-is "apply in append"
         (mappend '(2) nil '(3 4 5 6 7 8) '(9 10 11) nil)
         "(2 3 4 5 6 7 8 9 10 11)")
         
(test-is "apply 2" (let ((f #'mappend)
                          (lists '((4 5 6))))
                     (apply f lists))
         "(4 5 6)")
(test-is "apply 3" (apply '+ '(5 6)) 11)
(test-is "apply 4" (apply '+ 3 4 '(5 6)) 18)
(test-is "funcall 1" (let ((f #'list)
                            (lists '(4 5 6)))
                       (funcall f lists))
         "((4 5 6))")


(done-testing)
