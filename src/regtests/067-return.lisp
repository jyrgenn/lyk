;;; return function

(test-is "return fac" (flet ((fac-ret (n)
                               "This is certainly one of the more convoluted
ways to write a faculty function."
                               (let ((i 0)
                                     (result 1))
                                 (loop
                                  (incf i)
                                  (setf result (* i result))
                                  (when (= i n)
                                    (return result))))))
                        (fac-ret 7))
         5040)

;; make sure a return form in the argument list of a function does not
;; return from *that* function, but from the outer
(test-is "return outer" (flet ((inner (a b c)
                                 (list c b a))
                               (outer (b c)
                                 (list (inner 3 4 5)
                                       (inner b c (return "mimimi")))))
                          (outer 'humpty 'dumpty))
         "mimimi")
                             
(done-testing)
