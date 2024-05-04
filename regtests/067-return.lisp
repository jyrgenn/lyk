;;; return function

(test-is "return fac" (progn
                        (defun fac-ret (n)
                          (let ((i 0)
                                (result 1))
                            (loop
                             (incf i)
                             (setf result (* i result))
                             (when (= i n)
                               (return result)))))
                        (fac-ret 7))
         5040)
                             
(done-testing)
