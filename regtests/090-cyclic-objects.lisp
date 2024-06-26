(require 'regtests)

(test-is "recursive table 2" (let ((a #:((3 . 4)(5 . 6)(7 . 8))))
                               (table-put a a a))
         "#:((3 . 4)(5 . 6)(7 . 8)(... . ...))")
(test-is "recursive table 1" (let ((a #:((3 . 4)(5 . 6)(7 . 8))))
                               (table-put a 5 a))
         "#:((3 . 4)(5 . ...)(7 . 8))")

(test-is "recursive cons 1" (let ((a '(b . c)))
                             (rplaca a a))
        "(( ... ) . c)")
(test-is "recursive cons 2" (let ((a '(b . c)))
                             (rplacd a a))
        "(b ... )")
(test-is "recursive cons 3" (let ((a '(b . c)))
                             (rplaca a a)
                             (rplacd a a))
        "(( ... ) ... )")
(test-is "recursive vector" (let ((a #(3 4 5 6 7)))
                              (setf (elt a 3) a))
         "#(3 4 5 ... 7)")

(test-is "multiple, but not circular" (let* ((a #(3 4 5 6 7))
                                             (b (list 3 4 a 5 6 a 7 8 a)))
                                        b)
         (list 3 4 #(3 4 5 6 7) 5 6 #(3 4 5 6 7) 7 8 #(3 4 5 6 7)))

(done-testing)
