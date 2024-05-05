;;; printing (or rather string conversion) of cyclic data structures

(require 'regtests)

;; Testing this with an environment is difficult, and I don't know where I
;; (test-is "cycles env" ...)

(test-is "cycles pair" (let ((a '(3 4 5 6)))
                         (setf (cadr a) a)
                         a)
         "(3 ... 5 6)")

;;; TODO do I want structs again at some point?
;; (defstruct self-holder "can hold its liquor and itself" this that)
;; (test-is "cycles struct" (let ((s (make-self-holder :this 19 :that 23)))
;;                            (setf (self-holder-this s) s)
;;                            s)
;;          "#S(self-holder this ... that 23)")

(test-is "cycles table" (let ((tbl #:()))
                          (table-put tbl tbl tbl)
                          (table-get tbl tbl))
         "#:((... . ...))")

(test-is "cycles vector" (let ((v #(4 1 2))
                               (w #(4 1 1)))
                           (setf (aref v 1) w)
                           (setf (aref w 2) v)
                           w)
         "#(4 1 #(4 ... 2))")

;; (defstruct foo "each must play a part" bar ble quu)
;; (test-is "cycles indirect" (let* ((a #:((1 . 2) (3 . 4)))
;;                                   (b (list 5 6 7 a))
;;                                   (c (vector 8 9 10 b))
;;                                   (f (make-foo :bar 11 :ble 12 :quu c)))
;;                              (a 3 f)
;;                              a)
;;          "#:((1 . 2)(3 . #S(foo bar 11 ble 12 quu #(8 9 10 (5 6 7 ...)))))")

(defparameter a '(3 . 4))
(test-is "linear cycle 0" a "(3 . 4)")

(test-is "linear cycle 1" (progn (setf (car a) a)
                                 a)
         "(... . 4)")

(test-is "linear cycle 2" (progn (setf (cdr a) a)
                                 a)
         "(... . ...)")

(test-is "linear cycle 3" (let* ((a '(1 2 3 4 5 6 7 8 9 10))
                                 (9th (nthcdr 9 a)))
                            (setf (cdr 9th) a))
         "(1 2 3 4 5 6 7 8 9 10 . ...)")

(test-is "no cycle, but twice" (let ((a '(3 4 5)))
                                 (cons a a))
         "((3 4 5) 3 4 5)")

(test-is "no cycle, but thrice" (let* ((a '(3 4 5))
                                       (b (list a 6 7 a 9 a)))
                                  (list 'c b 'd a b))
         "(c ((3 4 5) 6 7 (3 4 5) 9 (3 4 5)) d (3 4 5) ((3 4 5) 6 7 (3 4 5) 9 (3 4 5)))")


(done-testing)

