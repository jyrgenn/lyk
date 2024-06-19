(require 'regtests)

(defparameter v (make-vector 3 4))

(test-is "new vector" v #(4 4 4))

(test-is "vector-get el 3" (errset (vector-get v 3)
                                    nil) nil)

(test-is "vector-get 0" (vector-get v 0) 4)
(test-is "vector-get 1" (vector-get v 1) 4)
(test-is "vector-get 2" (vector-get v 2) 4)
(test-is "vector-get 3" (errset (vector-get v 3)
                                 nil) nil)
(test-is "vector-get -1" (errset (vector-get v -1)
                                  nil) nil)

(test-is "vector-set 1" (vector-set v 1 5) 5)

(test-is "vector after set" v "#(4 5 4)")

(test-is "vector-get 0a" (vector-get v 0) 4)
(test-is "vector-get 1a" (vector-get v 1) 5)
(test-is "vector-get 2a" (vector-get v 2) 4)

(test-is "vector-set 0b" (vector-set v 0 6) 6)
(test-is "vector-set 1b" (vector-set v 1 7) 7)
(test-is "vector-set 2b" (vector-set v 2 8) 8)
(test-is "vector-set 3b" (errset (vector-set v 3 9)
                                  nil) nil)
(test-is "vector-set -1b" (errset (vector-set v -1 0)
                                   nil) nil)

;;; removed the tests with vector as a callable being called directly;
;;; we don't have that in this implementation


(test-is "vector again" v "#(6 7 8)")

;; same as above, with aref
(setf v #(4 4 4))
(test-is "aref 0" (aref v 0) 4)
(test-is "aref 1" (aref v 1) 4)
(test-is "aref 2" (aref v 2) 4)
(test-is "aref 3" (errset (aref v 3) nil) nil)
(test-is "aref -1" (errset (aref v -1) nil) nil)

(test-is "vector-set 1/a" (vector-set v 1 5) 5)

(test-is "vector after set/a" v "#(4 5 4)")

(test-is "aref 0a" (aref v 0) 4)
(test-is "aref 1a" (aref v 1) 5)
(test-is "aref 2a" (aref v 2) 4)

(test-is "vector type" (type-of v) "vector")

(test-is "make-vector 1" (make-vector 3 'a) #(a a a))
(test-is "make-vector 2" (make-vector 5 (let ((n 6)) (lambda () (incf n))))
         #(7 8 9 10 11))

(done-testing)
