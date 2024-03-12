;;; Project Euler problem 18

(defvar numtree                         ;vector of vectors
  #(#(75)
    #(95 64)
    #(17 47 82)
    #(18 35 87 10)
    #(20 04 82 47 65)
    #(19 01 23 75 03 34)
    #(88 02 77 73 07 63 67)
    #(99 65 04 28 06 16 70 92)
    #(41 41 26 56 83 40 80 70 33)
    #(41 48 72 33 47 32 37 16 94 29)
    #(53 71 44 65 25 43 91 52 97 51 14)
    #(70 11 33 28 77 73 17 78 39 68 17 57)
    #(91 71 52 38 17 14 91 43 58 50 27 29 48)
    #(63 66 04 68 89 53 67 30 73 16 69 87 40 31)
    #(04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)))
(defvar maxrow (1- (length numtree)))

(defvar lookup #:())                    ;this is a hash table

(defun memoize (func row column)        ;factored out for clarity
  (let ((key (string row ":" column)))
    (or (lookup key)
        (lookup key (func row column)))))

(defun pathsum-subtree (row column)
  (if (= row maxrow)
      ((numtree row) column)
    (+ ((numtree row) column)
       (max (memoize #'pathsum-subtree (1+ row) column)
            (memoize #'pathsum-subtree (1+ row) (1+ column))))))

(defun eu18 ()
  (pathsum-subtree 0 0))

;; result 1074
