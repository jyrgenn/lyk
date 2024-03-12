(require 'regtests)

;; callable vector

(defparameter v #(a b c d e f g h i j))

(test-is "vector 0 args" (errset (v) nil) nil)
(test-is "vector 1 arg"  (v 4) 'e)
(test-is "vector 2 args" (progn (v 4 123)
                                 (v 4))
         123)
(test-is "vector 3 args" (errset (v 3 4 5) nil) nil)


;; callable table

(defparameter tbl #:((a . b)(c . d)(e . f)(g . h)(i . j)(k . l)(m . n)))

(test-is "table 0 args" (errset (tbl) nil) nil)
(test-is "table 1 arg"  (tbl 'g) 'h)
(test-is "table 2 args" (progn (tbl 'g 129)
                                (tbl 'g))
         129)
(test-is "table 2a args" (progn (tbl 'g 133)
                                 (tbl 'f))
         nil)
(test-is "table 3 args" (errset (tbl 3 4 5) nil) nil)


;; callable regexp

(defparameter r #/^a+(.*(b.))*/)

(test-is "call regexp 0" (errset (r) nil) nil)
(test-is "call regexp 1a" (r "aaadrlzzt") '("aaa" "" ""))
(test-is "call regexp 1b" (r "aaadrblzzt") '("aaadrbl" "drbl" "bl"))
(test-is "call regexp 2" (errset (r "aaadrlzzt" "dldl") nil) nil)


(done-testing)
