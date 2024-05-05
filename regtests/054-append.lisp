(require 'regtests)

(test-is "append 0" (append) nil)
(test-is "append 1" (append '()) nil)
(test-is "append 2" (append '(a)) '(a))
(test-is "append 3" (append '(a b)) '(a b))
(test-is "append 4" (append '() '()) nil)
(test-is "append 5" (append '() '(a)) '(a))
(test-is "append 6" (append '(a) '()) '(a))
(test-is "append 7" (append '(a) '(b)) '(a b))
(test-is "append 8" (append '() '(a b)) '(a b))
(test-is "append 9" (append '(a b) '()) '(a b))
(test-is "append 10" (append '(a b) '(c d)) '(a b c d))
(test-is "append 11" (append '(a) '(c d)) '(a c d))
(test-is "append 12" (append '(a b) '(c)) '(a b c))
(test-is "append 13" (append '() '() '()) nil)
(test-is "append 14" (append '(a) '() '()) '(a))
(test-is "append 15" (append '(a b) '() '()) '(a b))
(test-is "append 16" (append '() '(a) '()) '(a))
(test-is "append 17" (append '(a) '(b) '()) '(a b))
(test-is "append 18" (append '(a b) '(c) '()) '(a b c))
(test-is "append 19" (append '() '(a b) '()) '(a b))
(test-is "append 20" (append '(a) '(c d) '()) '(a c d))
(test-is "append 21" (append '(a b) '(c d) '()) '(a b c d))
(test-is "append 22" (append '() '() '(a)) '(a))
(test-is "append 23" (append '(a) '() '(b)) '(a b))
(test-is "append 24" (append '(a b) '() '(c)) '(a b c))
(test-is "append 25" (append '() '(a) '(b)) '(a b))
(test-is "append 26" (append '(a) '(b) '(c)) '(a b c))
(test-is "append 27" (append '(a b) '(c) '(d)) '(a b c d))
(test-is "append 28" (append '() '(a b) '(c)) '(a b c))
(test-is "append 29" (append '(a) '(b c) '(d)) '(a b c d))
(test-is "append 30" (append '(a b) '(c d) '(e)) '(a b c d e))
(test-is "append 31" (append '() '() '(a b)) '(a b))
(test-is "append 32" (append '(a) '() '(c d)) '(a c d))
(test-is "append 33" (append '(a b) '() '(c d)) '(a b c d))
(test-is "append 34" (append '() '(a) '(b c)) '(a b c))
(test-is "append 35" (append '(a) '(b) '(c d)) '(a b c d))
(test-is "append 36" (append '(a b) '(c) '(d e)) '(a b c d e))
(test-is "append 37" (append '() '(a b) '(c d)) '(a b c d))
(test-is "append 38" (append '(a) '(b c) '(d e)) '(a b c d e))
(test-is "append 39" (append '(a b) '(c d) '(e f)) '(a b c d e f))
(test-is "append 41" (append '() '() '() '()) nil)
(test-is "append 42" (append '(1 2 3) '() '(4 5 6) '(a) '(fffff))
         '(1 2 3 4 5 6 a fffff))

(defparameter list-a '(2 3 4 5 6))
(defparameter list-b '(c d e f))
(defparameter list-c '(#:() #:() #:()))

(test-is "append 43" (append list-a list-b list-c)
         '(2 3 4 5 6 c d e f #:() #:() #:()))
(test-is "append 43a" list-a '(2 3 4 5 6))
(test-is "append 43b" list-b '(c d e f))
(test-is "append 43c" list-c '(#:() #:() #:()))

(done-testing)