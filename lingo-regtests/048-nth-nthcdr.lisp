(require 'regtests)

(test-is "nth -1a" (errset (nth -1 '(3 4 5 6))
                            nil) nil)

(test-is "nth 0b" (nth 0 '(a b c d)) 'a)

(test-is "nth 3" (nth 3 '(a b c d)) 'd)
(test-is "nth 6" (nth 6 '(a b c d)) nil)

(test-is "nthcdr -1" (errset (nthcdr -1 '(4 5 6 7))
                              nil) nil)
(test-is "nthcdr 0" (nthcdr 0 '(4 5 6 7)) '(4 5 6 7))
(test-is "nthcdr 2" (nthcdr 2 '(4 5 6 7)) '(6 7))
(test-is "nthcdr 6" (nthcdr 6 '(4 5 6 7)) nil)

(done-testing)
