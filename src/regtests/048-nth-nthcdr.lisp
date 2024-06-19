(require 'regtests)

(test-err "nth -1a" (nth -1 '(3 4 5 6))
          #/must not be negative/)

(test-is "nth 0b" (nth 0 '(a b c d)) 'a)

(test-is "nth 3" (nth 3 '(a b c d)) 'd)
(test-is "nth 6" (nth 6 '(a b c d)) nil)

(test-err "nthcdr -1" (nthcdr -1 '(4 5 6 7))
          #/must not be negative/)

(test-is "nthcdr 0" (nthcdr 0 '(4 5 6 7)) '(4 5 6 7))
(test-is "nthcdr 2" (nthcdr 2 '(4 5 6 7)) '(6 7))
(test-is "nthcdr 6" (nthcdr 6 '(4 5 6 7)) nil)

(done-testing)
