(require 'regtests)

(test-is "iota 0" (iota 0) '())
(test-is "iota 1" (iota 1) '(0))
(test-is "iota 5" (iota 5) '(0 1 2 3 4))
(test-is "iota 6" (iota 6 10) '(10 11 12 13 14 15))
(test-num "iota 7" (iota 7 1 -0.1) '(1 0.9 0.8 0.7 0.6 0.5 0.4))

(test-is "range 0" (range 0) '())
(test-is "range 1" (range 1) '(0))
(test-is "range 5" (range 5) '(0 1 2 3 4))
(test-is "range 6" (range 6 10) '(6 7 8 9))
(test-num "range 7" (range 7 1 -1) '(7 6 5 4 3 2))

(done-testing)
