;;; to reproduce float parsing bug: .991 -> 0.91 #35

(require 'regtests)

(test-is "read .991" (read ".991") 0.991)
(test-is "read .672" (read ".672") 0.672)
(test-is "read .072" (read ".072") 0.072)
(test-is "read 0.072" (read "0.072") 0.072)

;; It turned out to be not only a float parsing bug, but one that
;; effected all tokens starting with a period, followed by one or more
;; other characters.
(test-is "read .haha" (read ".haha") ".haha")

(done-testing)
