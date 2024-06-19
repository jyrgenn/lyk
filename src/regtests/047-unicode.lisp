(require 'regtests)

;; TODO deal correctly with real Unicode characters

;; the following worked for an invalid code point, but we refuse these
;; already on reading (which is good); I did not yet find a code point
;; that is not a control char and not graphic to fit the intent of
;; this test case
;; (test-is "outside BMP" (prin1-to-string #\U000e01EF)
;;          "#\\U000E01EF")

(test-is "control char" (prin1-to-string #\b11011) "#\\x1b")
;; (test-is "big char" (prin1-to-string #\b100111011101110111)
;;          "#\\𧝷")
(test-is "chinese" (prin1-to-string #\㛛) "#\\㛛")
;; (test-is "pile of poo" #\U0001f4a9 #\💩 )

(done-testing)
