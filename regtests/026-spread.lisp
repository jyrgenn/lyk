(require 'regtests)

;; spreadable arguments lists as with apply and list*

(test-is "spread 0" (list*) nil)
(test-is "spread 1" (list* 5) 5)
(test-is "spread 2" (list* 3 4 5) '(3 4 . 5))
(test-is "spread 3" (list* 3 4 '(5 6)) '(3 4 5 6))
(test-is "spread 4" (list* '(5 6)) '(5 6))
(test-is "spread 5" (list* nil) nil)




(done-testing)
