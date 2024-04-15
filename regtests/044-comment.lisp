#! this is a #! simulation comment

(require 'regtests)

;; if we get through this, reading comments seems to work
(test-is "line comment" (progn 23       ;line comment
                                46)
         46)

;;; gnuddelfatz



(done-testing)
