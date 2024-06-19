(require 'regtests)

(test-err "comment in regexp" (read "#r;" nil)
          #/delimiter may not be whitespace or comment sign/)

(done-testing)
