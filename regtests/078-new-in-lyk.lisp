(require 'regtests)

(test-err "1st warning" (no-warnings
                          (factor 64927)
                          (warning "must not be seen during regtests"))
         #/WarningError: must not be seen/)

(test-is "2nd warning"
         (append (no-warnings-as-errors
                   (no-warnings
                     (warning "this, 2, must not be seen during regtests")
                     (list (car (factor 64927)) (warnings-as-errors))))
                 (list (warnings-as-errors)))
         '(64927 nil t))

(done-testing)
