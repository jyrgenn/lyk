(require 'regtests)

;; show a nil args list as ()
(test-err "cons 0 args" (cons) #/args are \(\)/)

(done-testing)
