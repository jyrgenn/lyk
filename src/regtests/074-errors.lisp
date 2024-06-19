(require 'regtests)

;; show a nil args list as ()
(test-err "cons 0 args" (cons)
          #/too few normal args for builtin function `cons`: \(\)/)

(done-testing)
