(require 'regtests)


;; turning on warnings-as-errors should make the warning that a
;; previously unbound variable is set outside of a defvar or
;; defparameter also prevent that setting, which it didn't before the
;; fix; now it does, which is tested here.

(makunbound 'unset-var)
(test "unset-var pre" (not (boundp 'unset-var)))
(test-err "setq unset-var" (setq unset-var 119)
          #/setting unbound variable/)
(test "unset-var post" (not (boundp 'unset-var)))

(done-testing)
