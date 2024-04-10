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

;; seems like let cannot cope with a binding form that is a list with
;; one element. Two, yes, a symbol, yes, but not a list like (result).

;; so, this triggered an error "let: malformed variable clause for `result`",
;; which it must not
(test-is "let one-elem-binding" (let ((result))
                                  result)
         nil)

(done-testing)
