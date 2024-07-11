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

;; (when t) returned t while SBCL showed it shouldn't
(test-is "when t" (when t) nil)


;; not *really* a bugfix, but we can assume that is was a bug that
;; setq wasn't multi-variable before...


(test-is "setq multi-var" (let (a b c d e f g h)
                            (setq a 1 b 1
                                  c (+ a b) d (+ b c)
                                  e (+ c d) f (+ d e)
                                  g (+ e f) h (+ f g))
                            h)
         21)

;; #34 `load :error nil` does not work
;;
;; > (load "no existe" :error nil)
;; IOError: could not find load file: no existe
;; #1 *terminal-io*:3 #<environment1[root:80]{...}>
;;    (load "no existe" :error nil)

(test-is "#34 load :error nil" (load "no existe" :error nil)
         nil)


(done-testing)
