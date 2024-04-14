(require 'regtests)

(test-is "rplaca" (let ((a '(4 . 5)))
                    (rplaca a 6))
         "(6 . 5)")
(test-is "rplacd" (let ((cc '(lala . gugu)))
                    (rplacd cc nil))
         "(lala)")
(test-is "eval" (eval '(+ 3 (let ((a '(3 4))) (car (cdr a))))) "7")
(test-is "if nil" (let ((a '(c d))) (if nil (car a) (cdr a))) "(d)")
(test-is "if t" (let ((a '(c d))) (if t (car a) (cdr a))) "c")
(test-is "setq" (let ((a '(c d))) (setq a 19) a) "19")
(test "atom nil" (atom nil))
(test "atom t" (atom t))
(test "atom number" (atom 13))
(test "atom string" (atom "aa"))
(test "atom char" (atom #\4))
(test-not "atom cons" (atom '(nil)))
(test-is "car cons" (car '(a . b)) "a")
(test-not "car nil" (car nil))
(test-is "car list" (car '(a b)) "a")
(test-err "car string" (car "'(a . b)")
          #/argument is not a list/)
(test-is "cdr cons" (cdr '(a . b)) "b")
(test-not "cdr nil" (cdr nil))
(test-is "cdr list" (cdr '(a b)) "(b)")
(test-err "cdr string" (cdr "'(a . b)")
         #/argument is not a list/)
(test-is "cons" (cons 4 5) "(4 . 5)")
(test "eq y" (let ((a 'huhu) (b 'huhu)) (eq a b)))
(test "eq n" (let ((a "huhu") (b "huhu")) (eq a b)))
(test-not "eq l" (let ((a '(lala)) (b '(lala))) (eq a b)))

(done-testing)
