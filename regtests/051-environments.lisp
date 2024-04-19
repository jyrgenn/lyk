(require 'regtests)

(makunbound 'a)
(makunbound 'b)

(defparameter env1 (let ((a 3))
                     (the-environment)))

(test-is "env bound 1" (boundp 'a) nil)
(test-is "env bound 2" (with-environment env1 (boundp 'a)) t)
(test-is "env bound 3" (with-environment env1 a) 3)

(defparameter env2 (new-environment))
(defparameter env3 (new-environment env1))

(test-is "env new 1" (with-environment env2 (boundp 'a)) nil)
(test-is "env new 2" (with-environment env3 (boundp 'a)) t)

;; lyk is started by run-rests.lisp with -W
(test-err "env setq 1a" (setq a 110)
          #/setting unbound variable a to 110/)
(test-is "env setq 1b" (boundp 'a) nil)

(test-is "*warnings-as-errors*" *warnings-as-errors* t)


(test-is "env setq 1c" (no-warnings
                         (let ((*warnings-as-errors* nil))
                           (progn (setq a 110)
                                  a)))
         110)

(defparameter a 119)
(test-is "env value 1" (with-environment env1 a) 3)
(test-is "env value 2" (with-environment env2 a) 119)
(test-is "env value 3" (with-environment env3 a) 3)

(defparameter env4 (new-environment nil))
(test-is "env value 4" (with-environment env4 (boundp 'a)) nil)

(defparameter env5 (new-environment t #:((a . 110) (b . 993))))
(test-is "env value 5" (let ((b 64927)) (with-environment env5 b)) 993)
(test-is "env value 5a" (let ((b 64927)) (with-environment env5 a)) 110)
(test-is "env value 5aa" a 119)
(test-is "env value 5b" (boundp 'b) nil)

(test-is "env table non-sym"
         (errset (new-environment t #:((a . 110) (4 . 993)))
                  nil)
         nil)

(done-testing)
