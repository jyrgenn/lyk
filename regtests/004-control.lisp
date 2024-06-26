(require 'regtests)

(test-is "quote" (quote lala) "lala")
(test-err "while 0" (while) #/too few normal args for/)
(test-not "while nil" (while nil))
(test-is "while" (let (a
                       (b '(2 3 5 7 11 13 17 19 23 29)))
                   (while b
                     (setq a (cons (car b) a))
                     (setq b (cdr b)))
                   (length a))
         10)
(test-is "while list" (let ((l '(3 4 5 6)) (sum 0))
                        (while l
                          (setq sum (+ sum (car l)))
                          (setq l (cdr l)))
                        sum)
         18)

(test-err "prog1 0" (let ((a 4) (b 5) (c 6) (d 7))
                      (list (prog1) a b c d))
         #/too few normal args for /)
(test-is "prog1 1" (let ((a 4) (b 5) (c 6) (d 7))
                     (list (prog1 (incf a)) a b c d))
         '(5 5 5 6 7))
(test-is "prog1 2" (let ((a 4) (b 5) (c 6) (d 7))
                     (list (prog1 (incf a) (incf b)) a b c d))
         '(5 5 6 6 7))
(test-is "prog1 3" (let ((a 4) (b 5) (c 6) (d 7))
                     (list (prog1 (incf a) (incf b) (incf c)) a b c d))
         '(5 5 6 7 7))
(test-is "prog1 4" (let ((a 4) (b 5) (c 6) (d 7))
                     (list (prog1 (incf a) (incf b) (incf c) (incf d)) a b c d))
         '(5 5 6 7 8))

;; TODO cannot catch errors in macro expansion in errset, sadly
;; (test-err "prog2 0" (let ((a 4) (b 5) (c 6) (d 7))
;;                       (list (prog2) a b c d))
;;          #/too few normal args for /)
;; (test-err "prog2 1" (let ((a 4) (b 5) (c 6) (d 7))
;;                       (list (prog2 (incf a)) a b c d))
;;           #/too few normal args for /)
(test-is "prog2 2" (let ((a 4) (b 5) (c 6) (d 7))
                     (list (prog2 (incf a) (incf b)) a b c d))
         '(6 5 6 6 7))
(test-is "prog2 3" (let ((a 4) (b 5) (c 6) (d 7))
                     (list (prog2 (incf a) (incf b) (incf c)) a b c d))
         '(6 5 6 7 7))
(test-is "prog2 4" (let ((a 4) (b 5) (c 6) (d 7))
                     (list (prog2 (incf a) (incf b) (incf c) (incf d)) a b c d))
         '(6 5 6 7 8))

(test-is "progn 0" (let ((a 4) (b 5) (c 6) (d 7))
                     (list (progn) a b c d))
         '(nil 4 5 6 7))
(test-is "progn 1" (let ((a 4) (b 5) (c 6) (d 7))
                     (list (progn (incf a)) a b c d))
         '(5 5 5 6 7))
(test-is "progn 2" (let ((a 4) (b 5) (c 6) (d 7))
                     (list (progn (incf a) (incf b)) a b c d))
         '(6 5 6 6 7))
(test-is "progn 3" (let ((a 4) (b 5) (c 6) (d 7))
                     (list (progn (incf a) (incf b) (incf c)) a b c d))
         '(7 5 6 7 7))
(test-is "progn 4" (let ((a 4) (b 5) (c 6) (d 7))
                     (list (progn (incf a) (incf b) (incf c) (incf d)) a b c d))
         '(8 5 6 7 8))


(done-testing)
