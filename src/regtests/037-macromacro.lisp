(require 'regtests)

(test-is "macro parse 0" '`(foomly ,bar-tolomew neggli ,@schumm dibumm)
         '(quasiquote (foomly (unquote bar-tolomew)
                              neggli (unquote-splicing schumm) dibumm)))

(test-is "expand quasiquote 0" `a 'a)

(test-is "expand quasiquote 1"
         (let ((schumm '(3 4 5))
                (bar-tolomew 'Scotch))
            `(foomly ,bar-tolomew neggli ,@schumm dibumm))
         '(foomly Scotch neggli 3 4 5 dibumm))

(test-is "expand quasiquote 1a"
         (errset (let ((schumm 13)
                        (bar-tolomew 'Scotch))
                    `(foomly ,bar-tolomew neggli ,@schumm dibumm))
                  nil)
         nil)

;; example from the backquote page of the CLHS
(test-is "expand quasiquote 2"
         (let ((x 'param1)
                (y '(d e f)))
            `(cond ((numberp ,x) ,@y) (t (print ,x) ,@y)))
         '(cond ((numberp param1) d e f) (t (print param1) d e f)))

;; TODO does not explode JVM stack any more, but show circles where
;; there are none
;; example from the backquote page of the CLHS
;; (test-is "expand quasiquote 3"
;;          (let ((x '(a b c)))
;;             `(x ,x ,@x foo ,(cadr x) bar ,(cdr x) baz ,@(cdr x)))
;;          '(x (a b c) a b c foo b bar (b c) baz b c))

;; example from the backquote page of the CLHS
(test-is "expand quasiquote 4"
         (let ((a 'A)
                (c 'C)
                (d '(D D)))
            `((,a b) ,c ,@d))
         '((A b) C D D))


(defmacro quote-eval (toquote toeval)
  `(cons ',toquote ,toeval))

(test-is "macrop" (cons (macrop #'cons) (macrop #'quote-eval))
         '(nil . t))

(defmacro until (cond &rest bodyforms)
  "Repeat BODYFORMS until COND is true."
  `(while (not ,cond) ,@bodyforms))

(test-is "defun w/ macro" (progn (defun newstring (len init)
                                    (let ((str ""))
                                      (until (>= (length str) len)
                                             (setq str (string str init)))
                                      str))
                                  (newstring 10 "lala"))
         "lalalalalala")

(test-is "defun w/ when" (progn (defun stringw1 (base adder do)
                                   (when do
                                     (setq base (string base adder))
                                     (setq base (string base "done")))
                                   base)
                                 (cons (stringw1 "lala" "hoho" t)
                                       (stringw1 "lili" "huhu" nil)))
         '("lalahohodone" . "lili"))

;;; #'when is no longer a macro
;; (test-is "expand when 0" (macroexpand '(when condition))
;;          '(cond (condition nil)))
;; (test-is "expand when 1" (macroexpand '(when condition (doit)))
;;          '(cond (condition (doit))))
;; (test-is "expand when 2" (macroexpand '(when condition (dothis) (dothat)))
;;          '(cond (condition (dothis) (dothat))))

(test-is "defun w/ unless" (progn (defun stringu1 (base adder do)
                                     (unless do
                                       (setq base (string base adder))
                                       (setq base (string base "done")))
                                     base)
                                   (cons (stringu1 "lala" "hoho" t)
                                         (stringu1 "lili" "huhu" nil)))
         '("lala" . "lilihuhudone"))

(defun func_nm1 (base adder len do)
  (until (>= (length base) len)
         (when do
           (setf base (string base adder))
           (setf base (string base "|")))
         (setq base (string base ".")))
  base)

(test-is "nested macro 1" (func_nm1 "ha!" "sss" 20 t)
         "ha!sss|.sss|.sss|.sss|.")
(test-is "nested macro 2" (func_nm1 "ha!" "ss" 10 nil)
         "ha!.......")

;;; until no longer a macro (should it be, again?)
;; (test-is "nested expanded 1" (function-body #'func_nm1)
;;          '((while (not (>= (length base) len)) (when do (setq base (string base adder)) (setq base (string base "|"))) (setq base (string base "."))) base))

(defmacro do-until (bodyforms condition)
  (let ((controlvar (gensym "do-until-controlvar")))
    `(let ((,controlvar nil))
       (until ,controlvar
              ,@bodyforms
              (when ,condition
                (setq ,controlvar t))))))

(defun addup (base inc limit)
  (let ((sum base)
        (count 0))
    (do-until
     ((incf sum inc)
      (incf count))
     (>= sum limit))
    (cons count sum)))

(test-is "nested macro 3" (addup 3 2 10) '(4 . 11))

;;; until no longer a macro
;; must remove gensym tags from expanded macro
;; (test-is "nested expanded 2" (function-body #'addup)
;;          '((let ((sum base) (count 0)) (let ((do-until-controlvar nil)) (until do-until-controlvar (setq sum (+ sum inc)) (setq count (1+ count)) (when (>= sum limit) (setq do-until-controlvar t)))) (cons count sum))))

(done-testing)
