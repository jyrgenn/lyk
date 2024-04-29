(require 'regtests)

(test-is "setf car 1" (let ((l '(d e f g h i j)))
                        (setf (car l) 119)
                        l)
         '(119 e f g h i j))
(test-is "setf car 1 ex" (macroexpand '(setf (car l) 119))
         '(rplaca-ret-value l 119))
(test-is "setf car 2" (let ((l '(d e f g h i j)))
                        (setf (car (cdr l)) 119)
                        l)
         '(d 119 f g h i j))
(test-is "setf car 2 ex" (macroexpand '(setf (car (cdr l)) 119))
         '(rplaca-ret-value (cdr l) 119))
(test-is "setf car 3" (let ((l '(d e f g h i j)))
                        (setf (car (cdr (cdr l))) 119)
                        l)
         '(d e 119 g h i j))
(test-is  "setf car 3 ex" (macroexpand '(setf (car (cdr (cdr l))) 119))
          '(rplaca-ret-value (cdr (cdr l)) 119))
(test-is "setf car 4" (let ((l '((d e f) g h i j)))
                        (setf (car (cdr (car l))) 119)
                        l)
         '((d 119 f) g h i j))
(test-is "setf car 4 ex" (macroexpand '(setf (car (cdr (car l))) 119))
         '(rplaca-ret-value (cdr (car l)) 119))

(test-is "setf cdr 1" (let ((l '(d e f g h i j)))
                        (setf (cdr l) 119)
                        l)
         '(d . 119))
(test-is "setf cdr 1 ex" (macroexpand '(setf (cdr l) 119))
         '(rplacd-ret-value l 119))
(test-is "setf cdr 2" (let ((l '(d e f g h i j)))
                        (setf (cdr (cdr l)) 119)
                        l)
         '(d e . 119))
(test-is "setf cdr 2 ex" (macroexpand '(setf (cdr (cdr l)) 119))
         '(rplacd-ret-value (cdr l) 119))
(test-is "setf cdr 3" (let ((l '(d e f g h i j)))
                        (setf (cdr (cdr (cdr l))) 119)
                        l)
         '(d e f . 119))
(test-is "setf cdr 3 ex" (macroexpand '(setf (cdr (cdr (cdr l))) 119))
         '(rplacd-ret-value (cdr (cdr l)) 119))
(test-is "setf cdr 4" (let ((l '((d e f) g h i j)))
                        (setf (cdr (cdr (car l))) 119)
                        l)
         '((d e . 119) g h i j))
(test-is "setf cdr 4 ex" (macroexpand '(setf (cdr (cdr (car l))) 119))
         '(rplacd-ret-value (cdr (car l)) 119))

(defparameter tt '((aa . da) .
                   (ad . dd)))

(test-is "setf caar" (progn (setf (caar tt) 71) tt)
         '((71 . da) .
           (ad . dd)))
(test-is "setf cadr" (progn (setf (cadr tt) 72) tt)
         '((71 . da) .
           (72 . dd)))
(test-is "setf cdar" (progn (setf (cdar tt) 73) tt)
         '((71 . 73) .
           (72 . dd)))
(test-is "setf cddr" (progn (setf (cddr tt) 74) tt)
         '((71 . 73) .
           (72 . 74)))

(defparameter ttt '(((aaa . daa) .
                     (ada . dda)) .
                     ((aad . dad) .
                      (add . ddd))))

(test-is "setf caaar" (progn (setf (caaar ttt) 34) ttt)
         '(((34 . daa) .
            (ada . dda)) .
            ((aad . dad) .
             (add . ddd))))
(test-is "setf caadr" (progn (setf (caadr ttt) 35) ttt)
         '(((34 . daa) .
            (ada . dda)) .
            ((35 . dad) .
             (add . ddd))))
(test-is "setf cadar" (progn (setf (cadar ttt) 36) ttt)
         '(((34 . daa) .
            (36 . dda)) .
            ((35 . dad) .
             (add . ddd))))
(test-is "setf caddr" (progn (setf (caddr ttt) 37) ttt)
         '(((34 . daa) .
            (36 . dda)) .
            ((35 . dad) .
             (37 . ddd))))
(test-is "setf cdaar" (progn (setf (cdaar ttt) 38) ttt)
         '(((34 . 38) .
            (36 . dda)) .
            ((35 . dad) .
             (37 . ddd))))
(test-is "setf cdadr" (progn (setf (cdadr ttt) 39) ttt)
         '(((34 . 38) .
            (36 . dda)) .
            ((35 . 39) .
             (37 . ddd))))
(test-is "setf cddar" (progn (setf (cddar ttt) 40) ttt)
         '(((34 . 38) .
            (36 . 40)) .
            ((35 . 39) .
             (37 . ddd))))
(test-is "setf cdddr" (progn (setf (cdddr ttt) 41) ttt)
         '(((34 . 38) .
            (36 . 40)) .
            ((35 . 39) .
             (37 . 41))))

(defparameter tttt '((((aaaa . daaa) .
                       (adaa . ddaa)) .
                       ((aada . dada) .
                        (adda . ddda))) .
                        (((aaad . daad) .
                          (adad . ddad)) .
                          ((aadd . dadd) .
                           (addd . dddd)))))

(test-is "setf caaaar" (progn (setf (caaaar tttt) 119) tttt)
         '((((119 . daaa) .
             (adaa . ddaa)) .
             ((aada . dada) .
              (adda . ddda))) .
              (((aaad . daad) .
                (adad . ddad)) .
                ((aadd . dadd) .
                 (addd . dddd)))))
(test-is "setf caaadr" (progn (setf (caaadr tttt) 120) tttt)
         '((((119 . daaa) .
             (adaa . ddaa)) .
             ((aada . dada) .
              (adda . ddda))) .
              (((120 . daad) .
                (adad . ddad)) .
                ((aadd . dadd) .
                 (addd . dddd)))))
(test-is "setf caadar" (progn (setf (caadar tttt) 121) tttt)
         '((((119 . daaa) .
             (adaa . ddaa)) .
             ((121 . dada) .
              (adda . ddda))) .
              (((120 . daad) .
                (adad . ddad)) .
                ((aadd . dadd) .
                 (addd . dddd)))))
(test-is "setf caaddr" (progn (setf (caaddr tttt) 122) tttt)
         '((((119 . daaa) .
             (adaa . ddaa)) .
             ((121 . dada) .
              (adda . ddda))) .
              (((120 . daad) .
                (adad . ddad)) .
                ((122 . dadd) .
                 (addd . dddd)))))
(test-is "setf cadaar" (progn (setf (cadaar tttt) 123) tttt)
         '((((119 . daaa) .
             (123 . ddaa)) .
             ((121 . dada) .
              (adda . ddda))) .
              (((120 . daad) .
                (adad . ddad)) .
                ((122 . dadd) .
                 (addd . dddd)))))
(test-is "setf cadadr" (progn (setf (cadadr tttt) 124) tttt)
         '((((119 . daaa) .
             (123 . ddaa)) .
             ((121 . dada) .
              (adda . ddda))) .
              (((120 . daad) .
                (124 . ddad)) .
                ((122 . dadd) .
                 (addd . dddd)))))
(test-is "setf caddar" (progn (setf (caddar tttt) 125) tttt)
         '((((119 . daaa) .
             (123 . ddaa)) .
             ((121 . dada) .
              (125 . ddda))) .
              (((120 . daad) .
                (124 . ddad)) .
                ((122 . dadd) .
                 (addd . dddd)))))
(test-is "setf cadddr" (progn (setf (cadddr tttt) 126) tttt)
         '((((119 . daaa) .
             (123 . ddaa)) .
             ((121 . dada) .
              (125 . ddda))) .
              (((120 . daad) .
                (124 . ddad)) .
                ((122 . dadd) .
                 (126 . dddd)))))
(test-is "setf cdaaar" (progn (setf (cdaaar tttt) 127) tttt)
         '((((119 . 127) .
             (123 . ddaa)) .
             ((121 . dada) .
              (125 . ddda))) .
              (((120 . daad) .
                (124 . ddad)) .
                ((122 . dadd) .
                 (126 . dddd)))))
(test-is "setf cdaadr" (progn (setf (cdaadr tttt) 128) tttt)
         '((((119 . 127) .
             (123 . ddaa)) .
             ((121 . dada) .
              (125 . ddda))) .
              (((120 . 128) .
                (124 . ddad)) .
                ((122 . dadd) .
                 (126 . dddd)))))
(test-is "setf cdadar" (progn (setf (cdadar tttt) 129) tttt)
         '((((119 . 127) .
             (123 . ddaa)) .
             ((121 . 129) .
              (125 . ddda))) .
              (((120 . 128) .
                (124 . ddad)) .
                ((122 . dadd) .
                 (126 . dddd)))))
(test-is "setf cdaddr" (progn (setf (cdaddr tttt) 130) tttt)
         '((((119 . 127) .
             (123 . ddaa)) .
             ((121 . 129) .
              (125 . ddda))) .
              (((120 . 128) .
                (124 . ddad)) .
                ((122 . 130) .
                 (126 . dddd)))))
(test-is "setf cddaar" (progn (setf (cddaar tttt) 131) tttt)
         '((((119 . 127) .
             (123 . 131)) .
             ((121 . 129) .
              (125 . ddda))) .
              (((120 . 128) .
                (124 . ddad)) .
                ((122 . 130) .
                 (126 . dddd)))))
(test-is "setf cddadr" (progn (setf (cddadr tttt) 132) tttt)
         '((((119 . 127) .
             (123 . 131)) .
             ((121 . 129) .
              (125 . ddda))) .
              (((120 . 128) .
                (124 . 132)) .
                ((122 . 130) .
                 (126 . dddd)))))
(test-is "setf cdddar" (progn (setf (cdddar tttt) 133) tttt)
         '((((119 . 127) .
             (123 . 131)) .
             ((121 . 129) .
              (125 . 133))) .
              (((120 . 128) .
                (124 . 132)) .
                ((122 . 130) .
                 (126 . dddd)))))
(test-is "setf cddddr" (progn (setf (cddddr tttt) 134) tttt)
         '((((119 . 127) .
             (123 . 131)) .
             ((121 . 129) .
              (125 . 133))) .
              (((120 . 128) .
                (124 . 132)) .
                ((122 . 130) .
                 (126 . 134)))))

(test-is "setf elt L" (let ((l '(22 33 44 55 66)))
                        (setf (elt l 2) 'bb)
                        l)
         '(22 33 bb 55 66))
(test-is "setf elt V" (let ((v #(22 33 44 55 66)))
                        (setf (elt v 2) 'bb)
                        v)
         #(22 33 bb 55 66))
(test-is "setf elt !S" (progn (errset (let ((s "abcdefg"))
                                        (setf (elt s 2) 4)
                                        s)
                                      nil)
                              *last-error*)
         "TypeError: string object is immutable: \"abcdefg\"")

(defparameter v #(3 4 5 6))
(test-is "setf vector-get" (progn (setf (vector-get v 2) 119)
                                  v)
         #(3 4 119 6))
(test-is "setf aref" (progn (setf (aref v 2) (- (aref v 2) 3))
                            v)
         #(3 4 116 6))
(defparameter tab #:((12 . 144)(13 . 169)(14 . 196)(15 . 225)))
(test-is "setf table-get" (progn (setf (table-get tab 13) 119)
                                 (sort (table-pairs tab)
                                       (lambda (p1 p2)
                                         (< (car p1) (car p2)))))
         '((12 . 144)(13 . 119)(14 . 196)(15 . 225)))

(defun square (n) (* n n))
(test-is "setf symbol-function" (progn (setf (symbol-function '^2) #'square)
                                       #'^2)
         "#<function square>")

(defparameter symbol 'prop-bearer)
(test-is "setf get" (progn (setf (get symbol 'shlongity) 1e19)
                           (get symbol 'shlongity))
         1e19)

(defvar shnllli)
(test-is "setf symbol-value" (progn (setf (symbol-value 'shnllli) 13.4)
                                    shnllli)
         13.4)

;; from the CLHS setf page
(defvar x)
(defvar y)

(test-is "setf clhs" (progn (setq x (cons 'a 'b))
                            (setq y (list 1 2 3))
                            (setf (car x) 'x)
                            (setf (cadr y) (car x))
                            (setf (cdr x) y)
                            (cons x y))
         '((x 1 x 3) . (1 x 3)))

(defparameter b '((3) (4) ((5 6))))
;;; TODO macroexpand is stackexplode
(test-is "incf" (progn (incf (car (cdr (caaddr b))) 2)
                       b)
         '((3) (4) ((5 8))))

(defparameter v #(2 4 6 8 10))
(test-is "incf aref" (progn (incf (aref v 2) 7)
                            v)
         #(2 4 13 8 10))

(test-is "decf" (progn (decf (car (caaddr b)))
                       b)
         '((3) (4) ((4 8))))

(test-is "pop" (cons (pop (caaddr b)) b)
         '(4 (3) (4) ((8))))

(test-is "pushf 1" (progn (push '73 (caaddr b))
                          b)
         '((3) (4) ((73 8))))

(test-is "pushf 2" (progn (push '73 (car (car (cdr (cdr b)))))
                          b)
         '((3) (4) ((73 73 8))))

;;; from CLHS
(fset 'list-length #'length)

(defun middleguy (x) (nth (truncate (/ (1- (list-length x)) 2)) x))
(defun set-middleguy (x v)
  (unless (null x)
    (rplaca (nthcdr (truncate (/ (1- (list-length x)) 2)) x) v))
  v)

(defsetf middleguy set-middleguy)
(defparameter a (list 'a 'b 'c 'd))
(defparameter b (list 'x))
(defparameter c (list 1 2 3 (list 4 5 6) 7 8 9))

(test-is "defsetf clhs 1" (cons (setf (middleguy a) 3)
                                a)
         '(3 a 3 c d))
(test-is "defsetf clhs 2" (cons (setf (middleguy b) 7)
                                b)
         '(7 7))
(test-is "defsetf clhs 3" (cons (setf (middleguy (middleguy c))
                                      'middleguy-symbol)
                                c)
         '(middleguy-symbol 1 2 3 (4 middleguy-symbol 6) 7 8 9))

(test-is "setf nth" (let ((l '(a b c d e f g h)))
                      (setf (nth 3 l) 133)
                      l)
         '(a b c 133 e f g h))

(done-testing)
