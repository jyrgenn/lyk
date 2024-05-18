(require 'regtests)

(defparameter tree1 '(a . b))
(test-is "car" (car tree1) "a")
(test-is "cdr" (cdr tree1) "b")

(defparameter tree2 '((a . b) .
                      (c . d)))
(test-is "caar" (caar tree2) "a")
(test-is "cdar" (cdar tree2) "b")
(test-is "cadr" (cadr tree2) "c")
(test-is "cddr" (cddr tree2) "d")

(defparameter tree3 '(((a . b) .
                       (c . d)) .
                       ((e . f) .
                        (g . h))))
(test-is "caaar" (caaar tree3) "a")
(test-is "cdaar" (cdaar tree3) "b")
(test-is "cadar" (cadar tree3) "c")
(test-is "cddar" (cddar tree3) "d")
(test-is "caadr" (caadr tree3) "e")
(test-is "cdadr" (cdadr tree3) "f")
(test-is "caddr" (caddr tree3) "g")
(test-is "cdddr" (cdddr tree3) "h")

(defun n2b (n width &optional (acc ""))
  "Convert n to binary representation string of width."
  (if (zerop width)
      acc
      (n2b (ash n -1)
           (1- width)
           (string (if (zerop (% n 2)) "0" "1")
                   acc))))

(defparameter tree4
  (let ((*next-tag* 0))
    (flet ((next-symbol ()
             (prog1
                 (intern (n2b *next-tag* 4))
               (setq *next-tag* (1+ *next-tag*))))
           (mktree (depth)
             (if (zerop depth)
                 (next-symbol)
                 (cons (mktree (1- depth))
                       (mktree (1- depth))))))
      (mktree 4))))
;;(format t "~A~%" tree4)
(test-is "caaaar" (caaaar tree4) "0000")
(test-is "cdaaar" (cdaaar tree4) "0001")
(test-is "cadaar" (cadaar tree4) "0010")
(test-is "cddaar" (cddaar tree4) "0011")
(test-is "caadar" (caadar tree4) "0100")
(test-is "cdadar" (cdadar tree4) "0101")
(test-is "caddar" (caddar tree4) "0110")
(test-is "cdddar" (cdddar tree4) "0111")
(test-is "caaadr" (caaadr tree4) "1000")
(test-is "cdaadr" (cdaadr tree4) "1001")
(test-is "cadadr" (cadadr tree4) "1010")
(test-is "cddadr" (cddadr tree4) "1011")
(test-is "caaddr" (caaddr tree4) "1100")
(test-is "cdaddr" (cdaddr tree4) "1101")
(test-is "cadddr" (cadddr tree4) "1110")
(test-is "cddddr" (cddddr tree4) "1111")

(done-testing)
