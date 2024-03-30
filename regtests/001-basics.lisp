(require 'regtests)

(test "zerop 0" (zerop (- 3 3)))
(test "zerop 0.0" (zerop (- 3.4 3.4)))
(test-not "zerop !0" (zerop (- 3 3.4)))
(test-is "describe" (describe 'newsymbol)
          "((type . symbol) (name . "newsymbol") (immutable) (desc-name . "newsymbol") (function) (props) (boundp . t) (value))")
(test "null" (null (null 'a)))
(test "not" (not (not 'a)))
(test-is "princ" (princ 'lala) "lala")
(test-not "terpri" (terpri))
(test-is "type-of symbol" (type-of 'a) "symbol")
(test-is "type-of pair" (type-of '(lambda)) "pair")
(test-is "type-of fixnum" (type-of 34444) "number")
(test-is "type-of flonum" (type-of 34.444) "number")
(test-is "type-of t" (type-of t) "symbol")
(test-is "type-of nil" (type-of nil) "symbol")
(test-is "type-of table" (type-of #:()) "table")
(test-is "type-of vector" (type-of #()) "vector")
(test-is "type-of string" (type-of "") "string")
(test-is "type-of function" (type-of #'car) "function")
;; TODO (defstruct str "teststructtype" x y)
;; TODO (test-is "type-of struct" (type-of (make-str)) 'struct)
(test-is "princs" (princs (list (* 3 4) t)) "(12 t)")
(test-is "funcall" (funcall '* 1 2 3 4 5 6 7) 5040)
(test-is "apply" (apply '* '(1 2 3 4 5 6 7)) 5040)
(test-is "identity" (identity 'lalala) 'lalala)
(test-not "ignore 0" (ignore))
(test-not "ignore 1" (ignore 1))
(test-not "ignore 2" (ignore 1 2))

(defvar b)
(test-is "makunbound b" (let ((a 13))
                          (makunbound 'b)
                          a)
         13)
(test-err "makunbound a" (let ((a 13))
                           (makunbound 'a)
                           a)
          #/unbound variable/)
(test-is "fmakunbound a" (flet ((b (lambda (n) (* n n))))
                           (fmakunbound 'a)
                           (b 13))
         169)
(test-err "fmakunbound b" (flet ((b (lambda (n) (* n n))))
                            (fmakunbound 'b)
                            (b 13))
         #/no function/)
(test-is "fset dfdf" (progn (fset 'dfdf (lambda (n) (+ n 13)))
                            (dfdf 22))
         35)
(test-is "symbol-function" (progn (fset 'mal (symbol-function '*))
                                  (mal 1 2 3 4 5 6 7))
         5040)
(test-err "error" (error "lala: `%d'" (+ 3 4)) #/^lala: `7'$/)

(test "integerp 1" (integerp 1))
(test "integerp 2" (integerp 2))
(test "integerp -3" (integerp -3))
(test-not "integerp 4.5" (integerp 4.5))
(test-not "integerp a" (integerp 'a))
(test-not "integerp \"huhu\"" (integerp "huhu"))
(test-not "integerp nil" (integerp '()))

;; the value of the symbol is the symbol itself
(test-is "lambda sym" lambda "lambda")

;; lambda definitions
(test-is "lambda fun" (lambda (n) (* n n)) "#<function (n) (* n n)>")
(test-is "λ fun" (λ (n) (* n n)) "#<function (n) (* n n)>")

;; ... quoted
(test-is "function lambda" (eval (read "#'(lambda (n) (+ n n))"))
         (function (lambda (n) (+ n n))))
(test-is "function λ" (eval (read "#'(λ (n) (+ n n))"))
         (function (lambda (n) (+ n n))))

;; ... as functions
(test-is "lambda 2+" (map #'(lambda (n) (+ 2 n)) '(2 3 5 7 11 13 17 19 23))
         '(4 5 7 9 13 15 19 21 25))
(test-is "λ 2+" (map #'(λ (n) (+ 2 n)) '(2 3 5 7 11 13 17 19 23))
         '(4 5 7 9 13 15 19 21 25))

;; lingo version

(test "lingo-version 1" (stringp sys:lingo-version))
(test-is "lingo-version 2" (join " " (sys:lingo-version)) sys:lingo-version)
(test-match "lingo-version 3" (car (sys:lingo-version 'version-tag))
            #/^v\d+\.\d+/)

(test-is "lingo-version 4" (let ((items '(version-tag build-date builder)))
                             (length (apply #'sys:lingo-version items)))
         (let ((items '(version-tag build-date builder)))
           (length items)))
(test-is "lingo-version 5" (let ((items '(version-tag build-date builder)))
                             (reverse (apply #'sys:lingo-version items)))
         (let ((items '(version-tag build-date builder)))
           (apply #'sys:lingo-version (reverse items))))

;; #22 #'set evaluates too much

(test-is "setq a" (let ((a 13)
                        (b 'c))
                    (setq a b)
                    a)
         'c)
;; with bug #22, this complains about an unbound variable c
(test-is "set 'a" (let ((a 13)
                       (b 'c))
                   (set 'a b)
                   a)
         'c)
(test-is "set b" (let ((a 'd)
                       (b 'c)
                       (c 0))
                   (set b a)
                   c)
         'd)

(done-testing)
