(require 'regtests)

(test-is "fset" (progn (fset 'fooo (lambda (n) (+ n n)))
                        (fooo 34))
         "68")
(test-err "lambda 1" (lambda)
          #/too few normal args for special form/)
(test-err "lambda 2" (lambda 3)
          #/is not a proper list/)
(test-is "lambda 3" (lambda (n) (+ 3 n)) "#<lambda function *anon-lambda*>")
(test-is "defun" (progn (defun lala (n) (* n n))
                        (lala 123))
         "15129")

(defun f () nil)
(defmacro g () nil)
(defvar foomly)

(test-is "functionp" (map #'functionp (list #'f #'g foomly))
         '(t t nil))

(defparameter f-parameters
  '(n x &optional b (c 3) &key col (row 0) &rest other))
(defparameter f-body '((list n x b c other col row)))
(defparameter f-docstring "A sample function with a complicated arglist and...
...a multi-line docstring.")
(defparameter sample-f
  `(defun sample-function ,f-parameters ,f-docstring ,@f-body))

(eval sample-f)

(defparameter anon1 (lambda (n p)
                      "Be fruitful and multiply!"
                      (* n p)))

(test-is "function-definition 1" (function-definition #'sample-function)
         sample-f)
(test-err "function-definition 2" (function-definition #'cdr)
          #/not a lambda function or macro/)
(test-is "function-definition 3" (function-definition anon1)
         '(lambda (n p) "Be fruitful and multiply!" (* n p)))

(test-is "function-docstring 1" (function-docstring #'sample-function)
         f-docstring)
(test-is "function-docstring 2" (function-docstring #'cdr)
         "Return the contents of the decrement part of the `list` register.
The cdr of nil is nil.")
(test-is "function-docstring 3" (function-docstring anon1)
         "Be fruitful and multiply!")

(test-is "function-parameters 1" (function-parameters #'sample-function)
         f-parameters)
(test-is "function-parameters 2" (function-parameters #'with-open-file)
         '(file-declaration &rest bodyforms))
(test-is "function-parameters 3" (function-parameters anon1)
         '(n p))

(test-is "function-body 1" (function-body #'sample-function)
         f-body)
(test-err "function-body 2" (function-body #'cdr)
          #/not a lambda function or macro/)
(test-is "function-body 3" (function-body anon1)
         '((* n p)))

(test-is "function-call 1" (sample-function 3 2.22)
         '(3 2.22 nil 3 nil nil 0))

;; check that modifying code works as it should
(setf (cadar (function-body #'sample-function)) 17)
(test-is "function-call 2" (sample-function 3 2.22)
         '(17 2.22 nil 3 nil nil 0))

;; check that self-modifying code works as it should
(defun changeling ()
  (let ((n 0))
    (setf (car (cdr (car (car (cdr (car (function-body #'changeling)))))))
          (+ n 1))
    n))
(test-is "function-call 3" (changeling) 0)
(test-is "function-call 4" (changeling) 1)
(test-is "function-call 5" (changeling) 2)
(test-is "function-call 6" (changeling) 3)
(test-is "function-call 7" (changeling) 4)

(done-testing)
