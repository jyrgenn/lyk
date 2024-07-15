
(defmacro unless (condition &rest else-clauses)
  "If `condition` evaluates to nil, eval all `else-clauses` and return
the value of the last. Otherwise, return nil."
  (cond ((not else-clauses) (setq else-clauses '(nil))))      
  `(cond (,condition nil)
         (t ,@else-clauses)))

(defmacro when (condition &rest then-clauses)
  "If `condition` evaluates to non-nil, eval all `then-clauses` and return
the value of the last. Otherwise return nil."
  (cond ((not then-clauses) (setq then-clauses '(nil))))      
  `(cond (,condition ,@then-clauses)))

(defmacro if (condition then-clause &rest else-clauses)
  "If `condition` evals to non-nil, eval `then-clause` and return the value.
Otherwise, evaluate `else-clauses` and return the last value."
  (cond ((not else-clauses) (setq else-clauses '(nil))))      
  `(cond (,condition ,then-clause)
         (t ,@else-clauses)))


(defmacro =/= (ob1 ob2)
  "Return true iff OB1 and OB2 are unequal (in terms of #'equal)."
  `(not (equal ,ob1 ,ob2)))

(defmacro dolist (control-vars &rest bodyforms)
  "(dolist (var list-form &optional result-form) &rest bodyforms)"
  (let (((var list-form result-form) control-vars)
        (listsym (gensym)))
    `(let ((,listsym ,list-form))
       (while ,listsym
         (let ((,var (pop ,listsym)))
           ,@bodyforms))
       ,result-form)))

(defmacro dotimes (countargs &rest bodyforms)
  "(dotimes (var count-form &optional result-form) &rest bodyforms)
dotimes evaluates count-form, which should produce an integer. If
count-form is zero or negative, the body is not executed. dotimes then
executes the body once for each integer from 0 up to but not including
the value of count-form, in the order in which the statements occur,
with var bound to each integer. Then result-form is evaluated. At the
time result-form is processed, var is bound to the number of times the
body was executed. [CLHS]"
  (let ((var (car countargs))
        (endval (cadr countargs))
        (resultform (caddr countargs)))
    (let ((end (gensym)))
      `(let ((,var 0)
             (,end ,endval))
         (while (< ,var ,end)
           ,@bodyforms
           (incf ,var))
         ,resultform))))

(defmacro dotimes1 (countargs &rest bodyforms)
  "(dotimes1 (var count-form &optional result-form) &rest bodyforms)
dotimes evaluates count-form, which should produce an integer. If
count-form is zero or negative, the body is not executed. dotimes then
executes the body once for each integer from 1 up to including the
value of count-form, in the order in which the statements occur, with
var bound to each integer. Then result-form is evaluated. At the time
result-form is processed, var is bound to the number of times the body
was executed."
  (let ((var (car countargs))
        (endval (cadr countargs))
        (resultform (caddr countargs)))
    (let ((end (gensym)))
      `(let ((,var 1)
             (,end ,endval))
         (while (<= ,var ,end)
           ,@bodyforms
           (incf ,var))
         (decf ,var)
         ,resultform))))

;; (defmacro aref (vector index)
;;   "Return the element of VECTOR at INDEX."
;;   `(vector-get ,vector ,index))

(defmacro with-open-file (file-declaration &rest bodyforms)
  "(with-open-file (stream-symbol pathname &rest options) BODY...)
Evaluate BODYFORMS with STREAM-SYMBOL bound to a port open to PATHNAME.
Options: :direction followed by :input (default) or :output or :io,
         :if-exists followed by :supersede (default) or :new-version
                             or :append or :overwrite or :error or nil
         :if-no-exists followed by :error (default) or :create or nil"
;;;  (let (((stream-symbol pathname . options) file-declaration)
  (let ((stream-symbol (car file-declaration))
        (pathname (cadr file-declaration))
        (options (cddr file-declaration))
        (bodyform (if (= (length bodyforms) 1)
                      (car bodyforms)
                    (cons 'progn bodyforms))))
    `(let ((,stream-symbol (open ,pathname ,@options)))
       (unwind-protect
           ,bodyform
         (close ,stream-symbol)))))

(defmacro with-lines-from-file (declarations &rest bodyforms)
  "(with-lines-from-file (line-symbol pathname &optional chomp) body ...)
Evaluate BODYFORMS with LINE-SYMBOL bound to the lines of file PATHNAME
in sequence. On failure to open the file, an error is raised.
Instead of a pathname, an already open input port can be used as well,
but that port will be closed afterwards.
If &optional CHOMP is non-nil, the newline characters are removed from
the lines."
  (let ((line-symbol (car declarations))
        (pathname (cadr declarations))
        (chomp (caddr declarations))
        (stream-symbol (gensym)))
    `(with-open-file (,stream-symbol ,pathname)
       (let (,line-symbol)
         (while (setq ,line-symbol (read-line ,stream-symbol nil nil ,chomp))
           ,@bodyforms)))))

(defmacro debug-vars (&rest vars)
  (let ((format-string "DBG"))
    (dolist (var vars)
      (setf format-string (string format-string (format nil " ~A: ~~A;" var))))
    (setf format-string (string format-string "\n"))
    `(format t ,format-string ,@vars)))

(defmacro declare (&rest declarations)
  "Declare variable types by making type assertions.
A declaration like (declare (number n m) (string name) (symbol a b c))
generates the corresponding type assertions for the named variables and
their respectively declared types. For the available type symbols see
the variable *object-types*."
  (let (assertions)
    (dolist (d declarations)
      (let (((type . vars) d))
        (assert (member type *object-types*)
                (string "invalid type in declare: " type))
        (let ((pred (intern (string type "p"))))
          (dolist (var vars)
            (push `(assert (,pred ,var)
                           (format nil "declared ~A as ~A, but has ~A value"
                                   ',var ',type (type-of ,var)))
                  assertions)))))
    (if (> (length assertions) 1)
        (cons 'progn (nreverse assertions))
      (car assertions))))

(defmacro prog2 (first-form result-form &rest otherforms)
  "Evaluate all forms and return the value of the second."
  (if otherforms
      `(progn ,first-form (prog1 ,result-form ,@otherforms))
    `(progn ,first-form ,result-form)))

(defmacro with-input-from-string (control &rest bodyforms)
  "(with-input-from-string (variable string-value) bodyforms ...)
Bind variable to a stream make from string-value and evaluate `bodyforms`."
  (let (((symbol string-value) control))
    `(let ((,symbol (make-string-input-stream ,string-value)))
       ,@bodyforms)))

(defmacro with-output-to-string (control &rest bodyforms)
  "(with-output-to-string (variable) bodyforms ...)
Bind variable to an output stream and evaluate `bodyforms`;
return the content written to the output stream as a string."
  (let (((variable) control))
    `(let ((,variable (make-string-output-stream)))
       ,@bodyforms
       (get-output-stream-string ,variable))))
