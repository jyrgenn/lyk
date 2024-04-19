

(defmacro =/= (ob1 ob2)
  "Return true iff OB1 and OB2 are unequal (in terms of #'equal)."
  `(not (equal ,ob1 ,ob2)))

(defmacro dolist (cvars &rest bodyforms)
  "(dolist (var listform &optional resultform) &rest bodyforms)"
  (let ((var (car cvars))
        (listform (cadr cvars))
        (resultform (caddr cvars))
        (listsym (gensym)))
    `(let ((,listsym ,listform))
       (while ,listsym
         (let ((,var (pop ,listsym)))
           ,@bodyforms))
       ,resultform)))

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
         ,resultform))))

(defmacro for (params &rest bodyforms)
  "(for (var from to [step [test]])
The for loop uses `var' as the counter variable, starting with `from',
adding `step' to `var' after each run, ending when `(test var to)' no
longer is true. The default step is 1; the default test is #'<."
  (let ((var (pop params))
        (from (pop params))
        (to (pop params))
        (step (or (pop params) 1))
        (test (or (pop params) '<)))
    `(let ((,var ,from))
       (while (,test ,var ,to)
         ,@bodyforms
         (incf ,var ,step)))))

(defmacro with-gensyms (syms &rest body)
  "Run the BODY with the symbols in SYMS (a list) bound to gensyms.
This is meant to simplify macro definitions that would otherwise
use a
  (let ((param1 (gensym))
        (param2 (gensym)))
        ... )
    ,@body)
symbol definition chain explicitly."
  (let (decls)
    (while syms
      (push (list (pop syms) '(gensym)) decls))
    `(let ,decls
       ,@body)))

(defmacro aref (vector index)
  "Return the element of VECTOR at INDEX."
  `(vector-get ,vector ,index))

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
      (setf format-string (string format-string (format nil " %s: %%s;" var))))
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
                           (format nil "declared %s as %s, but has %s value"
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

