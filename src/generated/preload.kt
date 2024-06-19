// generated preload string -- DO NOT EDIT!

package org.w21.lyk

val preload_code = """
;#file preload/00-start.lisp
(debug preload "preload/00-start.lisp") ;;; Lisp code integrated into the interpreter and evaluated on startup

;; moved the types list to generated/10-types.lisp

;#file preload/20-macros.lisp
(debug preload "preload/20-macros.lisp") 
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

(defmacro dolist (cvars &rest bodyforms)
  "(dolist (var listform &optional resultform) &rest bodyforms)"
  (let (((var listform resultform) cvars)
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

;#file preload/30-places.lisp
(debug preload "preload/30-places.lisp") ;;; places.lisp -- place-based setter macros

(defmacro incf (place &optional delta)
  "Increment number-valued PLACE by DELTA (or 1); return new value."
  (if delta
      `(setf ,place (+ ,place ,delta))
      `(setf ,place (1+ ,place))))

(defmacro decf (place &optional delta)
  "Decrement number-valued PLACE by DELTA (or 1); return new value."
  (if delta
      `(setf ,place (- ,place ,delta))
    `(setf ,place (1- ,place))))

(defmacro push (element place)
  "Prepend ITEM to the list stored in PLACE and store the result in PLACE.
Return the new value of PLACE."
  (let ((elsym (gensym "el"))
        (newsym (gensym "new")))
    `(let* ((,elsym ,element)
            (,newsym (cons ,elsym ,place)))
       (setf ,place ,newsym))))
       
(defmacro pop (place)
  "Return the car of the value of PLACE; store the cdr of the value into PLACE."
  (let ((placesym (gensym "place"))
        (newsym (gensym "new")))
    `(let* ((,placesym ,place)
            (,newsym (cdr ,place)))
       (setf ,place ,newsym)
       (car ,placesym))))

;; The setf macro is much more readable with the operator/expansion
;; pairs factored out into a lookup table. arg1 and arg2 are the first
;; and second argument, respectively, of the accessor function, like
;; in (elt sequence index), where the sequence is arg1 and the index
;; is arg2. value is the value to be set.

(defparameter *setf-update-table* #:())

(defmacro defsetf (access-fn update-fn-or-lambda-list &rest forms)
  "Define an update-function for an access-function, to be used by setf.
Short form:
    (defsetf access-fn update-fn)
where both ACCESS-FN and UPDATE-FN are function symbols.

Long form:
    (defsetf access-fn (lambda-list) forms ...)
defines an update function with the parameters specified in the
LAMBDA-LIST, and the forms executed for update in a body where
these parameters are bound accordingly.

In both cases the update function is called with the arguments of the access
function plus the value argument."
  `(progn
     ,(if (symbolp update-fn-or-lambda-list)
          (if forms
              (error "defsetf: two-symbol form called with forms argument(s)")
            `(table-put *setf-update-table*
                        ',access-fn ',update-fn-or-lambda-list))
        `(table-put *setf-update-table*
                    ',access-fn (lambda ,update-fn-or-lambda-list ,@forms)))
      ',access-fn))

(defsetf car rplaca-ret-value)
(defsetf cdr rplacd-ret-value)
(defsetf caar (l value) (rplaca-ret-value (car l) value))
(defsetf cadr (l value) (rplaca-ret-value (cdr l) value))
(defsetf cdar (l value) (rplacd-ret-value (car l) value))
(defsetf cddr (l value) (rplacd-ret-value (cdr l) value))
(defsetf caaar (l value) (rplaca-ret-value (caar l) value))
(defsetf caadr (l value) (rplaca-ret-value (cadr l) value))
(defsetf cadar (l value) (rplaca-ret-value (cdar l) value))
(defsetf caddr (l value) (rplaca-ret-value (cddr l) value))
(defsetf cdaar (l value) (rplacd-ret-value (caar l) value))
(defsetf cdadr (l value) (rplacd-ret-value (cadr l) value))
(defsetf cddar (l value) (rplacd-ret-value (cdar l) value))
(defsetf cdddr (l value) (rplacd-ret-value (cddr l) value))
(defsetf caaaar (l value) (rplaca-ret-value (caaar l) value))
(defsetf caaadr (l value) (rplaca-ret-value (caadr l) value))
(defsetf caadar (l value) (rplaca-ret-value (cadar l) value))
(defsetf caaddr (l value) (rplaca-ret-value (caddr l) value))
(defsetf cadaar (l value) (rplaca-ret-value (cdaar l) value))
(defsetf cadadr (l value) (rplaca-ret-value (cdadr l) value))
(defsetf caddar (l value) (rplaca-ret-value (cddar l) value))
(defsetf cadddr (l value) (rplaca-ret-value (cdddr l) value))
(defsetf cdaaar (l value) (rplacd-ret-value (caaar l) value))
(defsetf cdaadr (l value) (rplacd-ret-value (caadr l) value))
(defsetf cdadar (l value) (rplacd-ret-value (cadar l) value))
(defsetf cdaddr (l value) (rplacd-ret-value (caddr l) value))
(defsetf cddaar (l value) (rplacd-ret-value (cdaar l) value))
(defsetf cddadr (l value) (rplacd-ret-value (cdadr l) value))
(defsetf cdddar (l value) (rplacd-ret-value (cddar l) value))
(defsetf cddddr (l value) (rplacd-ret-value (cdddr l) value))
(defsetf elt setelt)
(defsetf vector-get vector-set)
(defsetf aref vector-set)
(defsetf table-get table-put)
(defsetf symbol-function fset)
(defsetf symbol-value set)
(defsetf get put)
(defsetf nth (n l value) (rplaca-ret-value (nthcdr n l) value))

(defmacro setf (place value)
  "Set field at PLACE to VALUE."
  (cond ((symbolp place)
         `(setq ,place ,value))
        ((consp place)
         (let* ((access-fn (car place))
                (args (deep-copy-cons (cdr place)))
                (update-fn (table-get *setf-update-table* access-fn)))
           (if update-fn
               `(,update-fn ,@args ,value)
             (error "no setf expansion found for place" place))))
        (t (error "no setf expansion for place" place))))

;#file preload/40-macros-using-places.lisp
(debug preload "preload/40-macros-using-places.lisp") ;; these must come after the places

(defmacro for (params &rest bodyforms)
  "(for (var from to [step [test]])
The for loop uses `var' as the counter variable, starting with `from',
adding `step' to `var' after each run, ending when `(test var to)' no
longer is true. The default step is 1; the default test is #'<."
  (let (((var from to step test) params))
    (setq step (or step 1))
    (setq test (or test #'<))
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


;#file preload/aliases.lisp
(debug preload "preload/aliases.lisp") 
(fset 'quit #'exit)
(fset 'string-concat #'string)
(fset 'map #'mapcar)
(fset 'λ #'lambda)
(fset 'eql #'eq)

(fset 'file-namestring #'basename)
(fset 'directory-namestring #'dirname)

;; (fset 'namestring 'normalize-pathname)

(fset 'char-code #'char-int)

;;;EOF

;#file preload/copy.lisp
(debug preload "preload/copy.lisp") ;; copy a structure

(defun deep-copy (ob)
  "Return a deep copy of object ob."
  (let ((copy-function (or (table-get *deep-copy-function-table* (type-of ob))
                           #'identity)))
    (assert copy-function
            (format nil "copy function missing for %s (%s)"
                    ob (type-of ob)))
    (copy-function ob)))

(defun deep-copy-cons (p)
  "Return a deep copy of cons P.
See the documentation of function #'copy for details."
  (if (consp p)
      (cons (deep-copy (car p)) (deep-copy (cdr p)))
    p))

(defun deep-copy-vector (v)
  "Return a deep copy of vector V.
See the documentation of function #'copy for details."
  (let ((elems (elements v))
        (newelems ()))
    (apply #'vector (dolist (el elems (nreverse newelems))
                      (push (deep-copy el) newelems)))))

(defun deep-copy-table (tbl)
  "Return a deep copy of table TBL.
See the documentation of function #'copy for details."
  (let (newconses)
    (apply #'make-table (dolist (cons (table-conses tbl) (nreverse newconses))
                          (push (deep-copy cons) newconses)))))

;; (defun deep-copy-struct (s)
;;   "Return a deep copy of struct S.
;; See the documentation of function #'copy for details."
;;   (let* (((tag nil . slots) (get-struct-type s))
;;          (nslots (length slots))
;;          (values (map (lambda (n)
;;                         (get-struct-slot s n))
;;                       (iota nslots)))
;;          (newstruct (make-struct tag)))
;;     (dotimes (i nslots)
;;       (set-struct-slot newstruct i (copy (pop values))))
;;     newstruct))

(defvar *deep-copy-function-table*
  (make-table (cons 'cons #'deep-copy-cons)
              (cons 'vector #'deep-copy-vector)
              (cons 'table #'deep-copy-table)
              ;; (cons 'struct #'deep-copy-struct)
              )
  "Copy functions per object type.")


;#file preload/hooks.lisp
(debug preload "preload/hooks.lisp") ;;; more hook things

(defvar lyk-rc-file (expand-file-name "~/.lykrc")
  "User lyk startup file")

(defun load-lyk-rc-file (&rest ignored)
  "Load lyk-rc-file, if it exists."
  (unless (cdr (assoc 'no-user-startup (lyk-command-options)))
    (errset (load lyk-rc-file :error nil) nil)))

(add-hook-function '*startup-hook*
                   #'load-lyk-rc-file)

;#file preload/lists.lisp
(debug preload "preload/lists.lisp") 
(defun remove-if (pred l)
  "Remove items from list L for which predicate PRED is true."
  (if (null l)
      l
    (let ((first (car l))
          (rest-result (remove-if pred (cdr l))))
      (if (pred first)
          rest-result
        (cons first rest-result)))))

(defun filter (predicate l)
  "Return a list of elements from list L for which PREDICATE is true."
  (let ((lc (list-collector)))
    (dolist (elem l)
      (when (funcall predicate elem)
        (lc elem)))
    (lc)))


(defun reduce (func l)
  "Reduce a list L to a single value using FUNC and return the value.
This is done by continually replacing the first two elements of L by the
result of applying FUNC to both, until there is only one element left."
  (if (cdr l)
      (reduce func (cons (func (car l) (cadr l))
                         (cddr l)))
    (car l)))


(defun make-list (n el)
  "Return a list of length `n` with elements `el`.
`el` may be a parameter-less function; in that case it is called for
 each place on the list, and its return value is filled in as the
element of the list."
  (let ((lc (list-collector)))
    (dotimes (i n (lc))
      (if (functionp el)
          (lc (funcall el))
          (lc el)))))


(defun iota (count &optional (start 0) (step 1))
  "Return a list of numbers of length COUNT.
The list starts with optional START (default 0), with each element being
bigger than the previous one by optional STEP (default 1)."
  (let ((lc (list-collector)))
    (while (>= (decf count) 0)
      (lc start)
      (incf start step))
    (lc)))

(defun range (limit-or-start &optional limit-or-not (step 1))
  "Return a list of numbers similar to the corresponding Python function.
Invoked as (range <limit>), it starts at 0 and ends before limit.
Invoked as (range <start> <limit>), it starts at start and ends before limit.
Optional STEP is the step width between the list members, defaulting to 1.
It is an error if any of the values is not an integer."
  (let ((start 0)
        (limit limit-or-start))
    (when limit-or-not
      (setf start limit-or-start)
      (setf limit limit-or-not))
    (unless (and (integerp start) (integerp limit) (integerp step))
      (error "range: one of the arguments is not an integer"))
    (let ((lc (list-collector)))
      (if (>= step 0)
          (while (< start limit)
            (lc start)
            (incf start step))
        (while (> start limit)
          (lc start)
          (incf start step)))
      (lc))))

;#file preload/numeric.lisp
(debug preload "preload/numeric.lisp") (defun pow (base power)
  "Return BASE to the power of (integer) POWER."
  (if (zerop power)
      1
    (* base (pow base (1- power)))))

;; (defun evenp (n)
;;   "Return a true value iff the number is an even integer."
;;   (and (integerp n)
;;        (zerop (% n 2))))

;; (defun oddp (n)
;;   "Return a true value iff the number is an odd integer."
;;   (and (integerp n)
;;        (null (zerop (% n 2)))))

(defun float (number)
  "Return the argument as a float.
This is a null operation for a number argument (as all numbers are floats).
For any other argument type, this function raises an error."
  (if (numberp number)
      number
    (error "not a number: %s" number)))

(defun gcd (n1 n2)
  "Return the greatest common divisor of the arguments."
  (let ((gcd 1)
        (factors1 (factor n1))
        (factors2 (factor n2)))
    (while (and factors1 factors2)
      (let ((f1 (car factors1))
            (f2 (car factors2)))
        (if (= f1 f2)
            (progn (setf gcd (* gcd f1))
                   (pop factors1)
                   (pop factors2))
          (if (< f1 f2)
              (pop factors1)
            (pop factors2)))))
    gcd))

(defun lcm (&rest args)
  "Return the least common multiple of all arguments."
  (let* ((f-merge
          #'(lambda(f1 f2)
              (cond ((null f1) f2)
                    ((null f2) f1)
                    (t (let ((c1 (car f1))
                             (c2 (car f2)))
                         (cond ((< c1 c2) (cons c1 (f-merge (cdr f1) f2)))
                               ((< c2 c1) (cons c2 (f-merge f1 (cdr f2))))
                               (t (cons c1 (f-merge (cdr f1) (cdr f2))))))))))
         (flists (mapcar #'factor args))
         (merged))
    (while flists
      (setf merged (f-merge merged (pop flists))))
    (apply #'* merged)))

(defun seq (start end &optional step)
  "Return a list of numbers from START to END.
Optional STEP (default 1 or -1) specifies the step-width."
  (if (= start end)
      (list start)
    (let* (result
           (step (or step (signum (- end start))))
           (cmp (if (< step 0) #'>= #'<=)))
      (while (cmp start end)
        (push start result)
        (incf start step))
      (nreverse result))))

(defun prime-numbers (start end)
  "Return a list of consecutive prime numbers with start <= prime < end."
  (let ((lc (list-collector))
        (current (1- start)))
    (while (< current end)
      (let ((nextp (next-prime current)))
        (when (< nextp end)
          (lc nextp))
        (setf current nextp)))
    (lc)))

;#file preload/other.lisp
(debug preload "preload/other.lisp") ;; smallish things that don't belong elsewhere

(defun arg-number-is-one (arg)
  "Iff ARG (or the number of its elements) is 1, return true."
  (let ((n (cond ((numberp arg) arg)
                 ((sequencep arg) (length arg)))))
    (= n 1)))
  
(defun plural-s (arg)
  "Return a plural-s or an empty string as appropriate for ARG.
If ARG is a number, return \"s\" if it is zero or greater than 1, but
an empty string if it is 1.
If ARG is a sequence, do the same for the number of it elements."
  (if (arg-number-is-one arg)
      ""
    "s"))

(defun plural-ies (arg)
  "Return a plural \"ies\" or singular \"y\" as appropriate for ARG.
If ARG is a number, return \"ies\" if it is zero or greater than 1, but
\"y\" if it is 1.
If ARG is a sequence, do the same according to the number of it elements."
  (if (arg-number-is-one arg)
      "y"
    "ies"))

(defun all-symbols (&optional comparison-function)
  "Return a list of all symbols, alphabetically sorted.
If optional `comparison-function` is supplied , use it to sort the list.
if `comparison-function` is t, sort the list alphabetically."
  (let (l)
    (do-symbols (sym)
      (push sym l))
    (cond ((null comparison-function) l)
          ((eq comparison-function t) (sort l #'<))
          ((functionp comparison-function) (sort l comparison-function))
          (t (error "all-symbols: comparison-function ~S is not t or a function"
                    comparison-function)))))

(defun function-symbols ()
  "Return a list of all function symbols."
  (filter #'fboundp (all-symbols t)))


(defmacro no-warnings-as-errors (&rest bodyforms)
  "Evaluate `bodyforms` with *warnings-as-errors* set to nil.
After that, restore the original value."
  `(let ((*previous-warnings-as-errors* (warnings-as-errors nil)))
     (unwind-protect
          (progn
            ,@bodyforms)
       (warnings-as-errors *previous-warnings-as-errors*))))


(defun get-program-output (command &key capture-all input error-output
                                     (raise-error t)
                                     (in-shell t))
  "Run program `command` and return its standard output as a string.
Raise an error if the program returned a non-zero exit status and the
keyword argument :raise-error is true.

If &key argument :capture-all is true, capture the error output,
too, and return a list of exit status, standard output and error output
of the program run as strings."
  (let ((out (make-string-output-stream)))
    (if capture-all
        (let* ((err (make-string-output-stream))
               (status (run-program command
                                    :input input
                                    :output out
                                    :error-output err
                                    :raise-error raise-error
                                    :in-shell in-shell)))
          (list status
                (get-output-stream-string out)
                (get-output-stream-string err)))
        (run-program command
                     :input input
                     :output out
                     :error-output error-output
                     :raise-error raise-error
                     :in-shell in-shell)
        (let ((result (get-output-stream-string out)))
          result))))
          
(defun select-string-from-prefix (prefix selections)
  "If `prefix` is a prefix of just one of `selections`, return that one.
Otherwise, return nil if it matches none, a list if it matches more than one."
  (let (matches)
    (dolist (item selections)
      (when (string-starts-with item prefix)
        (push item matches)))
    (if (null matches)
        nil
        (if (null (cdr matches))
            (car matches)
            matches))))

(defun example-startup-hook-function (load-files expr-list other-args)
  "Print the arguments passed to lyk.
This is an example for a startup hook function."
  (format t "; startup: load ~A -e ~A args ~A~%"
          load-files expr-list other-args))

;; (set-hook-function '*startup-hook* #'example-startup-hook-function)
                   

;;; EOF

;#file preload/sequences.lisp
(debug preload "preload/sequences.lisp") 
(defun concatenate (result-type &rest seqs)
  (let ((type (or result-type (type-of (car seqs)))))
    (apply (symbol-function type)
           ;; nonchalantly exploiting the fact that
           ;; we have variadic creator functions of
           ;; just the type name for each of the
           ;; sequence types.
           (let (result)     
             (dolist (seq seqs (nreverse result))
               (doseq (el seq)
                 (push el result)))))))

(defun sort (seq &optional (pred #'<))
  "Sort sequence `seq` with predicate `pred` and return the result."
  (let ((combine (cond ((listp seq) #'list)
                       ((vectorp seq) #'vector)
                       ((stringp seq) #'string)
                       (t (error "sort not implemented for type %s"
                                 (type-of seq))))))
    (flet ((helper (seq)
             (if (< (length seq) 2)
                  (elements seq)
                (let ((pivot (elt seq 0))
                      l1 l2)
                  (doseq (el seq (nconc (helper l1)
                                        (list pivot)
                                        (helper l2)) 1)
                         (if (pred el pivot)
                             (push el l1)
                           (push el l2)))))))
      (apply combine (helper seq)))))


(defun member (item list &key (test #'eq))
  "Find first ITEM in LIST and return the tail of the list beginning with item.
Keyword :TEST specifies a test predicate function of two arguments to use
instead of eq."
  (if (null list)
      nil
    (if (funcall test item (car list))  ;be sure to use lexical var scope here!
        list
      (member item (cdr list) :test test))))

(defun member-if (predicate list)
  "Find first item in LIST for which PREDICATE is true
and return the tail of the list beginning with this item."
    (if (null list)
        nil
      (if (predicate (car list))
          list
        (member-if predicate (cdr list)))))

(defun member-if-not (predicate list)
  "Find first item in LIST for which PREDICATE is false
and return the tail of the list beginning with this item."
    (if (null list)
        nil
      (if (predicate (car list))
          (member-if-not predicate (cdr list))
        list)))


      

;#file preload/short-commands.lisp
(debug preload "preload/short-commands.lisp") ;;; short commands for the repl in table *repl-short-commands*
;;; entries are  :name => func

(defvar *repl-short-commands* (make-table)
  "The table (keyword => function) of short commands for the REPL.")


(defun maybe-run-short-command (expr)
  "Run a short command if `expr` is a keyword matching one.
Return true iff a short command was run or at least attempted."
  (unless (keywordp expr)
    (return nil))
  (unless (tablep *repl-short-commands*)
    (warning "*repl-short-commands* is not a table: ~A" *repl-short-commands*)
    (return nil))
  (setf + *the-non-printing-object*)
  (let ((cmd (select-string-from-prefix expr
                                        (table-keys *repl-short-commands*))))
    (cond ((null cmd) nil)
          ((listp cmd)
           (warning "`~A` matches multiple short commands: ~A"
		    expr (join cmd ", "))
	   (warning "type :help for more information")
           (return t)))
    (funcall (table-get *repl-short-commands* cmd))
    t))

(add-hook-function '*repl-interactive-input-hook*
                   #'maybe-run-short-command)


(defmacro define-repl-short-command (name docstring &rest bodyforms)
  "Define short command `name` for the repl with `docstring` and `bodyforms`.
`name` must be a :keyword."
  (unless (keywordp name)
    (error "short command name `~A` is not a keyword" name))
  `(table-put *repl-short-commands* ,name
              (lambda () ,docstring
                ,@bodyforms)))


(define-repl-short-command :replsyms
    "print the repl's special symbols and their values"
  (dolist (sym '(* ** *** + ++ +++ / // ///))
    (format t " ~3@A: ~A~%" sym (symbol-value sym))))

(define-repl-short-command :help
    "show help on short commands"
  (println "Defined short commands:")
  (dolist (name (sort (table-keys *repl-short-commands*)))
    (let ((func (table-get *repl-short-commands* name)))
      (when (functionp func)
        (format t "~17@A : ~A~%" name (function-docstring func)))))
  (terpri)
  (println
   "A unique command prefix is sufficient to call a command.
See macro `define-repl-short-command` on how to define short commands."))


(define-repl-short-command :explore
    "show hints for exploring the system"
  (format t "There are a few tools to help explore lyk:

  - `apropos` prints all known symbols that match a substring or a
    regular expression, together with the information if the symbol
    is bound to a function (builtin, lambda, or macro), has a
    variable binding, or properties.

        (apropos \"substr\")
	(apropos #/r.*exp/)

  - `doc` prints the function documentation for a symbol (or a
    function object) with synopsis, description, and place of
    definition.

        (doc 'apropos)

  - `describe` return an alist with an object's attributes.

        (describe 'doc)

  - The directory `~A/doc/` contains not a full
    documentation, but a few chapters describing some aspects of lyk.

  - `~A/doc/DOCSTRINGS.md` contains the docstrings
    of all functions, same as those printed interactively with `doc`.

And then, there is the source code, of course.~%"
          *lyk-install-directory* *lyk-install-directory*))

(define-repl-short-command :perfdata
    "show status counters since the start of the system"
  (let ((perfdata ))
    (format t ";")
    (dolist (item (system-perfdata))
      (format t " ~A ~A" (cdr item) (car item)))
    (terpri)))

(define-repl-short-command :exit "exit the system" (exit))

(defun print-file (pathname &optional (stream *standard-output*))
  "Print the contents of file `pathname` to `stream`."
  (with-lines-from-file (line pathname)
    (princ line stream)))

(define-repl-short-command :license "show the lyk license"
  (print-file (string *lyk-install-directory*
                      "/" "LICENSE")))

(define-repl-short-command :jline-license "show the jline license"
  (print-file (string *lyk-install-directory*
                      "/jline/LICENSE")))

;;; EOF

;#file preload/time.lisp
(debug preload "preload/time.lisp") 
(defun get-iso-time (&optional (universal-time (get-universal-time-ns))
                               &key time-zone (long-form t) fractions
                               append-blank)
  "Format the UNIVERSAL-TIME according to ISO 8601 and return the string.
The default is the current time.
Key :long-form may be true for long form (default) or nil for short.
Key fractions may be :ms, :us, or :ns for milliseconds, microseconds, or
nanoseconds, respectively."
  (let ((format-string
         ;; long/short format:fractions:blank
         (format nil "%s%s%s"
                 (if long-form "%Y-%m-%d %H:%M:%S" "%Y%m%d:%H%M%S")
                 (if fractions
                     (or (#:((:ms . ".%i")(:us . ".%J")(:ns . ".%K")) fractions)
                         (error "get-iso-time: unknown fraction specifier `%s'"
                                fractions))
                   "")
                 (if append-blank " " ""))))
    (format-universal-time format-string universal-time)))

            

;#file generated/10-types.lisp
(debug preload "generated/10-types.lisp") 
;;; all Lisp object types known to lyk

(defvar *object-types*
  '(
    builtin
    char
    cons
    console-reader-stream
    environment
    error-object
    file-io-stream
    file-reader-stream
    file-writer-stream
    lambda
    macro
    number
    regexp
    string
    string-reader-stream
    string-writer-stream
    symbol
    table
    vector
   )
  "List of types known to the system.
These are the leaves of the object type tree; there are also the type
categories `function`, `stream`, and sequence, which don't appear here,
but for which type predicates exist.")
(put '*object-types* t '*system*)

"""
// EOF
