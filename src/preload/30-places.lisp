;;; places.lisp -- place-based setter macros

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
