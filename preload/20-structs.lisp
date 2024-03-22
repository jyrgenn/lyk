;; structs, not too dissimilar from CL's

;; (defstruct tag [docstring] field1 field2 ...)
;; Example:
;;     (defstruct hcoord "homogenous coordinates" x y z m)
;;   results in
;;     (sys:def-struct-type hcoord "homogenous coordinates" x y z m)
;;     (defun make-hcoord (&rest args)
;;       (apply #'sys:make-struct 'hcoord args))
;;     (defun hcoord-x (s)
;;       (sys:get-struct-slot s 0)
;;     (defun hcoord-y (s)
;;       (sys:get-struct-slot s 1)
;;     (defun hcoord-z (s)
;;       (sys:get-struct-slot s 2)
;;     (defun hcoord-m (s)
;;       (sys:get-struct-slot s 3)
;;   and setf operations for each

;; (defmacro defstruct (tag &rest args)
;;   "(defstruct tag [docstring] field1 field2 ...)
;; Define a struct type named TAG with optional DOCSTRING and named fields.
;; Together with that struct type, a constructor macro make-TAG is defined
;; as well as accessor macros with names TAG-field1, TAG-field2, etc.
;; These accessor macros can be used as places with setf.

;; The constructor macro is called like this:
;;   (make-TAG :field1 value1 :field2 value2 ...)
;; The order of the field keywords is irrelevant. Every field that is not
;; addressed with its keyword and a value will be initialized to nil.

;; The accessor macros take a struct object of this type as their only
;; argument."
;;   (let ((docstring (car args))
;;         (maker-sym (intern (string "make-" tag))))
;;     (if (stringp docstring)
;;         (pop args)
;;       (setf docstring ""))
;;     (let ((arg-index 0)
;;           (maker-docstring
;;            (format nil "Make a struct of type %s.\nSlot keywords:" tag))
;;           defs)
;;       (dolist (slot args)
;;         (let* ((funsym (intern (string tag "-" slot)))
;;                (docstring
;;                 (format nil
;;                         "Get the value of the %s slot of a struct of type %s."
;;                         slot tag)))
;;           (setf maker-docstring (string maker-docstring " :" slot))
;;           (push `(defmacro ,funsym (s)
;;                    ,docstring
;;                    `(sys:get-struct-slot ,(list 'unquote 's) ,arg-index))
;;                 defs)
;;           (eval `(defsetf ,funsym (struct value)
;;                    (sys:set-struct-slot struct ,arg-index value))))
;;         (incf arg-index))
;;       `(progn (sys:def-struct-type ,tag ,docstring ,@args)
;;               (defmacro ,maker-sym (&rest make-args)
;;                 ,maker-docstring
;;                 `(sys:make-struct ',tag ,@(list 'unquote 'make-args)))
;;               ,@defs
;;               ',tag))))
