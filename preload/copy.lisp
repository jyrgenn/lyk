;; copy a structure

(defun deep-copy-pair (p)
  "Return a deep copy of pair P.
See the documentation of function #'copy for details."
  (cons (copy (car p)) (copy (cdr p))))

(defun deep-copy-vector (v)
  "Return a deep copy of vector V.
See the documentation of function #'copy for details."
  (let ((elems (elements v))
        (newelems ()))
    (apply #'vector (dolist (el elems (nreverse newelems))
                      (push (copy el) newelems)))))

(defun deep-copy-table (tbl)
  "Return a deep copy of table TBL.
See the documentation of function #'copy for details."
  (let (newpairs)
    (apply #'make-table (dolist (pair (table-pairs tbl) (nreverse newpairs))
                          (push (copy pair) newpairs)))))

(defun deep-copy-struct (s)
  "Return a deep copy of struct S.
See the documentation of function #'copy for details."
  (let* (((tag nil . slots) (get-struct-type s))
         (nslots (length slots))
         (values (map (lambda (n)
                        (sys:get-struct-slot s n))
                      (iota nslots)))
         (newstruct (sys:make-struct tag)))
    (dotimes (i nslots)
      (sys:set-struct-slot newstruct i (copy (pop values))))
    newstruct))

(defvar *deep-copy-function-table*
  (make-table (cons 'pair #'deep-copy-pair)
              (cons 'vector #'deep-copy-vector)
              (cons 'table #'deep-copy-table)
              (cons 'struct #'deep-copy-struct))
  "Copy functions per object type.")

(defun copy (ob)
  "Return a deep copy of object OB.
The atoms contained in the copy will be the same as in OB, but all
structs, vectors, tables, and pairs will be copies of the original.
Sharing of structure will not be preserved. A circular argument will
make this function go into an endless loop."
  (let ((copy-function (*deep-copy-function-table* (type-of ob))))
    (if copy-function
        (copy-function ob)
      ob)))

