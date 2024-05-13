;; copy a structure

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

