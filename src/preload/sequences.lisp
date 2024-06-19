
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


      
