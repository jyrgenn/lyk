
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
