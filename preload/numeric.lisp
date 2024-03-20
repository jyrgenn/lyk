(defun pow (base power)
  "Return BASE to the power of (integer) POWER."
  (if (zerop power)
      1
    (* base (pow base (1- power)))))

(defun evenp (n)
  "Return a true value iff the number is an even integer."
  (and (integerp n)
       (zerop (% n 2))))

(defun oddp (n)
  "Return a true value iff the number is an odd integer."
  (and (integerp n)
       (null (zerop (% n 2)))))

(defun float (number)
  "Return the argument as a float.
This is a null operation for a number argument (as all numbers are floats).
For any other argument type, this function raises an error."
  (if (numberp number)
      number
    (error "not a number: %v" number)))

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
