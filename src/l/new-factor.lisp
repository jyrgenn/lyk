
(defvar *the-primes* '(2 3)
  "The ever-growing consecutive list of prime numbers.")

(defvar last-pair (last *the-primes*)
  "The last cons cell of the prime number list.")

(defun append-prime (new-prime)
  "Append a new prime to the prime number list."
  (let ((new-pair (list new-prime)))
    (setf (cdr last-pair) new-pair)
    (setf last-pair new-pair)))

(defun return-existing-primes-func ()
  "Return a function that returns the existing primes (and nil when done)."
  (let ((pos *the-primes*))
    (lambda ()
      (when pos
        (prog1
            (car pos)
          (setf pos (cdr pos)))))))

(defun try-candidate (candidate)
  "Try a prime number candidate. Return t if it is prime, nil else."
  (let ((limit (isqrt candidate))
        (divisor 0)
        (prime-gen (return-existing-primes-func)))
    (while (progn (setf divisor (prime-gen))
                  (<= divisor limit))
      (when (zerop (% candidate divisor))
        (return nil)))
    t))


(defun expand-primes ()
  "Expand the list of prime numbers by one and return the new prime."
  (let ((candidate (+ 2 (car last-pair))))
    (while (not (try-candidate candidate))
      (incf candidate 2))
    (append-prime candidate)
    candidate))

;; defun a println function in case we don't have one
(when (not (fboundp 'println))
  (defun println (&rest args)
    (while args
      (princ (car args))
      (princ " ")
      (setf args (cdr args)))
    (terpri)))

(defun next-prime-func ()
  "Return a function that, when called, returns all prime numbers, in order.
Well, in theory."
  (let ((pos *the-primes*))             ; pointer into *the-primes*, moving
    (lambda ()
      (if pos
          (prog1
              (car pos)
            (setf pos (cdr pos)))
          (expand-primes)))))

(defun factors (n)
  "Return a list of the prime factors of `n`."
  (let ((factors ())
        (limit (isqrt n))
        (next-p (next-prime-func)))
    (while (> n 1)
      (let ((divisor (next-p)))
        (when (> divisor limit)
          (setf divisor n))
        (when (zerop (% n divisor))
          (setf factors (cons divisor factors))
          (setf n (/ n divisor)))))
    (nreverse factors)))

