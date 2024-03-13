
(defvar *the-primes* '(2 3)
  "The ever-growing consecutive list of prime numbers.")

(defvar last-pair (last *the-primes*)
  "The last cons cell of the prime number list.")

(defun append-prime (new-prime)
  "Append a new prime to the prime number list."
  (let ((new-pair (list new-prime)))
    (rplacd last-pair new-pair)
    (setq last-pair new-pair)))

(defun return-existing-primes-func ()
  "Return a function that returns the existing primes (and nil when done)."
  (let ((pos *the-primes*))
    (lambda ()
      (if pos
          (prog1
              (car pos)
            (setq pos (cdr pos)))
          nil))))

(defun try-candidate (candidate)
  "Try a prime number candidate. Return t if it is prime, nil else."
  (let ((limit (isqrt candidate))
        (divisor 0)
        (prime-gen (return-existing-primes-func))
        (still-good t))
    (while (and still-good (<= divisor limit))
      (let ((divisor (prime-gen)))
        (when (zerop (% candidate divisor))
          (setq still-good nil))))
    still-good))


(defun expand-primes ()
  "Expand the list of prime numbers by one."
  (let ((candidate (car last-pair))
        found-one)
    (until found-one
      (setq candidate (+ candidate 2))
      (setq found-one (try-candidate candidate)))))
    

(defun next-prime-func ()
  "Return a function that, when called, returns all prime numbers, in order.
Well, in theory."
  (let ((pos *the-primes*))             ; pointer into *the-primes*, moving
    (lambda ()
      (unless pos
        (expand-primes))
      (prog1
          (car pos)
        (setq pos (cdr pos))))))

