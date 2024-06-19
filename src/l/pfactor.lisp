
(defvar the-primes '(2 3))
(defvar last-pair (last the-primes))

(defun append-prime (new-prime)
  (let ((newpair (list new-prime)))
    (rplacd last-pair newpair)
    (setq last-pair newpair))
  new-prime)

(defun next-prime ()
  (let ((candidate (+ 2 (car last-pair))))
    (catch 'return
      (while t
        (catch 'divisible
          (let ((limit (sqrt candidate))
                (primes the-primes))
            (while primes
              (let ((prime (car primes)))
                (setq primes (cdr primes))
                (cond ((> prime limit)
                       (throw 'return (append-prime candidate)))
                      ((zerop (% candidate prime))
                       (throw 'divisible nil)))))))
      (setq candidate (+ 2 candidate))))))
        

(defun all-primes ()
  (let ((pos the-primes))
    (lambda ()
      (if (null pos)
          (next-prime)
        (let ((value (car pos)))
          (setq pos (cdr pos))
          value)))))

(defun prime-factors (n)
  (let ((factors (list-collector))
        (get-next (all-primes)))
    (catch 'break
      (while t
        (let ((p (get-next)))
          (while (and (> n 1)
                      (zerop (% n p)))
            (factors p)
            (setq n (/ n p)))
          (when (eq n 1)
            (throw 'break nil)))))
    (when (> n 1)
      (factors n))
    (factors)))
