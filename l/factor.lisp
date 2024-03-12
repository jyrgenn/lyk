(provide 'factor)

(when (not (fboundp 'builtin-factor))
  (fset 'builtin-factor #'factor))

(defvar *primes* '(2 3)
  "List of continuous prime numbers known so far") 

(defvar *last-prime-pair* (last *primes*)
  "Place to append new primes, updated as it goes")

(defvar next-p *primes*
  "state variable for (next-prime)")

(defun add-next-prime (n)
  (let ((newlast (list n)))
    (setf (cdr *last-prime-pair*) newlast)
    (setf *last-prime-pair* newlast)))

(defun highest-prime ()
  (car *last-prime-pair*))

(defun have-divisor (n)
  (let ((limit (isqrt n))
        (p *primes*)
        result)
    (while (and p (<= (car p) limit))
      (if (zerop (% n (car p)))
          (progn (setf result t)
                 (setf p nil)))
      (setf p (cdr p)))
    result))

(defun grow-primes ()
  (let ((candidate (+ (highest-prime) 2)))
    (while (have-divisor candidate)
      (incf candidate 2))
    (add-next-prime candidate)))

(defun reset-primes ()
  (setf next-p *primes*))

(defun next-prime ()
  (while (null (cdr next-p))
    (grow-primes))
  (let ((this-one (car next-p)))
    (setf next-p (cdr next-p))
    this-one))

(defun factor (n &optional print-factor)
  (reset-primes)
  (let ((limit (isqrt n))
        (working t)
        (result '()))
    (while working
      (let ((p (next-prime)))
        (if (<= p limit)
            (while (and (> n 1)
                        (zerop (% n p)))
              (setf result (cons p result))
              (if print-factor
                  (format t " %d" p))
              (setf n (/ n p)))
          (setf working nil))))
    (if (> n 1)
        (progn 
          (setf result (cons n result))
          (if print-factor
                  (format t " %d" n))))
    (if print-factor (terpri))
    (nreverse result)))

(defun factor-loop (&optional max pause)
  (let ((n 1))
    (setf pause (or pause 1))
    (while (or (null max) (<= n max))
      (let ((factors (factor n)))
        (if (or (not (zerop pause)) (zerop (% n 1000)))
            (format t "%d: %v\n" n factors))
        (sleep pause))
      (incf n))))

(if sys:args
    (let ((max (read (car sys:args)))
          (pause (read (car (cdr sys:args)))))
      (factor-loop max pause)))

(defun 101s (n)
  (let ((zeros ""))
    (while (> n 0)
      (setf zeros (string-concat zeros "0"))
      (decf n))
    (read (format nil "1%s1" zeros))))

(defun factor-101s (n)
  (let ((i 0))
    (while (<= i n)
      (let ((n (101s i)))
        (format t "%d: %v\n" n (factor n)))
      (incf i))))
