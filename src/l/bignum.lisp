;;; beginnings of a bignum system

(defun new-bign (places)
  "Create a new bignum vector of the size PLACES."
  (make-vector places 0))

(defun bign-normalize (v)
  "Normalize a result, i.e. do all the carry operations."
  (let ((elems (elements v))
        (carry 0)
        result)
    (while elems
      (let ((el (+ carry (pop elems))))
        (push (mod el 10) result)
        (setf carry (div el 10))))
    (while (> carry 0)
      (push (mod carry 10) result)
      (setf carry (div carry 10)))
    (setf result (nreverse result))
    (make-vector (length result) (lambda () (pop result)))))
        
(defun bign-mult (a b)
  "Multiply two bignums and return the result."
  (let* ((lena (bign-len a))
         (lenb (bign-len b))
         (prod (new-bign (+ lena lenb))))
    (dotimes (i lena)
      (dotimes (j lenb)
        (let ((pplace (+ i j))
              (pp (* (a i) (b j))))
          (incf (vector-get prod pplace) pp))))
    (bign-normalize prod)))

(defun bign-len (a)
  "Return the significant length (without leading zeros) of bignum A."
  (let ((len (length a)))
    (while (and (> len 1) (zerop (elt a (1- len) 0)))
      (decf len))
    len))

(defun bign-n2v (n)
  (apply #'vector (nreverse (map #'read (split-string (string n) "")))))

(defun bign-fact (n)
  "Return the bignum factorial of N (a Lisp number)."
  (if (< n 2)
      #(1)
    (bign-mult (bign-n2v n)
               (bign-fact (1- n)))))

(defun bign-add (a b)
  (let* ((places (max (bign-len a) (bign-len b)))
         (s (make-vector (1+ places) 0)))
    (dotimes (i places s)
      (let ((sum (+ (elt a i 0) (elt b i 0) (elt s i 0))))
        (if (< sum 10)
            (s i sum)
          (s i (mod sum 10))
          (s (1+ i) 1))))))

(defun bign-len (a)
  (let ((len (length a)))
    (while (and (> len 1) (zerop (elt a (1- len) 0)))
      (decf len))
    len))

(defun bign-v2n (v)
  (read (join "" (nreverse (elements v)))))

(defun bign-n2v (n)
  (apply #'vector (nreverse (map #'read (split-string (string n) "")))))

