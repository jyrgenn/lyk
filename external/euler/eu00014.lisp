;;; Project Euler problem 14

(defun collatz-step (n)
  (if (evenp n)
      (/ n 2)
    (1+ (* n 3))))

(defun collatz-len (n)
  (let ((count 1))
    (while (not (= 1 n))
      (setf n (collatz-step n))
      (incf count))
    count))

(defun eu14 ()
  (let ((longest-seq 0)
        (longest-num 0))
    (for (n 1 1e6)
      (let ((len (collatz-len n)))
        (when (> len longest-seq)
          (setf longest-seq len)
          (setf longest-num n)
          ;; (format t "%d: %d\n" n len))))
    longest-num))    

;; result 837799
