;; Project Euler problem 4

(defun palindrome-p (n)
  (let ((digits (split-string (princs n) "")))
    (equal digits (reverse digits))))

(defun eu4 ()
  (let ((n 1)
        (largest 0))
    (while (< n 1000)
      (let ((m 1))
        (while (< m 1000)
          (let ((prod (* n m)))
            ;; (format t "n: %d m: %d\n" n m)
            (when (palindrome-p prod)
              (when (> prod largest)
                (print prod)
                (setf largest prod))))
          (incf m))
        (incf n)))
    largest))

;; result 906609
