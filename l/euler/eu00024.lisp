;;; Project Euler problem 24

;; (defun remove-nth (n l)
;;   (if l
;;       (if (zerop n)
;;           (cdr l)
;;         (cons (car l) (remove-nth (1- n) (cdr l))))
;;     nil))

;; (defun all-permutations (l)
;;   (let ((len (length l)))
;;     (if (> len 1)
;;         (let (result)
;;           (dotimes (n len (nreverse result))
;;             (let* ((el (nth n l))
;;                    (others (remove-nth n l))
;;                    (perms (all-permutations others)))
;;               (dolist (l1 perms)
;;                 (push (cons el l1) result)))))
;;       (list l))))

(defun remove-nth-string (n s)
  (let ((result ""))
    (dotimes (i (length s) result)
      (unless (= i n)
        (setq result (string result (elt s i)))))))

(defun permute-string (s)
  (let ((len (length s)))
    (if (> len 1)
        (let (result)
          (dotimes (n len (nreverse result))
            (let* ((c (elt s n))
                   (others (remove-nth-string n s))
                   (perms (permute-string others)))
              (dolist (ls perms)
                (push (string c ls) result)))))
      (list s))))
                    
                  

(defun eu24 ()
  (let ((a "0123456789"))
    (elt (permute-string a) 999999)))

;; result 2783915460
