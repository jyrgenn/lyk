;;; Project Euler problem 9

(defparameter limit 1000)

(defun sq (n) (* n n))

(defun eu9 ()
  (let ((c (- limit 3))
        result)
    (while (> c (/ limit 3))
      (let ((a 1))
        (while (< a (/ c 2))
          (let ((b (- limit a c)))
            (when (< a b c)
              ;; (format t "%d + %d + %d = %d\n" a b c limit)
              (when (= (+ (sq a) (sq b)) (sq c))
                (setf result (list a b c)))))
          (incf a)))
      (decf c))
    (list-bind ((a b c) result)
      ;; (format t "%d^2 + %d^2 = %d^2 && %d + %d + %d = %d\n"
      ;;         a b c a b c limit)
      ;; (format t "prod = %d\n" (* a b c))
      (* a b c))))

;; result 31875000
