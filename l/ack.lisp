;; Ackermann function

;;  A(m, n) =
;; \begin{cases}
;; n+1 & \mbox{if } m = 0 \\
;; A(m-1, 1) & \mbox{if } m > 0 \mbox{ and } n = 0 \\
;; A(m-1, A(m, n-1)) & \mbox{if } m > 0 \mbox{ and } n > 0.
;; \end{cases}

(defun ack (m n)
  "Ackermann function of M and N."
  (if (zerop m)
      (1+ n)
    (if (zerop n)
        (ack (1- m) 1)
      (ack (1- m) (ack m (1- n))))))
