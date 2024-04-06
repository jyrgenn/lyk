;;; have a function that throws an error at a specified recursion depth

(defun error-in-level (level n &optional depth)
  "Throw an error at a specified level; also calculate the faculty of n
(as far as it comes)."
  (when (and depth (= depth level))
    (error "error in level:" level))
  (if (zerop n)
      1
      (* n (error-in-level level (1- n) (+ (or depth 1) 1)))))
  
