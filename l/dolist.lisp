
;; lingo has a dolist builtin, but we could do this with a macro as well

(defmacro dolist (cvars &rest bodyforms)
  (let ((var (car cvars))
        (listform (cadr cvars))
        (resultform (caddr cvars))
        (listsym (gensym)))
    `(let ((,listsym ,listform))
       (while ,listsym
         (let ((,var (pop ,listsym)))
           ,@bodyforms))
       ,resultform)))

;; examples from CLHS

(let ((temp-two '()))
  (dolist (temp-one '(1 2 3 4) temp-two)
    (push temp-one temp-two)))

(let ((temp-two 0))
  (dolist (temp-one '(1 2 3 4))
    (incf temp-two))
  temp-two)

(dolist (x '(a b c d))
  (prin1 x)
  (princ " ")) 
