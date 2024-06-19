
(let ((a 'lexical))                     ; binding (1)
  (let ((f (lambda () (print a))))
    (let ((a 'dynamic))                 ; binding (2)
      (funcall f))))
