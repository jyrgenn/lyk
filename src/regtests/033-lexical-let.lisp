(require 'regtests)

(test-is "let nil" (let (a) a) nil)
(test-is "setq in let" (let (a) (setq a 115) a) 115)

(test-is "binding type"
         (let ((a 'lexical))                     ; binding (1)
           (let ((f (lambda () (print a devnull))))
             (let ((a 'dynamic))                 ; binding (2)
               (funcall f))))
         'lexical)

(done-testing)
