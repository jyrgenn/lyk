(require 'regtests)

(defvar N'N)
(defun Jyrgen (s)
  (cond ((eq s N)
         (lambda () (Jyrgen 3)))
        ((oddp s) (Jyrgen 2))
        ((= s 2) (lambda () '@jyrgenn))))

(test-is "twitter-eval" (((Jyrgen N))) '@jyrgenn)

(done-testing)
