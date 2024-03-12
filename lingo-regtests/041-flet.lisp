(require 'regtests)

;; (24) - bug: "; redefining builtin function compare" on *leaving* an
;;        flet where compare was defined; and it wasn't defined before,
;;        let alone a builtin

(fmakunbound 'triple-this)

(test-is "flet triple-this" (flet ((triple-this (lambda (n) (* 3 n))))
                               (triple-this 115))
         345)

(fmakunbound 'my-fac)
(test-is "flet recurse" (flet ((my-fac (lambda (n)
                                         (if (<= n 1)
                                             1
                                           (* n (my-fac (1- n)))))))
                          (my-fac 7))
         5040)

(fmakunbound 'drec1)
(fmakunbound 'drec2)

(test-is "flet mutrecurse" (flet ((drec1 (lambda (n)
                                           ;;(print (list 'drec1 n))
                                           (if (oddp n)
                                               (1+ n)
                                             (drec2 n))))
                                  (drec2 (lambda (n)
                                           ;;(print (list 'drec2 n))
                                           (if (evenp n)
                                               (if (< n 10)
                                                   n
                                                 (drec1 (/ n 2)))
                                             12))))
                             (drec2 112))
         8)


(done-testing)
