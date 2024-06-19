(require 'regtests)

;; from the CLHS throw page, adapted

(test-is "catch CLHS throw 1" (catch 'result
                                (let ((i 0) (j 0))
                                  (while t
                                    (incf j 3)
                                    (incf i)
                                    (if (= i 3)
                                        (throw 'result (cons i j))))))
         '(3 . 9))

(test-is "catch CLHS throw 2" (catch :nil
                                (unwind-protect (throw :nil 1)
                                  (throw :nil 2)))
         2)

;; from the CLHS catch page, adapted

(test-is "catch CLHS catch 1" (catch 'dummy-tag 1 2 (throw 'dummy-tag 3) 4) 3)
(test-is "catch CLHS catch 2" (catch 'dummy-tag 1 2 3 4) 4)

(defun throw-back (tag) (throw tag t))
(test-is "catch CLHS catch 3" (catch 'dummy-tag (throw-back 'dummy-tag) 2) t)

;; Contrast behavior of this example with corresponding example of BLOCK.
(test-is "catch CLHS catch 4" (catch 'c
                                (flet ((c1 () (throw 'c 1)))
                                  (+ 3 (catch 'c
                                         (c1)
                                         (print 'unreachable)
                                         5))
                                  ))
         4)

;; unwind-protect in catch
(test-is "catch w/ unw/protect" (let ((n 3))
                     (catch 'tag
                       (let ((b 4))
                         (unwind-protect
                             (progn (incf b 2)
                                    (throw 'tag 'foo))
                           (setf n b))))
                     n)
         6)

;; outer catch
(test-is "catch throw outer" (catch :t1
                               (catch :t2
                                 (throw :t1 't1)
                                 (throw :t2 't2)))
         't1)

;; inner catch
(test-is "catch throw inner" (catch :t1
                               (catch :t2
                                 (throw :t2 't2)
                                 (throw :t1 't1)))
         't2)

;; error in catch (both handled by panic/recover)
(test-err "catch w/ error" (catch :t1
                             (error "this is not a love song"))
          #/this is not a love song/)

;;; we don't have a mechanism to test this -- we cannot catch a throw
;;; without catch, obviously
;; (test-err "catch-less throw" (let ((n 0))
;;                                (throw 'tag 13))
;;           #/throw without catch/)

;; errset in catch
(test-is "catch w/ errset" (catch 'foo
                             (errset (let ((n 12))
                                       (incf n)
                                       (throw 'foo n)))
                             *last-error*)
         13)                             

(done-testing)
