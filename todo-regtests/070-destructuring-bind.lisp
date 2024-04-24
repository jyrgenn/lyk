(require 'regtests)

(test-is "dest let 0" (let ()
                        23)
         23)

(test-is "dest let 1" (let ((a 22))
                        (+ a a))
         44)

(test-is "dest let 2" (let (((a b c) '(3 4 5 6)))
                        (list a b c))
         '(3 4 5))

(test-is "dest let 3" (let (((a b c d) '(3 4 5)))
                        (list a b c d))
         '(3 4 5 nil))

(test-is "dest let 4" (let (((a (b c) d) '(3 (4 5) 6 7)))
                        (list a b c d))
         '(3 4 5 6))

(test-err "dest let 4E!" (let (((a (b c) d) '(3 4 5 6 7)))
                           (list a b c d))
          #/value structure error, not pair/)

(test-is "dest let 5" (let (((a b c . d) '(3 (4 5) 6 7 8 9)))
                        (list a b c d))
         '(3 (4 5) 6 (7 8 9)))

(defun return-a-list ()
  '(3 4 5))

(test-is "dest let 6" (let (((a . b) (return-a-list)))
                        (list a b))
         '(3 (4 5)))

(test-err "dest let 6E!" (let (((a . 119) (return-a-list)))
                           (list a b))
         #/non-symbol terminates varlist/)

(test-is "dest let 7" (let (((a b) (return-a-list)))
                        (list a b))
         '(3 4))

(test-is "dest let 8" (let (((a b c d) (return-a-list)))
                        (list a b c d))
         '(3 4 5 nil))

(test-is "dest let 9" (let ((a (sys:symbols))
                            (b (table-keys *setf-update-table*))
                            (c 1337))
                        (let (((a b c) '(3 4 5 6))
                              (b (list c c))
                              (c (* c c)))
                          (list a b c)))
         '(3 (1337 1337) 1787569))

(test-is "dest let* 0" (let* ()
                         23)
         23)

(test-is "dest let* 1" (let* ((a 22))
                         (+ a a))
         44)

(test-is "dest let* 2" (let* (((a b c) '(3 4 5 6)))
                         (list a b c))
         '(3 4 5))

(test-is "dest let* 3" (let* (((a b c d) '(3 4 5)))
                         (list a b c d))
         '(3 4 5 nil))

(test-is "dest let* 4" (let* (((a (b c) d) '(3 (4 5) 6 7)))
                         (list a b c d))
         '(3 4 5 6))

(test-err "dest let* 4E!" (let* (((a (b c) d) '(3 4 5 6 7)))
                            (list a b c d))
          #/value structure error, not pair/)

(test-is "dest let* 5" (let* (((a b c . d) '(3 (4 5) 6 7 8 9)))
                         (list a b c d))
         '(3 (4 5) 6 (7 8 9)))

(defun return-a-list ()
  '(3 4 5))

(test-is "dest let* 6" (let* (((a . b) (return-a-list)))
                         (list a b))
         '(3 (4 5)))

(test-err "dest let* 6E!" (let* (((a . 119) (return-a-list)))
                            (list a b))
          #/non-symbol terminates varlist/)

(test-is "dest let* 7" (let* (((a b) (return-a-list)))
                         (list a b))
         '(3 4))

(test-is "dest let* 8" (let* (((a b c d) (return-a-list)))
                         (list a b c d))
         '(3 4 5 nil))

(test-is "dest let* 9" (let* ((a (sys:symbols))
                              (b (table-keys *setf-update-table*))
                              (c 1337))
                         (let* (((a b c) '(3 4 5 6))
                                (b (list c c))
                                (c (* c c)))
                           (list a b c)))
         '(3 (5 5) 25))

(test-is "dest let 10" (let (((a b (c d . e) . f) (list 'x 'y (iota 4) 8 9 10)))
                         (list a b c d e f))
         '(x y 0 1 (2 3) (8 9 10)))

(test-is "dest let* 10"
         (let* (((a b (c d . e) . f) (list 'x 'y (iota 4) 8 9 10)))
           (list a b c d e f))
         '(x y 0 1 (2 3) (8 9 10)))

;; don't assign if symbol is nil

(test-is "dest nil" 
         (let ((line "/// name set-car!\n"))
           (let (((nil word rest)
                  (#r{^/// (name|impl|mina|spec|args|retv|docs) ?(.*)} line)))
             (list word rest)))
         '("name" "set-car!"))


(done-testing)
