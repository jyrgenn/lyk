(require 'regtests)

(defvar tbl (make-table))
(test-is "table 0" (table-count tbl) 0)

(test-is "table 3" (progn
                     (table-put tbl 'lala 'huhu)
                     (table-put tbl "humdrum" 23)
                     (table-put tbl 234 #(noogle))
                     (list (table-count tbl)
                           (table-get tbl 234)
                           (table-get tbl 'lala)
                           (table-get tbl 'd)
                           (table-get tbl 'd 'defaultvalue)
                           (table-get tbl "humdrum")))
         "(3 #(noogle) huhu nil defaultvalue 23)")

(test-is "table length" (length #:((3 . 4)(5 . 6)(gaga . lady)(hu . nil))) 4)

(test-is "make table from pairs"
         (make-table '(3 . 4) '(5 . 6) '(gaga . lady) '(hu . nil))
         #:((3 . 4)(5 . 6)(gaga . lady)(hu . nil)))

;; TODO Char (test-is "make table from non-pairs"
;;          (make-table 5 '(hu . nil) #\u4567 '(gaga . lady))
;;          #:((#\䕧 . nil)(5 . nil)(gaga . lady)(hu . nil)))

;; TODO Char (test-is "table string" #:((#\3 #\Newline #x123))
;;          "#:((#\\3 . (#\\Newline 291)))")

(done-testing)
