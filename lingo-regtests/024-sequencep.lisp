(require 'regtests)

(test-is "seqencep 0" (sequencep nil) t)
(test-is "seqencep 1" (sequencep 'a) nil)
(test-is "seqencep 2" (sequencep "lala") t)
(test-is "seqencep 3" (sequencep (vector 2 3 4)) t)
(test-is "seqencep 4" (sequencep (list 3 4 5)) t)
(test-is "seqencep 5" (sequencep (make-table)) nil)
(test-is "seqencep 6" (sequencep 3.4) nil)
(test-is "seqencep 7" (sequencep #\Newline) nil)
(test-is "seqencep 8" (sequencep #\a) nil)
(test-is "seqencep 9" (sequencep #/^.*$/) nil)
(test-is "seqencep 10" (sequencep (lambda () 3)) nil)

(test-is "elt list !0" (errset (elt nil 0) nil) nil)
(test-is "elt list !1" (errset (elt '(a) 1) nil) nil)
(test-is "elt list !5" (errset (elt '(a b c) 5) nil) nil)
(test-is "elt list 0" (elt '(a) 0) 'a)
(test-is "elt list 1" (elt '(a b) 1) 'b)
(test-is "elt list 5" (elt '(a b c e f g) 5) 'g)

(test-is "elt vector !0" (errset (elt #() 0) nil) nil)
(test-is "elt vector !1" (errset (elt #(a) 1) nil) nil)
(test-is "elt vector !5" (errset (elt #(a b c) 5) nil) nil)
(test-is "elt vector 0" (elt #(a) 0) 'a)
(test-is "elt vector 1" (elt #(a b) 1) 'b)
(test-is "elt vector 5" (elt #(a b c e f g) 5) 'g)

(test-is "elt string !0" (errset (elt "" 0) nil) nil)
(test-is "elt string !1" (errset (elt "a" 1) nil) nil)
(test-is "elt string !5" (errset (elt "abc" 5) nil) nil)
(test-is "elt string 0" (elt "a" 0) #\a)
(test-is "elt string 1" (elt '"ab" 1) #\b)
(test-is "elt string 5" (elt "abcefg" 5) #\g)

(test-is "setelt string" (let ((a "lalelu"))
                            (setelt a 3 #\x)
                            a)
         "lalxlu")
(test-is "setelt list" (let ((a '(la le lu)))
                            (setelt a 1 #\x)
                            a)
         '(la #\x lu))
(test-is "setelt vector" (let ((a #(la le lu)))
                            (setelt a 1 #\x)
                            a)
         #(la #\x lu))

(test-is "setelt list !0" (errset (setelt nil 0 'x) nil) nil)
(test-is "setelt list !1" (errset (setelt '(a) 1 'x) nil) nil)
(test-is "setelt list !5" (errset (setelt '(a b c) 5 'x) nil) nil)
(test-is "setelt list 0" (let ((l '(a))) (setelt l 0 'x) l) '(x))
(test-is "setelt list 1" (let ((l '(a b))) (setelt l 1 'x) l) '(a x))
(test-is "setelt list 5" (let ((l '(a b c d e f g))) (setelt l 5 'x) l)
         '(a b c d e x g))
(test-is "setelt list 6" (let ((l '(a b c d e f g))) (setelt l 6 'x) l)
         '(a b c d e f x))
(test-is "setelt list 7" (errset (let ((l '(a b c d e f g)))
                                    (setelt l 7 'x)
                                    l)
                                  nil)
         nil)

(test-is "setelt vector !0" (errset (setelt #() 0 'x) nil) nil)
(test-is "setelt vector !1" (errset (setelt #(a) 1 'x) nil) nil)
(test-is "setelt vector !5" (errset (setelt #(a b c) 5 'x) nil) nil)
(test-is "setelt vector 0" (let ((v #(a))) (setelt v 0 'x) v) #(x))
(test-is "setelt vector 1" (let ((v #(a b))) (setelt v 1 'x) v) #(a x))
(test-is "setelt vector 5" (let ((v #(a b c d e f g))) (setelt v 5 'x) v)
         #(a b c d e x g))
(test-is "setelt vector 6" (let ((v #(a b c d e f g))) (setelt v 6 'x) v)
         #(a b c d e f x))
(test-is "setelt vector 7" (errset (let ((v #(a b c d e f g)))
                                      (setelt v 7 'x)
                                      v)
                                    nil)
         nil)

(test-is "setelt string !0" (errset (setelt "" 0 #\x) nil) nil)
(test-is "setelt string !1" (errset (setelt "a" 1 #\x) nil) nil)
(test-is "setelt string !5" (errset (setelt "abc" 5 #\x) nil) nil)
(test-is "setelt string !1a" (errset (setelt "abc" 1 'x) nil) nil)
(test-is "setelt string 0" (let ((s "a")) (setelt s 0 #\x) s) "x")
(test-is "setelt string 1" (let ((s '"ab")) (setelt s 1 #\x) s) "ax")
(test-is "setelt string 5" (let ((s "abcdefg")) (setelt s 5 #\x) s) "abcdexg")
(test-is "setelt string 6" (let ((s "abcdefg")) (setelt s 6 #\x) s) "abcdefx")
(test-is "setelt string 7" (errset (let ((s "abcdefg"))
                                      (setelt s 7 #\x)
                                      s)
                                    nil)
         nil)

;; strings are immutable, so equal strings are eq anyway
(test-is "copy-seq string 0" (let* ((s1 "")
                                     (s2 (copy-seq s1)))
                                (list (eq s1 s2)
                                      (equal s1 s2)))
         '(t t))
(test-is "copy-seq string 1" (let* ((s1 "lalelu")
                                     (s2 (copy-seq s1)))
                                (list (eq s1 s2)
                                      (equal s1 s2)))
         '(t t))
(test-is "copy-seq string 2" (let* ((s1 "Gruß, Jürgen Nickelsen.
于尔根尼克尔森")
                                     (s2 (copy-seq s1)))
                                (list (eq s1 s2)
                                      (equal s1 s2)))
         '(t t))

(test-is "copy-seq vector 0" (let* ((v1 #())
                                     (v2 (copy-seq v1)))
                                (list (eq v1 v2)
                                      (equal v1 v2)))
         '(nil t))
(test-is "copy-seq vector 1" (let* ((v1 #(1))
                                     (v2 (copy-seq v1)))
                                (list (eq v1 v2)
                                      (equal v1 v2)))
         '(nil t))
(test-is "copy-seq vector 2" (let* ((v1 #(3 4 'shoonana "Mufti, Dufti"))
                                     (v2 (copy-seq v1)))
                                (list (eq v1 v2)
                                      (equal v1 v2)))
         '(nil t))

;; all empty lists are the same list, hence eq
(test-is "copy-seq list 0" (let* ((l1 '())
                                   (l2 (copy-seq l1)))
                              (list (eq l1 l2)
                                    (equal l1 l2)))
         '(t t))
(test-is "copy-seq list 1" (let* ((l1 '(1))
                                   (l2 (copy-seq l1)))
                              (list (eq l1 l2)
                                    (equal l1 l2)))
         '(nil t))
(test-is "copy-seq list 2" (let* ((l1 '(3 4 'shoonana "Mufti, Dufti"))
                                   (l2 (copy-seq l1)))
                              (list (eq l1 l2)
                                    (equal l1 l2)))
         '(nil t))
;; copy last cdr, too
(test-is "copy-seq list 3" (let* ((l1 '(3 4 'shoonana "Mufti, Dufti" . t))
                                   (l2 (copy-seq l1)))
                              (list (eq l1 l2)
                                    (equal l1 l2)))
         '(nil t))

(test-is "copy-seq symbol" (errset (copy-seq 'shoo) nil) nil)

;; doseq??

(test-is "doseq 0" (let ((s '(3 4 5 6 7 8 9))
                         (result ""))
                     (doseq (el s result)
                            (setf result (string result el))))
         "3456789")
(test-is "doseq 1" (let ((s '(3 4 5 6 7 8 9))
                         (result ""))
                     (doseq (el s result 1)
                            (setf result (string result el))))
         "456789")
(test-is "doseq 2" (let ((s '(3 4 5 6 7 8 9))
                         (result ""))
                     (doseq (el s result 2 5)
                            (setf result (string result el))))
         "567")
(test-is "doseq 0v" (let ((s #(3 4 5 6 7 8 9))
                         (result ""))
                     (doseq (el s result)
                            (setf result (string result el))))
         "3456789")
(test-is "doseq 1v" (let ((s #(3 4 5 6 7 8 9))
                         (result ""))
                     (doseq (el s result 1)
                            (setf result (string result el))))
         "456789")
(test-is "doseq 2v" (let ((s #(3 4 5 6 7 8 9))
                         (result ""))
                     (doseq (el s result 2 5)
                            (setf result (string result el))))
         "567")
(test-is "doseq 3v" (let ((s #(3 4 5 6 7 8 9))
                          (from 2)      ;must be evaluated!
                          (to 5)
                          (result ""))
                     (doseq (el s result from to)
                            (setf result (string result el))))
         "567")
(test-is "doseq 4v" (let ((s #(3 4 5 6 7 8 9))
                          from to       ;both nil
                          (result ""))
                     (doseq (el s result from to)
                            (setf result (string result el))))
         "3456789")

;; find et al.

;; don't find
(test-is "find 0" (find 'a '(3 4 5 6)) nil)

;; find
(test-is "find 1" (find 4 '(3 4 5 6)) 4)

;; find with custom test
(test-is "find 2" (find '(3 . 4) '((4 . a) (3 . d) (7 . c) (3 . 5) (5 . 9))
                        nil
                        (lambda (a b) (eq (car a) (car b))))
         '(3 . d))
;; find with custom test from-end
(test-is "find 3" (find '(3 . 4) '((4 . a) (3 . d) (7 . c) (3 . 5) (5 . 9))
                        t
                        (lambda (a b) (eq (car a) (car b))))
         '(3 . 5))
;; find with custom test from-end, end, start
(test-is "find 4" (find '(3 . 4) '((4 . a) (3 . d) (7 . c) (3 . 5) (5 . 9))
                        t (lambda (a b) (eq (car a) (car b))) 0 2)
         '(3 . d))
;; find with custom test, star
(test-is "find 5" (find '(3 . 4) '((4 . a) (3 . d) (7 . c) (3 . 5) (5 . 9))
                        nil (lambda (a b) (eq (car a) (car b))) 2)
         '(3 . 5))

;; don't find
(test-is "find 0v" (find 'a #(3 4 5 6)) nil)

;; find
(test-is "find 1v" (find 4 #(3 4 5 6)) 4)

;; find with custom test
(test-is "find 2v" (find '(3 . 4) #((4 . a) (3 . d) (7 . c) (3 . 5) (5 . 9))
                        nil
                        (lambda (a b) (eq (car a) (car b))))
         '(3 . d))
;; find with custom test from-end
(test-is "find 3v" (find '(3 . 4) #((4 . a) (3 . d) (7 . c) (3 . 5) (5 . 9))
                        t
                        (lambda (a b) (eq (car a) (car b))))
         '(3 . 5))
;; find with custom test from-end, end, start
(test-is "find 4v" (find '(3 . 4) #((4 . a) (3 . d) (7 . c) (3 . 5) (5 . 9))
                        t (lambda (a b) (eq (car a) (car b))) 0 2)
         '(3 . d))
;; find with custom test, start
(test-is "find 5v" (find '(3 . 4) #((4 . a) (3 . d) (7 . c) (3 . 5) (5 . 9))
                        nil (lambda (a b) (eq (car a) (car b))) 2)
         '(3 . 5))

;; don't find
(test-is "find-if 0" (find-if #'oddp '(2 4 6 8 10)) nil)
(test-is "find-if-not 1" (find-if-not #'evenp '(2 4 6 8 10)) nil)

;; find
(test-is "find-if 1" (find-if #'evenp '(2 4 6 8 10)) 2)
(test-is "find-if-not 0" (find-if-not #'oddp '(2 4 6 8 10)) 2)
(test-is "find-if 2" (find-if #'evenp '(3 5 7 8 9 11)) 8)
(test-is "find-if-not 2" (find-if-not #'oddp '(3 5 7 8 9 11)) 8)

(test-is "member 0" (member 'c nil) nil)
(test-is "member 1" (member 'c '(c)) '(c))
(test-is "member 2" (member 'c '(a b c d)) '(c d))
(test-is "member 3" (member 'c '(3 4 5 6)) nil)
(test-is "member 4" (member 4 '(3 4 5 6)) '(4 5 6))

;; member with custom test
(test-is "member 5" (member '(3 . 4) '((4 . a) (3 . d) (7 . c) (3 . 5) (5 . 9))
                            :test (lambda (a b) (eq (car a) (car b))))
         '((3 . d) (7 . c) (3 . 5) (5 . 9)))

;; don't member
(test-is "member-if 0" (member-if #'oddp '(2 4 6 8 10)) nil)
(test-is "member-if-not 1" (member-if-not #'evenp '(2 4 6 8 10)) nil)

;; member
(test-is "member-if 1" (member-if #'evenp '(2 4 6 8 10)) '(2 4 6 8 10))
(test-is "member-if-not 0" (member-if-not #'oddp '(2 4 6 8 10)) '(2 4 6 8 10))
(test-is "member-if 2" (member-if #'evenp '(3 5 7 8 9 11)) '(8 9 11))
(test-is "member-if-not 2" (member-if-not #'oddp '(3 5 7 8 9 11)) '(8 9 11))

(test-is "reverse string" (reverse "Gruß, Jürgen Nickelsen.
于尔根尼克尔森")
         "森尔克尼根尔于
.neslekciN negrüJ ,ßurG")

;; issue #23 Strings are not really Unicode/UTF-8 strings
(test-is "issue #23 1" (length "böse") 4)
(test-is "issue #23 2" (length "森尔克尼根尔于
.neslekciN negrüJ ,ßurG") 31)

(done-testing)
