(require 'regtests)

(defvar the-alist '((3 . 4)
                    (7 . 5)
                    (lala . humdi)
                    (10 . 11)
                    ((1 2 3) . 12)
                    ("hudi" . :rudi)))

(defvar num-alist '((1 . "one") (2 . "two") (3 . "three") (4 . "four")
                    (5 . "five") (6 . "six") (7 . "seven")))

(test-is "assoc 0" (assoc 'lala nil) nil)
(test-is "assoc 1" (errset (assoc 'lala '(4)) nil) nil)
(test-is "assoc 2" (assoc 'lala the-alist) '(lala . humdi))
(test-is "assoc 3" (assoc '(1 2 3) the-alist) '((1 2 3) . 12))
(test-is "assoc 4" (assoc '(1 2 5) the-alist) nil)
(test-is "assoc 5" (assoc 10 the-alist) '(10 . 11))

(test-is "assq 0" (assq 'lala nil) nil)
(test-is "assq 1" (errset (assq 'lala '(4))
                           nil) nil)
(test-is "assq 2" (assq 'lala the-alist) '(lala . humdi))
(test-is "assq 3" (assq '(1 2 3) the-alist) nil)
(test-is "assq 4" (assq '(1 2 5) the-alist) nil)

(defun default () 'this)

(test-is "sassoc 0" (sassoc 'lala nil #'default) 'this)
(test-is "sassoc 1" (errset (sassoc 'lala '(4) #'default) nil) nil)
(test-is "sassoc 2" (sassoc 'lala the-alist #'default) '(lala . humdi))
(test-is "sassoc 3" (sassoc '(1 2 3) the-alist #'default) '((1 2 3) . 12))
(test-is "sassoc 4" (sassoc '(1 2 5) the-alist #'default) 'this)
(test-is "sassoc 5" (sassoc 10 the-alist 'default) '(10 . 11))

(test-is "sassq 0" (sassq 'lala nil #'default) 'this)
(test-is "sassq 1" (errset (sassq 'lala '(4) #'default) nil) nil)
(test-is "sassq 2" (sassq 'lala the-alist #'default) '(lala . humdi))
(test-is "sassq 3" (sassq '(1 2 3) the-alist #'default) 'this)
(test-is "sassq 4" (sassq '(1 2 5) the-alist 'default) 'default)

(fmakunbound 'shnuddel)
(makunbound 'shnuddel)
(test-is "assoc-if 0" (errset (assoc-if 'shnuddel the-alist) nil) nil)

(test-is "assoc-if 1" (assoc-if 'evenp num-alist) '(2 . "two"))
(test-is "assoc-if 2" (assoc-if #'evenp num-alist) '(2 . "two"))
(test-is "assoc-if 3" (assoc-if (lambda (n) (> n 5)) num-alist) '(6 . "six"))
(test-is "assoc-if 4" (assoc-if (lambda (n) (> n 15)) num-alist) nil)

(test-is "assoc-if-not 1" (assoc-if-not 'evenp num-alist) '(1 . "one"))
(test-is "assoc-if-not 2" (assoc-if-not #'evenp num-alist) '(1 . "one"))
(test-is "assoc-if-not 3" (assoc-if-not #'numberp the-alist) '(lala . humdi))
(test-is "assoc-if-not 4" (assoc-if-not #'tablep the-alist) '(3 . 4))


(done-testing)
