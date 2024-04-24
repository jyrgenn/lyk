(require 'regtests)

(test-is "keywordp 0" (keywordp 'elephant) nil)
(test-is "keywordp 1" (keywordp 12) nil)
(test-is "keywordp 2" (keywordp :test) t)
(test-is "keywordp 3" (keywordp ':test) t)
(test-is "keywordp 4" (keywordp nil) nil)
(test-is "keywordp 5" (keywordp :nil) t)
(test-is "keywordp 6" (keywordp '(:test)) nil)
(test-is "keywordp 7" (keywordp "hello") nil)
(test-is "keywordp 8" (keywordp ":hello") nil)
(test-is "keywordp 9" (keywordp '&optional) nil)
(test-is "keywordp 10" (keywordp '(3 4 5)) nil)
(test-is "keywordp 11" (keywordp '(3 4 5)) nil)

(defun foo (n &key multiplier) (if multiplier (* multiplier n) n))

(test-is "&key 0" (foo 12) 12)
(test-is "&key 1" (foo 12 :multiplier 3) 36)
(test-err "&key 2" (foo 12 2)
          #/expected keyword argument for function foo, found 2/)
(test-err "&key 3" (foo 12 2 3)
          #/expected keyword argument for function foo, found 2/)

(defun multer (n &key (multiplier 3)) (* n multiplier))

(test-is "&key 4" (multer (+ 2 3)) 15)
(test-is "&key 5" (multer (+ 2 6) :multiplier 2) 16)

(defun addder (n &optional (a1 2) (a2 4)
                 &key (a3 8) (a4 16) a5)
  (+ n a1 a2 a3 a4 a5))

(test-err "&key 6a" (addder 7)
          #/argument is not a number: nil/)
(test-is "&key 6b" (addder 7 :a5 1) 38)
(test-is "&key 6c" (addder 7 9 :a5 5 :a3 2) 43)

(defun arrrghs (a1 a2 &optional a3 (a4 'x) &rest a5 &key (a6 (+ 10 9))
                   a7 (a8 2))
  (list 'a1 a1 'a2 a2 'a3 a3 'a4 a4 'a5 a5 'a6 a6 'a7 a7 'a8 a8))

(test-err "arrrghs 1" (arrrghs 1) #/too few arguments for function/)
(test-is "arrrghs 2" (arrrghs 1 2)
         '(a1 1 a2 2 a3 nil a4 x a5 nil a6 19 a7 nil a8 2))
(test-is "arrrghs 3" (arrrghs 'd 'e 'f)
         '(a1 d a2 e a3 f a4 x a5 nil a6 19 a7 nil a8 2))
(test-is "arrrghs 4" (arrrghs 'd 'e 'f 'g)
         '(a1 d a2 e a3 f a4 g a5 nil a6 19 a7 nil a8 2))
(test-is "arrrghs 5" (arrrghs 'd 'e 'f 'g 'h)
         '(a1 d a2 e a3 f a4 g a5 (h) a6 19 a7 nil a8 2))
(test-is "arrrghs 7" (arrrghs 'd 'e 'f 'g 'h 'i 'j)
         '(a1 d a2 e a3 f a4 g a5 (h i j) a6 19 a7 nil a8 2))
(test-is "arrrghs 8" (arrrghs 'd 'e 'f 'g 'h 'i 'j :a7 'ff :a6 "www")
         '(a1 d a2 e a3 f a4 g a5 (h i j) a6 "www" a7 ff a8 2))
(test-is "arrrghs 9"
         (arrrghs 'd 'e 'f 'g 'h 'i 'j :a7 'ff :a8 (* 2 60) :a6 "www")
         '(a1 d a2 e a3 f a4 g a5 (h i j) a6 "www" a7 ff a8 120))
(test-is "arrrghs 10"
         (arrrghs 'd 'e :a8 'g :a6 "www")
         '(a1 d a2 e a3 nil a4 x a5 nil a6 "www" a7 nil a8 g))
(test-is "arrrghs 11"
         (arrrghs 'd 'e :a5 44 :a8 'g)
         '(a1 d a2 e a3 :a5 a4 44 a5 nil a6 19 a7 nil a8 g))
(test-is "arrrghs 12"
         (arrrghs 'd 'e :a6 44 :a8 'g)
         '(a1 d a2 e a3 nil a4 x a5 nil a6 44 a7 nil a8 g))

;; test if keyword init-form is evaluated at the right moment and in
;; the right environment

(defparameter hoo 'shoo)

(defun funfunfun (foon &key k1 (k2 (cons hoo hoo)))
  (list foon k2))

(test-err "&key init 1" (funfunfun 'tagga 'tugga) #/keyword expected, found:/)
(test-is "&key init 2" (funfunfun 'tagga :k2 112) '(tagga 112))
(test-is "&key init 3" (funfunfun 'tagga) '(tagga (shoo . shoo)))

(setf hoo 'moo)
(let ((hoo "shoobeedoo"))
  (test-is "&key init 4" (funfunfun 'tagga) '(tagga (moo . moo))))

(done-testing)
