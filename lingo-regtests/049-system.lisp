(require 'regtests)

(test-is "performance data" (let ((pdata (collect-performance-data t)))
                              (and (equal (elt pdata 0) '(evals . 1))
                                   (equal (elt pdata 1) '(pairs . 0))
                                   (eq (car (elt pdata 2)) 'secs)
                                   (numberp (cdr (elt pdata 2)))))
         t)

(test-is "apropos string 1" (> (length (apropos-list "a")) 100) t)
(test-is "apropos string 2" (= (length (apropos-list "^a")) 0) t)

;; the correctness of this *may* change some time; currently we are at 15
;; matches
(test-is "apropos regexp" (let ((len (length (apropos-list #/^a/))))
                            (and (> len 10)
                                 (< len 40)))
         t)

(defparameter a 19)
(defparameter b 21)

(test-is "assertion good" (assert (< a b) "Apfel < Birne") nil)
(incf a 2)
(test-err "assertion fail 1" (assert (< a b) "Apfel < Birne")
          #/assertion failed: Apfel < Birne/)
(test-err "assertion fail 2" (assert (< a b))
          #/assertion failed: \(< a b\)/)
(test-err "assertion fail 3" (assert (< a b) (format nil "%s < %s" 'a 'b))
          #/assertion failed: a < b/)
(test-err "assertion fail 4" (assert (< a b) (format nil "%s < %s" a b))
          #/assertion failed: 21 < 21/)

(test-is "declare good" (declare (number a b)) nil)
(test-err "declare err" (declare (int a b))
          #/invalid type in declare: int/)
(test-err "declare fail" (declare (string a))
          #/declared a as string, but has number value/)

(defun fac-decl (n)
  "Return the faculty of N."
  (declare (number n))
  (if (zerop n)
      1
    (* n (fac-decl (1- n)))))

(test-is "fac-decl good" (fac-decl 7) 5040)
(test-err "fac-decl err" (fac-decl "7")
          #/declared n as number, but has string value/)

;; double-decl
(defun nakes-and-adders (a b)
  "Return the sum of A and B."
  (declare (number a b))
  (+ a b))

(test-is "adder 0" (nakes-and-adders 3 4) 7)
(test-err "adder 1" (nakes-and-adders "3" 4)
          #/declared a as number, but has string value/)
(test-err "adder 2" (nakes-and-adders 3 '|4|)
          #/declared b as number, but has symbol value/)

;; two. decls. Wouldn't work with CL, which only allows one declare statement.
(defun nakes-and-adders (a b)
  "Return the sum of A and B."
  (declare (number b))
  (declare (number a))
  (+ a b))

(test-is "adder 0a" (nakes-and-adders 3 4) 7)
(test-err "adder 1a" (nakes-and-adders "3" 4)
          #/declared a as number, but has string value/)
(test-err "adder 2a" (nakes-and-adders 3 '|4|)
          #/declared b as number, but has symbol value/)


(done-testing)
