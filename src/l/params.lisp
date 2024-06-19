
(defun params (a b c &optional d (e 4) &rest grabbelsack &key (k1 3) k2)
  "Exercise all kinds of function parameters and return t."
  (println "a =" a)
  (println "b =" b)
  (println "c =" c)
  (println "d =" d)
  (println "e =" e)
  (println "grabbelsack =" grabbelsack)
  (println "k1 =" k1)
  (println "k2 =" k2)
  t)
