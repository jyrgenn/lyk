;;; Project Euler problem 17

(defvar name-table #:((0 . "")
                      (1 . "one")
                      (2 . "two")
                      (3 . "three")
                      (4 . "four")
                      (5 . "five")
                      (6 . "six")
                      (7 . "seven")
                      (8 . "eight")
                      (9 . "nine")
                      (11 . "eleven")
                      (12 . "twelve")
                      (13 . "thirteen")
                      (14 . "fourteen")
                      (15 . "fifteen")
                      (16 . "sixteen")
                      (17 . "seventeen")
                      (18 . "eighteen")
                      (19 . "nineteen")
                      (10 . "ten")
                      (20 . "twenty")
                      (30 . "thirty")
                      (40 . "forty")
                      (50 . "fifty")
                      (60 . "sixty")
                      (70 . "seventy")
                      (80 . "eighty")
                      (90 . "ninety")
                      ))

(defun num-to-string (n)
  (cond ((= n 1000) "one thousand")
        ((>= n 100)
         (string (num-to-string (truncate (/ n 100)))
                 " hundred"
                 (let ((rest (mod n 100)))
                   (if (zerop rest)
                       ""
                     (string " and " (num-to-string rest))))))
        ((>= n 10)
         (or (name-table n)
           (let ((tens (* 10 (truncate (/ n 10))))
                 (ones (mod n 10)))
             (string (name-table tens)
                     (if (zerop ones)
                         ""
                       (string "-" (num-to-string ones)))))))
        (t (name-table n))))

(defun eu17 ()
  (let ((letter-count 0))
    (dotimes1 (i 1000)
      (let ((nums (num-to-string i)))
        ;; (format t "%d: %s\n" i nums)
        (incf letter-count (length (regex-replace #/[^a-z]+/ nums "")))))
    letter-count))

;;; result