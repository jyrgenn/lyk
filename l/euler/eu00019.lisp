;;; Project Euler problem 18

;; for year in {1901..2000}; do
;;     for month in {1..12}; do
;;         cal $month $year
;;     done
;; done | grep '^ 1 ' | wc -l

(defvar month-length #(31 28 31 30 31 30 31 31 30 31 30 31)
  "Number of days in a month in a non-leap year.
Index by (zero-based) month.")

(defun days (month year)
  "Return the number of days of (zero-based!) MONTH in YEAR.
From 1901 to 2099 every multiple of 4 is a leap year."
  (+ (month-length month)
     (if (and (zerop (mod year 4))      ;leap year?
              (= month 1))              ;and February?
         1
       0)))

(defun eu19 ()
  (let ((day 2)                         ;start day is Monday (0..6)
        (sundays 0))
    (for (year 1901 2000 1 <=)
      (dotimes (month 12)               ;month is 0..11
        (when (zerop day)
          (incf sundays))
        (setf day (mod (+ day (days month year)) 7))))
    sundays))

;; result 171
