
;; #21: Wrong complaint about keyword arg missing. This was a false
;; alarm -- the underlying confusion caused by the keyword for
;; keywords not being a keyword (i.e. not :key, but &key) -- and is
;; closed now.

(require 'regtests)

(defun kw-only (&key alarm)
  "A function with only a keyword argument."
  (if alarm
      (format nil "Alarm: %s!" alarm)
    (format nil "Ok")))

(defun kw-plus (huhu &key remark)
  "A function with a normal and a keyword argument."
  (if remark
      (format nil "huhu: %s, Remark: %s" huhu remark)
    (format nil "huhu: %s, no remark" huhu)))

(test-is "kw-only function sans" (kw-only) "Ok")
(test-is "kw-only function with" (kw-only :alarm "hey") "Alarm: hey!")
(test-is "kw-plus function sans" (kw-plus "dumdi") "huhu: dumdi, no remark")
(test-is "kw-plus function with" (kw-plus "simsalabim" :remark "this is fine.")
         "huhu: simsalabim, Remark: this is fine.")

(done-testing)
