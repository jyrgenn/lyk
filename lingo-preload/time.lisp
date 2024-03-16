
(defun get-iso-time (&optional (universal-time (get-universal-time-ns))
                               &key time-zone (long-form t) fractions
                               append-blank)
  "Format the UNIVERSAL-TIME according to ISO 8601 and return the string.
The default is the current time.
Key :long-form may be true for long form (default) or nil for short.
Key fractions may be :ms, :us, or :ns for milliseconds, microseconds, or
nanoseconds, respectively."
  (let ((format-string
         ;; long/short format:fractions:blank
         (format nil "%s%s%s"
                 (if long-form "%Y-%m-%d %H:%M:%S" "%Y%m%d:%H%M%S")
                 (if fractions
                     (or (#:((:ms . ".%i")(:us . ".%J")(:ns . ".%K")) fractions)
                         (error "get-iso-time: unknown fraction specifier `%s'"
                                fractions))
                   "")
                 (if append-blank " " ""))))
    (format-universal-time format-string universal-time)))

            
