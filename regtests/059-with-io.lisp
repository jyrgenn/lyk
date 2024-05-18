(require 'regtests)

(test-is "with-output-to-string"
         (with-output-to-string (s)
                                (print "lalala" s)
                                ;; TODO floating point format
                                (format s "TODO %08.3f\n" 13.4))
         "\nlalala 0013.400\n")
(test-is "with-input-from-string"
         (with-input-from-string (s "13.4 a #/^.*$/ 129")
                                 (list (read s nil)
                                       (read s nil)
                                       (read s nil)
                                       (read s nil)))
         '(13.4 a #/^.*$/ 129))

(done-testing)
