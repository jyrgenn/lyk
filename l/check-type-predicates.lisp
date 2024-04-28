;;; Check if there is a type predicate for each of the types

(defun check-type-predicates ()
  "Check if there is a type predicate for each of the types in lyk."
  (let ((types (append *object-types* '(function stream sequence))))
    (dolist (type types)
      (let* ((type-name (string (symbol-name type)))
             (suffix (if (string-contains-p type-name "-")
                         "-p"
                         "p"))
             (pred (intern (string type-name suffix))))
        (format t "(test %-38s (fboundp '%s))\n"
                (format nil "\"type predicate %s\"" type) pred)))))
