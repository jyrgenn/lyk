
(defvar sys:features (make-table) "Table of all provided features")
(putprop 'sys:features t 'sys:system)

(defun have-feature (feature)
  "Return non-nil if FEATURE has been provided."
  (if (table-get sys:features feature)
      feature
    nil))

(defun require (feature)
  "Load FEATURE from file or fail."
  (if (stringp feature)
      (setq feature (intern feature)))
  (unless (have-feature feature)
    (load feature)
    (unless (have-feature feature)
      (error "feature %s is not provided" feature)))
  feature)

(defun provide (feature)
  "Let FEATURE be provided."
  (if (stringp feature)
      (setq feature (intern feature)))
  (table-put sys:features feature t))

