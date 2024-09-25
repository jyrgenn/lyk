;;; Lisp code integrated into the interpreter and evaluated on startup

;; moved the types list to generated/10-types.lisp

(defvar *lyk-lib-directory*
  (let* ((bin-dir (dirname *lyk-called-as*))
         (base-dir (dirname bin-dir)))
    ;; (format *error-output* "bin: ~A base: ~A~%" bin-dir base-dir)
    (join (list base-dir "lib" "lyk") "/"))
    "Return the path of the lyk library directory.
This is based on the path used to call lyk, and then ../lib/lyk/.")

