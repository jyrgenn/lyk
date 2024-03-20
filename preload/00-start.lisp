;;; Lisp code integrated into the interpreter and evaluated on startup

(defvar *object-types*
  '(
    ;; char
    environment
    function
    macro
    ;; netaddr
    number
    cons
    ;; port
    regexp
    string
    ;; struct
    symbol
    table
    vector
    )
  "List of types known to the system.")
(put '*object-types* t '*system*)

