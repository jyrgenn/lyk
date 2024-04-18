;;; Lisp code integrated into the interpreter and evaluated on startup

(defvar *object-types*
  '(
    builtin
    char
    cons
    console-reader-stream
    environment
    error-object
    file-io-stream
    file-reader-stream
    file-writer-stream
    lambda
    macro
    number
    regexp
    string
    string-reader-stream
    string-writer-stream
    symbol
    table
    vector
    )
  "List of types known to the system.
These are the leaves of the object type tree; there are also type
categories like `function` or `stream`, which don't appear here, but
for which type predicates exist. TODO check")
(put '*object-types* t '*system*)

