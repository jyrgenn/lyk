
;;; all Lisp object types known to lyk

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
These are the leaves of the object type tree; there are also the type
categories `function`, `stream`, and sequence, which don't appear here,
but for which type predicates exist.")
(put '*object-types* t '*system*)
