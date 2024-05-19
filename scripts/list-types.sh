#!/bin/sh
# list all Lisp object types known to lyk

echo "
;;; all Lisp object types known to lyk

(defvar *object-types*
  '("
sed -n 's/.*override val obtype = "\(.*\)".*/    \1/p' */*.kt | sort
cat <<'EOT'
   )
  "List of types known to the system.
These are the leaves of the object type tree; there are also the type
categories `function`, `stream`, and sequence, which don't appear here,
but for which type predicates exist.")
(put '*object-types* t '*system*)
EOT
