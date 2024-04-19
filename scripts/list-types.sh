#!/bin/sh
# list all Lisp object types known to lyk

echo "
;;; all Lisp object types known to lyk

(defvar *object-types*
  '("
sed -n 's/.*override val type = "\(.*\)".*/    \1/p' */*.kt | sort
cat <<'EOT'
   )
  "List of types known to the system.
These are the leaves of the object type tree; there are also type
categories like `function` or `stream`, which don't appear here, but
for which type predicates exist. TODO check")
(put '*object-types* t '*system*)
EOT
