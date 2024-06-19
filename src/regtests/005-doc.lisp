(require 'regtests)

(defun dog (collar &optional leash) "Waff!" (list 'dog collar leash))
(test-match "docstring 1" (doc #'dog t nil)
            #/lambda function \(dog collar &optional leash\) => value\nWaff!/)
(test-is "docstring 2" (doc 'dog t t)
         "(dog collar &optional leash)")
(defun cat (&optional collar) t)
(test-match "docstring 3" (doc 'cat t nil)
            #/lambda function \(cat &optional collar\) => value
\[lambda function defined in/)

(test-is "docstring 4" (doc 'cat t t)
         "(cat &optional collar)")
(test-is "docstring 5" (doc 'symbolp t t)
         "(symbolp object)")
(test-match "docstring 6" (doc 'symbolp t nil)
            #/builtin function \(symbolp object\) => t\/nil
Return t if `object` is a symbol, else nil./)
(test-is "docstring 7" (doc 'if t t)
         "(if condition then-clause &rest else-clauses)")

;; as tables, vectors, and regexps are no longer callables (as they
;; were in an earlier implementation), doesn't make much sense to
;; scrutinise the doc of the equivalent functions more than those of
;; all other functions.

(test-is "docstring table brief" (doc 'table-get t t)
         "(table-get table key &optional default)")


(done-testing)
