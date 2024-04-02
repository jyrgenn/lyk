(require 'regtests)

(defun dog (collar &optional leash) "Waff!" (list 'dog collar leash))
(test-match "docstring 1" (doc #'dog t nil)
            #/lambda function \(dog collar &optional leash\) => value\nWaff!/)
(test-is "docstring 2" (doc 'dog t t)
         "(dog collar &optional leash)")
(defun cat (&optional collar) t)
(test-match "docstring 3" (doc 'cat t nil)
            #/lambda function \(cat &optional collar\) => value\n\[defined in/)
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
;; were in lingo), doesn't make much sense to scrutinise the doc of
;; the equivalent functions more than those of all other functions.

(test-is "docstring table brief" (doc 'table-get t t)
         "(table-get table key &optional default)")

(test-is "docstring regexp-match" (doc 'regexp-match nil t)
         "regexp (REGEXP STRING &optional LIMIT) => matches
If STRING matches REGEXP, return list of match and sub-matches, else nil.
With optional third argument LIMIT, a list of match lists for (potentially)
multiple matches is returned. If LIMIT is t, all matches are considered;
otherwise, a number specifies the number of matches to be considered.

Regular expression syntax is that of the Kotlin regexp package (RE2), which
is largely similar to that of the Perl and Python languages. A \"(?flags)\"
specification in the regexp can modify the behaviour of the match in the
current group. Possible flags are:

i  case-insensitive (default false)
m  multi-line mode: ^ and $ match begin/end line in addition to begin/end
   text (default false)
s  let . match \\n (default false)
U  ungreedy: swap meaning of x* and x*?, x+ and x+?, etc (default false)
")

(done-testing)
