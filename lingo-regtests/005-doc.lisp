(require 'regtests)

(defun dog (collar &optional leash) "Waff!" (list 'dog collar leash))
(test-is "docstring 1" (doc #'dog nil t)
         "lambda function (dog collar &optional leash)\nWaff!")
(test-is "docstring 2" (doc 'dog t t)
         "lambda function (dog collar &optional leash)")
(defun cat (&optional collar) t)
(test-is "docstring 3" (doc 'cat nil t)
         "lambda function (cat &optional collar)")
(test-is "docstring 4" (doc 'cat t t)
         "lambda function (cat &optional collar)")
(test-is "docstring 5" (doc 'symbolp t t)
         "builtin function (symbolp ARG) => t/nil")
(test-is "docstring 6" (doc 'symbolp nil t)
         "builtin function (symbolp ARG) => t/nil
Return t if ARG is a symbol, nil else.")
(test-is "docstring 7" (doc 'if t t)
         "builtin special form (if COND THEN &rest ELSE) => value")

(test-is "docstring table brief" (doc #:() t t)
         "table (TABLE KEY &optional VALUE) => value")

(test-is "docstring table long" (doc #:() nil t)
         "table (TABLE KEY &optional VALUE) => value
Return the value stored in the TABLE for KEY.
With optional VALUE, set the value.")

(test-is "docstring vector brief" (doc #() t t)
         "vector (VECTOR INDEX &optional VALUE) => value")

(test-is "docstring vector long" (doc #() nil t)
         "vector (VECTOR INDEX &optional VALUE) => value
Return the value of the VECTOR slot at INDEX.
With optional VALUE, set the value.")

(test-is "docstring regexp brief" (doc #// t t)
         "regexp (REGEXP STRING &optional LIMIT) => matches")

(test-is "docstring regexp long" (doc #// nil t)
         "regexp (REGEXP STRING &optional LIMIT) => matches
If STRING matches REGEXP, return list of match and sub-matches, else nil.
With optional third argument LIMIT, a list of match lists for (potentially)
multiple matches is returned. If LIMIT is t, all matches are considered;
otherwise, a number specifies the number of matches to be considered.

Regular expression syntax is that of the Go regexp package (RE2), which
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
