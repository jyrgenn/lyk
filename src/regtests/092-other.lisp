;;; Regtests for things outside of the big topics

(require 'regtests)

(test "unquote special" (regexp-match #/^special form/ (doc 'unquote t)))

(test-err "unquote error 1" ,lala #/unquote outside of a quasiquote/)

(test-err "unquote error 2" (unquote lala)
          #/unquote outside of a quasiquote/)

(done-testing)
