(require 'regtests)

(test-is "join 0" (join () "||") "")
(test-is "join 1" (join '("a") "||" ) "a")
(test-is "join 2" (join (list 'a "b") "||") "a||b")
(test-is "join 3" (join (list 'a "b" 'c) "||") "a||b||c")
(test-is "join l0" (join nil " ") "")
(test-is "join l1" (join '(3) " ") "3")
(test-is "join l2" (join '(3 4) " ") "3 4")
(test-is "join l3" (join '(3 4 t) " ") "3 4 t")
(test-is "join lol" (join (list " " "_" "." "'") '0) " 0_0.0'")

(done-testing)
