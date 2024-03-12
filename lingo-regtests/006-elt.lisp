(require 'regtests)

(test-err "elt -1a" (elt "lala" -1)
          #/elt: negative index/)
(test-err "elt -1b" (elt '(a b c d) -1)
          #/elt: negative index /)
(test-is "elt 0a" (elt "lala" 0) #\l)
(test-is "elt 0b" (elt '(a b c d) 0) 'a)
(test-is "elt 3a" (elt "lila" 3) #\a)
(test-is "elt 3b" (elt '(a b c d) 3) 'd)
(test-err "elt 4a" (elt "lila" 4)
          #/index 4 out of bounds/)
(test-err "elt 4b" (elt '(a b c d) 4)
          #/index 4 out of bounds/)
(test-is "elt 5" (elt #(3 4 5 6) 2) 5)
(test-is "elt 6" (elt #(3 4 5 6) 4 7) 7)
(test-is "elt 7" (elt #(3 4 5 6) 19 nil) nil)
(test-is "elt 8" (elt "eine alte dicke Gans" 29 'Q) 'Q)

(test-err "elt err" (elt #(3 4 5 6) 4) #/index 4 out of bounds/)

(test-is "nreverse nil" (nreverse nil) nil)
(test-is "nreverse 1" (nreverse '(a)) "(a)")
(test-is "nreverse 2" (nreverse '(a b)) "(b a)")
(test-is "nreverse n" (nreverse '(a b c d e f)) "(f e d c b a)")

(done-testing)
