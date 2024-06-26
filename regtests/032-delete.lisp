(require 'regtests)

(test-is "delete list 0" (delete 'a '(b)) '(b))
(test-is "delete list 1" (delete 'a '(a)) nil)
(test-is "delete list 2" (delete 'c '(a b c d e f)) '(a b d e f))
(test-is "delete list 3" (delete 'a '(a b c d e f)) '(b c d e f))
(test-is "delete list 4" (delete 'f '(a b c d e f)) '(a b c d e))
(test-is "delete list 5" (delete 'g '(a b c d e f)) '(a b c d e f))
(test-is "delete list 6" (delete 'g nil) nil)
(test-is "delete list 7" (errset (delete 'g '(a b c d e . f))
                                  nil) nil)
(test-is "delete list 8" (delete 'g '(a g b g c g)) '(a b c))

(test-is "delete string 0" (delete 'a "der Tag, als Conny Kramer starb")
         "der Tag, als Conny Kramer starb")
(test-is "delete string 1" (delete #\a "der Tag, als Conny Kramer starb")
         "der Tg, ls Conny Krmer strb")
(test-is "delete string 2" (delete #\a "") "")

(test-is "delete string 3" (delete #\ö "Pöh, die Flötentöne!")
         "Ph, die Fltentne!")
(test-is "delete string 4" (delete #\u2022 "Pöh, •die Fl•ötentöne!")
         "Pöh, die Flötentöne!")
(test-is "delete string 5" (delete #\u2022 "Pöh, die Flötentöne!")
         "Pöh, die Flötentöne!")

(test-is "delete vector 0" (delete 'c #(a b c d e f)) #(a b d e f))
(test-is "delete vector 1" (delete 'a #(a b c d e f)) #(b c d e f))
(test-is "delete vector 2" (delete 'f #(a b c d e f)) #(a b c d e))
(test-is "delete vector 3" (delete 'g #(a b c d e f)) #(a b c d e f))
(test-is "delete vector 4" (delete 'g nil) nil)

(test-is "delete symbol 0" (delete 'a nil) nil)
(test-is "delete symbol 1" (errset (delete 'a 'b)
                                    nil) nil)

(done-testing)
