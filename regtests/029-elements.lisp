(require 'regtests)

(test-is "elements l 0" (elements ()) nil)
(test-is "elements l 1" (elements '(123)) '(123))
(test-is "elements l n" (elements '(123 456 789)) '(123 456 789))

(test-is "elements s 0" (elements "") nil)
(test-is "elements s 1" (elements "d") '(#\d))
(test-is "elements s n" (elements "Da geht die Sau durch den Knick!")
         '(#\D #\a #\Space #\g #\e #\h #\t #\Space #\d #\i #\e #\Space #\S #\a
           #\u #\Space #\d #\u #\r #\c #\h #\Space #\d #\e #\n #\Space #\K #\n
           #\i #\c #\k #\!))
(test-is "elements s u" (elements "Ärgörnüß")
         '(#\Ä #\r #\g #\ö #\r #\n #\ü #\ß))

(test-is "elements v 0" (elements #()) nil)
(test-is "elements v 1" (elements #(123)) '(123))
(test-is "elements v n" (elements #(123 456 789)) '(123 456 789))


(done-testing)
