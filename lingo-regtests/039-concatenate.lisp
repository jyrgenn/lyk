(require 'regtests)

(test-is "concat v->vector"
         (concatenate 'vector #(2) #(3 4 5 6) #(a bc d) #("huhu" 13))
         #(2 3 4 5 6 a bc d "huhu" 13))

(test-is "concat v->list"
         (concatenate 'list #(2) #(3 4 5 6) #(a bc d) #("huhu" 13))
         '(2 3 4 5 6 a bc d "huhu" 13))

(test-is "concat v->string"
         (concatenate 'string #(2) #(3 4 5 6) #(a bc d) #("huhu" 13))
         "23456abcdhuhu13")

(test-is "concat l->string"
         (concatenate 'string '(2) '(3 4 5 6) '(a bc d) '("huhu" 13))
         "23456abcdhuhu13")

(test-is "concat l->list"
         (concatenate 'list '(2) '(3 4 5 6) '(a bc d) '("huhu" 13))
         '(2 3 4 5 6 a bc d "huhu" 13))

(test-is "concat l->vector"
         (concatenate 'vector '(2) '(3 4 5 6) '(a bc d) '("huhu" 13))
         #(2 3 4 5 6 a bc d "huhu" 13))

(test-is "concat s->string"
         (concatenate 'string "2" "3 4 5 6" "a bc d" "huhu 13")
         "23 4 5 6a bc dhuhu 13")

(test-is "concat s->vector"
         (concatenate 'vector "2" "3 4 5 6" "a bc d" "huhu 13")
         #(#\2 #\3 #\Space #\4 #\Space #\5 #\Space #\6 #\a #\Space #\b #\c #\Space #\d #\h #\u #\h #\u #\Space #\1 #\3))

(test-is "concat s->list"
         (concatenate 'list "2" "3 4 5 6" "a bc d" "huhu 13")
         '(#\2 #\3 #\Space #\4 #\Space #\5 #\Space #\6 #\a #\Space #\b #\c #\Space #\d #\h #\u #\h #\u #\Space #\1 #\3))


(done-testing)
