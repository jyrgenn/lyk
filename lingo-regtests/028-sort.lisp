(require 'regtests)

(test-is "sort list 0" (sort '() #'>) '())
(test-is "sort list 1" (sort '(60) #'>) '(60))
(test-is "sort list 2" (sort '(60 94) #'>) '(94 60))
(test-is "sort list 3" (sort '(94 60) #'>) '(94 60))
(test-is "sort list 4" (sort '(66 43 42 68 6 15 9 30 51 81
                                21 38 31 46 28 29 67 21 20 36)  #'>)
         '(81 68 67 66 51 46 43 42 38 36 31 30 29 28 21 21 20 15 9 6))

(test-is "sort vector 0" (sort #() #'>) #())
(test-is "sort vector 1" (sort #(60) #'>) #(60))
(test-is "sort vector 2" (sort #(60 94)  #'>) #(94 60))
(test-is "sort vector 3" (sort #(94 60)  #'>) #(94 60))
(test-is "sort vector 4" (sort #(66 43 42 68 6 15 9 30 51 81
                         21 38 31 46 28 29 67 21 20 36)  #'>)
         #(81 68 67 66 51 46 43 42 38 36 31 30 29 28 21 21 20 15 9 6))

(test-is "sort string 0" (sort "" #'>) "")
(test-is "sort string 1" (sort "W" #'>) "W")
(test-is "sort string 2" (sort "TF"  #'>) "TF")
(test-is "sort string 3" (sort "FT"  #'>) "TF")
(test-is "sort string 4" (sort "The quick brown fox jumps over the lazy dog."
                           #'>)
         "zyxwvuutsrrqpoooonmlkjihhgfeeedcbaT.        ")

(done-testing)
