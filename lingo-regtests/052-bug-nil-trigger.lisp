(require 'regtests)

(test-is "99 sort vector 1" (sort #(60) #'>) #(60))
(sort "W" #'>)

(done-testing)
