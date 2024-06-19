(require 'regtests)

(test-is "#' reader macro" '#'blubber '(function blubber))

(test-is "#/ regexp 1" #/a regexp/ "#/a regexp/")
(test-is "#r{ regexp 2" #r{b regexp} "#/b regexp/")
(test-is "#r[ regexp /" #r[c /regexp/] "#/c \\/regexp\\//")
(test-is "#r< regexp /\\" #r<d /re\\gexp/> "#/d \\/re\\\\gexp\\//")
(test-is "#r, regexp /\\" #r,e /re\\gexp/, "#/e \\/re\\\\gexp\\//")


(done-testing)
