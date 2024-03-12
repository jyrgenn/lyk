(require 'regtests)

(test-is "#' reader macro" '#'blubber '(function blubber))

(test-is "#/ regexp 1" #/a regexp/ "#/a regexp/")
(test-is "#r{ regexp 2" #r{a regexp} "#/a regexp/")
(test-is "#r[ regexp /" #r[a /regexp/] "#/a \\/regexp\\//")
(test-is "#r< regexp /\\" #r<a /re\\gexp/> "#/a \\/re\\\\gexp\\//")
(test-is "#r, regexp /\\" #r,a /re\\gexp/, "#/a \\/re\\\\gexp\\//")


(done-testing)
