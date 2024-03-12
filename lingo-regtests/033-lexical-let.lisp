(require 'regtests)

(test-is "let nil" (let (a) a) nil)
(test-is "setq in let" (let (a) (setq a 115) a) 115)

(done-testing)
