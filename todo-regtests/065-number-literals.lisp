(require 'regtests)

(test-is "binary 1" #b10101 21)
(test-is "binary 2" #b0 0)
(test-is "binary 3" #b1 1)
(test-is "binary 4" #b10101011 171)
(test-is "binary 5" #b10101011101001110010010000111001001010011000100
         94367177807044)
(test-is "binary 6" (errset (read "#b10121011") nil) nil)

(test-is "octal 0" #o0 0)
(test-is "octal 1" #o1 1)
(test-is "octal 2" #o177 127)
(test-is "octal 3" #o1237 671)
(test-is "octal 4" #o37777777777 4294967295)
(test-is "octal 5" #o3777777777777 274877906943)
(test-is "octal 6" (errset (read "#o3777777977777") nil) nil)

(test-is "hex 0" #x0 0)
(test-is "hex 1" #x1 1)
(test-is "hex 2" #xdada 56026)
(test-is "hex 3" #xDADA 56026)
(test-is "hex 4" #xFFFFFFFF 4294967295)
(test-is "hex 5" #x100000001 4294967297)
(test-is "hex 6" (errset (read "#x100g00001") nil) nil)

(done-testing)
