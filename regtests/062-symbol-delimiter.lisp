;; "Single quote not seen as delimiter when reading symbol" #40

(require 'regtests)

;; reproduce:
;; (read "(eq arg '{')") => (eq arg (quote |{'|))

(test-is "delim 1" (errset (read "(eq arg '{')") nil) nil)
(test-is "delim {'" (errset (read "('{')") nil) nil)
(test-is "delim a'" (read "a'") 'a)
(test-is "delim a|" (read "a|") 'a)
(test-is "delim a(" (read "a(") 'a)
(test-is "delim a)" (read "a)") 'a)
(test-is "delim a\"" (read "a\"") 'a)
(test-is "delim a;" (read "a;") 'a)
(test-is "delim a\\" (read "a\\") 'a)
(test-is "delim a`" (read "a`") 'a)

(done-testing)
