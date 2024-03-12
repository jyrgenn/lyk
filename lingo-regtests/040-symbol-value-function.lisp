(require 'regtests)

(test-is "symbol name simple" (intern "lalala") "lalala")
(test-is "symbol name blanks" (intern "la|la la") "la|la la")
(test-is "symbol name readable" (prin1-to-string (intern "la|la la"))
         "|la\\|la la|")
(test-is "symbol name 1" (symbol-name 'lalala) "lalala")
(test-is "symbol name 2" (symbol-name '|la la la|) "la la la")

(test-is "symbol name #a" (prin1-to-string (intern "#a")) "|#a|")
(test-is "symbol name a#" (prin1-to-string (intern "a#")) "a#")
(test-is "symbol name ,a" (prin1-to-string (intern ",a")) "|,a|")
(test-is "symbol name a," (prin1-to-string (intern "a,")) "a,")
(test-is "symbol name a{" (prin1-to-string (intern "a{")) "a{")
(test-is "symbol name a}" (prin1-to-string (intern "a}")) "a}")
(test-is "symbol name a[" (prin1-to-string (intern "a[")) "a[")
(test-is "symbol name a]" (prin1-to-string (intern "a]")) "a]")
(test-is "symbol name a|" (prin1-to-string (intern "a|")) "|a\\||")
(test-is "symbol name a(" (prin1-to-string (intern "a(")) "|a(|")
(test-is "symbol name a)" (prin1-to-string (intern "a)")) "|a)|")
(test-is "symbol name a\"" (prin1-to-string (intern "a\"")) "|a\"|")
(test-is "symbol name a'" (prin1-to-string (intern "a'")) "|a'|")
(test-is "symbol name a;" (prin1-to-string (intern "a;")) "|a;|")
(test-is "symbol name a\\" (prin1-to-string (intern "a\\")) "|a\\\\|")
(test-is "symbol name a`" (prin1-to-string (intern "a`")) "|a`|")
(test-is "symbol name ." (prin1-to-string (intern ".")) "|.|")
(test-is "symbol name ," (prin1-to-string (intern ",")) "|,|")
(test-is "symbol name 119" (prin1-to-string (intern "119")) "|119|")
(test-is "symbol name 1.3e4" (prin1-to-string (intern "1.3e4")) "|1.3e4|")
(test-is "symbol name 1,3e4" (prin1-to-string (intern "1,3e4")) "1,3e4")

(defvar funsym 'double)
(defun double (n) (* 2 n))
(test-is "symbol-function double" (symbol-function funsym) #'double)

(defvar nofunsym 'foomly)
(fmakunbound 'foomly)
(test-is "symbol-function double trouble"
         (errset (symbol-function nofunsym) nil)
	 nil)

(defvar gugu 123)
(defvar gigi 'gugu)
(test-form "symbol-value 1" (lambda () (symbol-value gigi)) 123)

(makunbound 'gipsnich)
(test-err "symbol-value 2" (symbol-value 'gipsnich)
          #/unbound variable/)

(defvar usym-name "hulalla")
(defvar uninterned-1 (make-symbol usym-name))

(test-is "uninterned name" (symbol-name uninterned-1) usym-name)
(test "uninterned eq" (eq uninterned-1 uninterned-1))

(defvar uninterned-2 (make-symbol usym-name))
(test-not "uninterned !eq" (eq uninterned-1 uninterned-2))

(test-not "un/interned eq" (eq uninterned-1 (intern usym-name)))

(done-testing)
