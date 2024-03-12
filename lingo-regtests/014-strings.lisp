(require 'regtests)

(test-is "split-string 1" (split-string "/Users/ni/src/jnil/lib"
                                         "/")
         '("" "Users" "ni" "src" "jnil" "lib"))
(test-is "split-string 2" (split-string "take me to the matador")
         '("take" "me" "to" "the" "matador"))
(test-is "split-string 3" (split-string "/Users/ni/src/jnil/lib"
                                         "/" 3)
         '("" "Users" "ni/src/jnil/lib"))
(test-is "split-string 4" (split-string "/Users/ni/src/jnil/lib"
                                         "/" 0)
         '())
(test-is "split-string 5" (split-string "/Users/ni/src/jnil/lib"
                                         "/" -1)
         '("" "Users" "ni" "src" "jnil" "lib"))
(test-is "split-string 6" (split-string "/Users/ni/src/jnil/lib"
                                         "/" 1)
         '("/Users/ni/src/jnil/lib"))
(test-is "split-string 7" (split-string "/Users/ni/src/jnil/lib"
                                         #r{/.} 3)
         '("" "sers" "i/src/jnil/lib"))
(test-is "split-string 8" (split-string "/Users/ni/src/jnil/lib"
                                         #r{/.})
         '("" "sers" "i" "rc" "nil" "ib"))
(test-is "split-string 9" (split-string "/Users/ni/src/jnil/lib"
                                         #r{/.} -3)
         '("" "sers" "i" "rc" "nil" "ib"))


(test-is "intern 1" (intern "lala") "lala")
(test-is "intern 2" (type-of (intern "lala")) "symbol")


(test-is "string 1" (list (string 13)) "(\"13\")")
(test-is "string 2" (list (string "abc")) "(\"abc\")")

(defvar Uurl)
(defparameter url "http://golang.org/pkg/regexp/#Regexp.FindStringSubmatch")

(test-is "string-upcase 1" (setq Uurl (string-upcase url))
         "HTTP://GOLANG.ORG/PKG/REGEXP/#REGEXP.FINDSTRINGSUBMATCH")
(test-is "string-upcase 2" (string-upcase url 23)
         "http://golang.org/pkg/rEGEXP/#REGEXP.FINDSTRINGSUBMATCH")
(test-is "string-upcase 3" (string-upcase url 23 50)
         "http://golang.org/pkg/rEGEXP/#REGEXP.FINDSTRINGSUBmatch")
(test-is "string-upcase 4" (string-upcase url 123 50) url)

(test-is "string-downcase 1" (string-downcase Uurl)
         "http://golang.org/pkg/regexp/#regexp.findstringsubmatch")
(test-is "string-downcase 2" (string-downcase Uurl 23)
         "HTTP://GOLANG.ORG/PKG/Regexp/#regexp.findstringsubmatch")
(test-is "string-downcase 3" (string-downcase Uurl 23 50)
         "HTTP://GOLANG.ORG/PKG/Regexp/#regexp.findstringsubMATCH")
(test-is "string-downcase 4" (string-downcase Uurl 123 50)
         Uurl)

(test-is "string-capitalize 1" (string-capitalize url)
         "Http://Golang.Org/Pkg/Regexp/#Regexp.Findstringsubmatch")
(test-is "string-capitalize 2" (string-capitalize url 23)
         "http://golang.org/pkg/regexp/#Regexp.Findstringsubmatch")
(test-is "string-capitalize 3" (string-capitalize url 23 45)
         "http://golang.org/pkg/regexp/#Regexp.FindstringSubmatch")
(test-is "string-capitalize 4" (string-capitalize url 123 50) url)

(defvar wandersmann "dA gehT deR kleinE wandersManN")
(test-is "string-capitalize 5" (string-capitalize wandersmann)
         "Da Geht Der Kleine Wandersmann")
(test-is "string-capitalize 6" (string-capitalize wandersmann 10)
         "dA gehT der Kleine Wandersmann")
(test-is "string-capitalize 7" (string-capitalize wandersmann 10 27)
         "dA gehT der Kleine WandersmanN")

(test-is "string escape \\octal" "sales plummeted by 32 \045 this month"
         "sales plummeted by 32 % this month")

(test-is "string escape \\hex" "sales plummeted by 32 \x25 this month"
         "sales plummeted by 32 % this month")

(test-is "string escape \\unicode" "sales plummeted by 32 \u0025 this month"
         "sales plummeted by 32 % this month")

(test-is "string escape \\Unicode" "sales plummeted by 32 \U00000025 this month"
         "sales plummeted by 32 % this month")


(test-is "string trim 1" (string-trim t " haha\t") "haha")
(test-is "string trim 2" (string-trim t "\n haha\t") "haha")

(test-is "string trim 3" (string-trim "abcde" "haahaa") "haah")
(test-is "string trim 4" (string-trim " .,;/%!" ", das ist perfekt!")
         "das ist perfekt")

(test-is "string left trim 1" (string-left-trim t " haha\t") "haha\t")
(test-is "string left trim 2" (string-left-trim t "\n haha\t") "haha\t")

(test-is "string left trim 3" (string-left-trim "abcde" "haahaa") "haahaa")
(test-is "string left trim 4" (string-left-trim " .,;/%!" ", das ist perfekt!")
         "das ist perfekt!")

(test-is "string right trim 1" (string-right-trim t " haha\t") " haha")
(test-is "string right trim 2" (string-right-trim t "\n haha\t") "\n haha")

(test-is "string right trim 3" (string-right-trim "abcde" "haahaa") "haah")
(test-is "string right trim 4" (string-right-trim " .,;/%!"
                                                   ", das ist perfekt!")
         ", das ist perfekt")

;; the following examples from the CLHS
(test-is "string trim CLHS 1" (string-trim "abc" "abcaakaaakabcaaa")
         "kaaak")

(test-is "string trim CLHS 2" (string-trim '(#\Space #\Tab #\Newline)
                                            " garbanzo beans
        ")
         "garbanzo beans")
         
(test-is "string trim CLHS 3" (string-trim " (*)"
                                            " ( *three (silly) words* ) ")
         "three (silly) words")

(test-is "string trim CLHS 4" (string-left-trim "abc" "labcabcabc")
         "labcabcabc")

(test-is "string trim CLHS 5" (string-left-trim " (*)"
                                                 " ( *three (silly) words* ) ")
         "three (silly) words* ) ")

(test-is "string trim CLHS 6" (string-right-trim " (*)"
                                                  " ( *three (silly) words* ) ")
         " ( *three (silly) words")

(test-is "string trim vector" (string-trim #(#\Space ?( ?* ?))
                                            " ( *three (silly) words* ) ")
         "three (silly) words")

(test-is "substring 0" (substring "shnargldings" 0) "shnargldings")
(test-is "substring 1" (substring "shnargldings" 3) "argldings")
(test-is "substring 2" (substring "shnargldings" 1 5) "hnar")
(test-is "substring 3" (substring "shnargldings" 1 nil) "hnargldings")
;; alas, -1 is like nil (and not easy to avoid)
(test-is "substring 4" (substring "shnargldings" 0 -1) "shnargldings")
(test-is "substring 5" (substring "shnargldings" 0 12) "shnargldings")
(test-is "substring 6" (substring "shnargldings" 0 13) "shnargldings")
(test-is "substring 7" (substring "shnargldings" 1 117) "hnargldings")
(test-err "substring 8" (substring "shnargldings" -3 117)
          #/negative substring start index/)
(test-err "substring 9" (substring "shnargldings" nil 'a)
          #/start argument is not an integer/)
(test-err "substring 10" (substring "shnargldings" 4 'a)
          #/end argument is not an integer/)

(test-is "plural-s 0" (plural-s 0) "s")
(test-is "plural-s 1" (plural-s 1) "")
(test-is "plural-s 2" (plural-s 2) "s")
(test-is "plural-s 3" (plural-s 3) "s")
(test-is "plural-s 67" (plural-s 67) "s")
(test-is "plural-s 0l" (plural-s '()) "s")
(test-is "plural-s 1l" (plural-s '(a)) "")
(test-is "plural-s 2l" (plural-s '(a b)) "s")
(test-is "plural-s 3l" (plural-s '(a b c)) "s")
(test-is "plural-s 67l" (plural-s '(make-list 67 "dingenskirchen")) "s")

(test-is "plural-ies 0" (plural-ies 0) "ies")
(test-is "plural-ies 1" (plural-ies 1) "y")
(test-is "plural-ies 2" (plural-ies 2) "ies")
(test-is "plural-ies 3" (plural-ies 3) "ies")
(test-is "plural-ies 67" (plural-ies 67) "ies")
(test-is "plural-ies 0l" (plural-ies '()) "ies")
(test-is "plural-ies 1l" (plural-ies '(a)) "y")
(test-is "plural-ies 2l" (plural-ies '(a b)) "ies")
(test-is "plural-ies 3l" (plural-ies '(a b c)) "ies")
(test-is "plural-ies 67l" (plural-ies '(make-list 67 "dingenskirchen")) "ies")


(done-testing)
