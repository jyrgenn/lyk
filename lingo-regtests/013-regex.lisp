(require 'regtests)

(defparameter url "http://golang.org/pkg/regexp/#Regexp.FindStringSubmatch")
(defparameter urlre #/^(([^:\/?\#]+):\/\/)?([^\/?\#:]*)(:([0-9]+))?(\/.*)?/)

;;; first, matches using regex-match

(test-is "regexp 1" (regex-match #/bc+/ "abcdef") '("bc"))
(test-is "regexp 2"
         (regex-match #/^(http:\/\/)([a-zA-Z0-9\.-]+)(:([0-9]+))?\/?$/
                        url)
         nil)
(test-is "regexp 3" (regex-match urlre url)
         '("http://golang.org/pkg/regexp/#Regexp.FindStringSubmatch" "http://"
           "http" "golang.org" "" "" "/pkg/regexp/#Regexp.FindStringSubmatch"))

(test-is "regexp ignore-case" (regex-match #/(?i)e./ "Ein Hase und ein Igel")
         '("Ei"))
(test-is "regexp all-matches" (regex-match #/e./ "Ein Hase und ein Igel" t)
         '(("e ") ("ei") ("el")))
(test-is "regexp ignore+all" (regex-match #/(?i)e./ "Ein Hase und ein Igel" t)
         '(("Ei") ("e ") ("ei") ("el")))
(test-is "regexp ign+all+lim" (regex-match #/(?i)e./ "Ein Hase und ein Igel"
                                             2)
         '(("Ei") ("e ")))
(test-is "regexp longer" (regex-match #/e.*e/ "Ein Hase und ein Igel")
         '("e und ein Ige"))
(test-is "regexp ungreedy" (regex-match #/(?U)e.*e/ "Ein Hase und ein Igel")
         '("e und e"))
(test-is "regexp ignore+ungreedy"
         (regex-match #/(?Ui)e.*e/ "Ein Hase und ein Igel")
         '("Ein Hase"))
(test-is "regexp ign+ungr-nl" (regex-match #/(?Ui)e.*e/
                                             "Ein\nHase und ein Igel")
         '("e und e"))
(test-is "regexp ign+ungr+dot" (regex-match #/(?Uis)e.*e/
                                              "Ein\nHase und ein Igel")
         '("Ein\nHase"))
(test-not "regexp -multi" (regex-match #/^Hase .*$/ "Ein\nHase und\n ein Igel")
          )
(test-not "regexp -mult+ungr"
         (regex-match #/(?U)^Hase .*$/ "Ein\nHase und\n ein Igel"))
(test-is "regexp multi+ungr"
         (regex-match #/(?Um)^Hase .*$/ "Ein\nHase und\n ein Igel")
         '("Hase und"))

;;; then, matches with the regexp itself as callable

(test-is "#/re/ 1" (#/bc+/ "abcdef") '("bc"))
(test-not "#/re/ 2"
          (#/^(http:\/\/)([a-zA-Z0-9\.-]+)(:([0-9]+))?\/?$/
              url))
(test-is "#/re/ 3" (urlre url)
         '("http://golang.org/pkg/regexp/#Regexp.FindStringSubmatch" "http://"
           "http" "golang.org" "" "" "/pkg/regexp/#Regexp.FindStringSubmatch"))

(test-is "#/re/ ignore-case" (#/(?i)e./ "Ein Hase und ein Igel")
         '("Ei"))
(test-is "#/re/ all-matches" (#/e./ "Ein Hase und ein Igel" t)
         '(("e ") ("ei") ("el")))
(test-is "#/re/ ignore+all" (#/(?i)e./ "Ein Hase und ein Igel" t)
         '(("Ei") ("e ") ("ei") ("el")))
(test-is "#/re/ ign+all+lim" (#/(?i)e./ "Ein Hase und ein Igel"
                                             2)
         '(("Ei") ("e ")))
(test-is "#/re/ longer" (#/e.*e/ "Ein Hase und ein Igel")
         '("e und ein Ige"))
(test-is "#/re/ ungreedy" (#/(?U)e.*e/ "Ein Hase und ein Igel")
         '("e und e"))
(test-is "#/re/ ignore+ungreedy"
         (#/(?Ui)e.*e/ "Ein Hase und ein Igel")
         '("Ein Hase"))
(test-is "#/re/ ign+ungr-nl" (#/(?Ui)e.*e/
                                             "Ein\nHase und ein Igel")
         '("e und e"))
(test-is "#/re/ ign+ungr+dot" (#/(?Uis)e.*e/
                                              "Ein\nHase und ein Igel")
         '("Ein\nHase"))
(test-is "#/re/ -multi" (#/^Hase .*$/ "Ein\nHase und\n ein Igel")
         '())
(test-is "#/re/ -mult+ungr"
         (#/(?U)^Hase .*$/ "Ein\nHase und\n ein Igel")
         '())
(test-is "#/re/ multi+ungr"
         (#/(?Um)^Hase .*$/ "Ein\nHase und\n ein Igel")
         '("Hase und"))

;; check that #'regexp handles regexps correctly

(test-is "#'regexp string" (let ((re (regexp "^.*$")))
                              (and (regexpp re)
                                   re))
         #/^.*$/)

(test-is "#'regexp bumber" (let ((re (regexp "444")))
                              (and (regexpp re)
                                   re))
         #/444/)

(test-is "#'regexp regexp" (let ((re (regexp #r{^.*$})))
                              (and (regexpp re)
                                   re))
         #/^.*$/)

(test-is "replace 1" (regex-replace "b(..)" "hubaluba" "c$1") "hucaluba")
(test-is "replace 2" (regex-replace "b(.)" "hubaluba" "c$1") "hucaluca")

(test-is "replace 3" (regex-replace #/a(..)/ "mango bardy hard schoomaly"
                                    "${1}b")
         "mngbo brdby hrdb schoomlyb")

(test-is "replace 4" (regex-replace #/a(..)/ "mango bardy hard schoomaly"
                                    "${1}gooey")
         "mnggooeyo brdgooeyy hrdgooey schoomlygooey")
(test-is "replace 5" (regex-replace #/a(..)/ "mango bardy hard schoomaly"
                                    "gooey$1")
         "mgooeyngo bgooeyrdy hgooeyrd schoomgooeyly")
(test-is "replace 6" (regex-replace #/a(..)/ "mango bardy hard schoomaly"
                                    "gooey")
         "mgooeyo bgooeyy hgooey schoomgooey")

(done-testing)
