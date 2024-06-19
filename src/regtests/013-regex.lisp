(require 'regtests)

(defparameter url "http://golang.org/pkg/regexp/#Regexp.FindStringSubmatch")
(defparameter urlre #/^(([^:\/?\#]+):\/\/)?([^\/?\#:]*)(:([0-9]+))?(\/.*)?/)

;;; first, matches using regexp-match

(test-is "regexp 1" (regexp-match #/bc+/ "abcdef") '("bc"))
(test-is "regexp 2"
         (regexp-match #/^(http:\/\/)([a-zA-Z0-9\.-]+)(:([0-9]+))?\/?$/
                        url)
         nil)
(test-is "regexp 3" (regexp-match urlre url)
         '("http://golang.org/pkg/regexp/#Regexp.FindStringSubmatch" "http://"
           "http" "golang.org" "" "" "/pkg/regexp/#Regexp.FindStringSubmatch"))

(test-is "regexp ignore-case" (regexp-match #/(?i)e./ "Ein Hase und ein Igel")
         '("Ei"))
(test-is "regexp all-matches" (regexp-match #/e./ "Ein Hase und ein Igel" t)
         '(("e ") ("ei") ("el")))
(test-is "regexp ignore+all" (regexp-match #/(?i)e./ "Ein Hase und ein Igel" t)
         '(("Ei") ("e ") ("ei") ("el")))
(test-is "regexp ign+all+lim" (regexp-match #/(?i)e./ "Ein Hase und ein Igel"
                                             2)
         '(("Ei") ("e ")))
(test-is "regexp longer" (regexp-match #/e.*e/ "Ein Hase und ein Igel")
         '("e und ein Ige"))
(test-is "regexp ungreedy" (regexp-match #/e.*?e/ "Ein Hase und ein Igel")
         '("e und e"))
(test-is "regexp ignore+ungreedy"
         (regexp-match #/(?i)e.*?e/ "Ein Hase und ein Igel")
         '("Ein Hase"))
(test-is "regexp ign+ungr-nl" (regexp-match #/(?i)e.*?e/
                                             "Ein\nHase und ein Igel")
         '("e und e"))
(test-is "regexp ign+ungr+dot" (regexp-match #/(?is)e.*?e/
                                              "Ein\nHase und ein Igel")
         '("Ein\nHase"))
(test-not "regexp -multi" (regexp-match #/^Hase .*$/ "Ein\nHase und\n ein Igel")
          )
(test-not "regexp -mult+ungr"
         (regexp-match #/^Hase .*?$/ "Ein\nHase und\n ein Igel"))
(test-is "regexp multi+ungr"
         (regexp-match #/(?m)^Hase .*?$/ "Ein\nHase und\n ein Igel")
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

(test-is "replace 1" (regexp-replace "b(..)" "hubaluba" "c$1") "hucaluba")
(test-is "replace 2" (regexp-replace "b(.)" "hubaluba" "c$1") "hucaluca")

(test-is "replace 3" (regexp-replace #/a(..)/ "mango bardy hard schoomaly"
                                     "$1b")
         "mngbo brdby hrdb schoomlyb")

(test-is "replace 4" (regexp-replace #/a(..)/ "mango bardy hard schoomaly"
                                    "$1gooey")
         "mnggooeyo brdgooeyy hrdgooey schoomlygooey")
(test-is "replace 5" (regexp-replace #/a(..)/ "mango bardy hard schoomaly"
                                    "gooey$1")
         "mgooeyngo bgooeyrdy hgooeyrd schoomgooeyly")
(test-is "replace 6" (regexp-replace #/a(..)/ "mango bardy hard schoomaly"
                                    "gooey")
         "mgooeyo bgooeyy hgooey schoomgooey")

(test-is "match-date" (car (regexp-match #/\d\d\d\d-\d\d-\d\d/
                                         "234234322024-04-16005"))
         "2024-04-16")

(done-testing)
