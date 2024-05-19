(require 'regtests)

;; this didn't previously
(test-is "format argless" (format nil "lalala") "lalala")

(test-is "format args" (format nil "fu~Adu" 'di) "fudidu")

;;; New format regime! Doing it as Lispy as possible. Within reason.
;;; Which CL's format is not.

;; testing with a real stream (even if it's a string stream), but just
;; a bit.
(test-is "format stream some" (with-output-to-string (s)
                                (format s "~3@A: the number that intrigued ~A~%"
                                        73 (string 'Sheldon '| | 'Cooper)))
         " 73: the number that intrigued Sheldon Cooper\n")

;; I think now we can resort to making the output a simple string
;; without sacrificing the efficacy of the tests.

(test-is "format char plain a" (format nil "dudidu ~C dadada" #\A)
         "dudidu A dadada")

(test-is "format char plain \\n" (format nil "dudidu ~C dadada" #\Newline)
         "dudidu \n dadada")

(test-is "format char : a" (format nil "dudidu ~:C dadada" #\A)
         "dudidu A dadada")

(test-is "format char : \\n" (format nil "dudidu ~:C dadada" #\Newline)
         "dudidu #\\Newline dadada")

(test-is "format char @ a" (format nil "dudidu ~@C dadada" #\A)
         "dudidu #\\A dadada")

(test-is "format char @ \\n" (format nil "dudidu ~@C dadada" #\Newline)
         "dudidu #\\Newline dadada")

(test-is "format char :@ a" (format nil "dudidu ~:@C dadada" #\A)
         "dudidu A dadada")

(test-is "format char :@ \\n" (format nil "dudidu ~:@C dadada" #\Newline)
         "dudidu #\\Newline dadada")

;; also tests ignoring unused args
(test-is "format %" (format nil "dudidu ~% dadada" #\Newline)
         "dudidu \n dadada")

(test-is "format % 3" (format nil "dudidu ~3% dadada" #\Newline)
         "dudidu \n\n\n dadada")

(test-err "format % par err" (format nil "dudidu ~,4% dadada" #\Newline)
         #/too many parameters in newline directive/)

(test-is "format &" (format nil "~&dudidu ~3@c~%~&dadada~&" #\@)
         "\ndudidu #\\@\ndadada\n")

(test-is "format |" (format nil "dudidu ~3@c ~|dadada" #\@)
         "dudidu #\\@ \u000cdadada")

(test-is "format 2|" (format nil "dudidu ~3@c ~2|dadada" #\@)
         "dudidu #\\@ \u000c\u000cdadada")

(test-is "format ~" (format nil "dudidu ~3@c ~~/dadada" #\@)
         "dudidu #\\@ ~/dadada")

(test-is "format 5~" (format nil "dudidu ~3@c ~5~/dadada" #\@)
         "dudidu #\\@ ~~~~~/dadada")

(test-is "format ~A plain" (format nil "dudidu ~A dadada" "heididei")
         "dudidu heididei dadada")

(test-is "format ~3A" (format nil "dudidu ~3A dadada" "heididei")
         "dudidu heididei dadada")

(test-is "format ~13A" (format nil "dudidu ~13A dadada" "heididei")
         "dudidu heididei      dadada")

(test-is "format ~13@A" (format nil "dudidu ~13@A dadada" "heididei")
         "dudidu      heididei dadada")

(test-is "format ~13,2@A" (format nil "dudidu ~13,2@A dadada" "heididei")
         "dudidu       heididei dadada")

(test-is "format ~9,,7,'.@A" (format nil "dudidu |~9,,7,'.@A| dadada"
                                     "heididei")
         "dudidu |.......heididei| dadada")

(test-is "format ~13:A" (format nil "dudidu ~13:A dadada" "heididei")
         "dudidu heididei      dadada")

(test-is "format ~13,2:A" (format nil "dudidu ~13,2:A dadada" "heididei")
         "dudidu heididei       dadada")

(test-is "format ~9,,7,'.:A" (format nil "dudidu |~9,,7,'.:A| dadada"
                                     "heididei")
         "dudidu |heididei.......| dadada")

(test-is "format ~23,,7,'.:A" (format nil "dudidu |~23,,7,'.:A| dadada"
                                      "heididei")
         "dudidu |heididei...............| dadada")

(test-is "format ~S plain" (format nil "dudidu ~S dadada" "heididei")
         "dudidu \"heididei\" dadada")

(test-is "format ~3S" (format nil "dudidu ~3S dadada" "heididei")
         "dudidu \"heididei\" dadada")

(test-is "format ~13S" (format nil "dudidu ~13S dadada" "heididei")
         "dudidu \"heididei\"    dadada")

(test-is "format ~13@S" (format nil "dudidu ~13@S dadada" "heididei")
         "dudidu    \"heididei\" dadada")

(test-is "format ~13,2@S" (format nil "dudidu ~13,2@S dadada" "heididei")
         "dudidu     \"heididei\" dadada")

(test-is "format ~9,,7,'.@S" (format nil "dudidu |~9,,7,'.@S| dadada"
                                     "heididei")
         "dudidu |.......\"heididei\"| dadada")

(test-is "format ~13:S" (format nil "dudidu ~13:S dadada" "heididei")
         "dudidu \"heididei\"    dadada")

(test-is "format ~13,2:S" (format nil "dudidu ~13,2:S dadada" "heididei")
         "dudidu \"heididei\"     dadada")

(test-is "format ~9,,7,'.:S" (format nil "dudidu |~9,,7,'.:S| dadada"
                                     "heididei")
         "dudidu |\"heididei\".......| dadada")

(test-is "format ~23,,7,'.:S" (format nil "dudidu |~23,,7,'.:S| dadada"
                                      "heididei")
         "dudidu |\"heididei\".............| dadada")


(done-testing)
