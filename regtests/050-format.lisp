(require 'regtests)

;;; Bug!
(test-is "format 130463" (format nil "~R" 130463)
         "one hundred thirty thousand four hundred sixty-three")

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

(test-is "format ign.newline"
         (format nil "dudidu ~
                      ~13,,,'_S ~:
                      shoobeedoobeedooah ,~@
                      ," 'schlumbada)
         "dudidu schlumbada___                       shoobeedoobeedooah ,\n,")

;; I converted the examples from that page to all using ~R
(test-is "CLHS 22.3.2.1 R a" (format nil "~2,,,' ,4:R" 13)
         "1101")

(test-is "CLHS 22.3.2.1 R b" (format nil "~2,,,' ,4:R" 17)
         "1 0001")

(test-is "CLHS 22.3.2.1 R c" (format nil "~2,19,'0,' ,4:R" 3333)
         "0000 1101 0000 0101")

(test-is "CLHS 22.3.2.1 R c2" (format nil "~2,16,'0,' ,4R" 3333)
         "0000110100000101")

(test-is "CLHS 22.3.2.1 R d" (format nil "~3,,,' ,2:R" 17)
         "1 22")

(test-is "CLHS 22.3.2.1 R e" (format nil "~10,,,'|,2:R" #xFFFF)
         "6|55|35")

(test-is "format mellum_addr" (format nil "~2,35,'0,'.,8:R" #x0A15D521)
         "00001010.00010101.11010101.00100001")


;; I converted the examples from that page to all using ~B
(test-is "CLHS 22.3.2.1 B a" (format nil "~,,' ,4:B" 13)
         "1101")

(test-is "CLHS 22.3.2.1 B b" (format nil "~,,' ,4:B" 17)
         "1 0001")

(test-is "CLHS 22.3.2.1 B c" (format nil "~19,'0,' ,4:B" 3333)
         "0000 1101 0000 0101")

(test-is "CLHS 22.3.2.1 B c2" (format nil "~16,'0B" 3333)
         "0000110100000101")

(test-is "CLHS 22.3.2.1 B d" (format nil "~,,' ,2:B" 17)
         "1 00 01")

(test-is "CLHS 22.3.2.1 B e" (format nil "~,,'|,2:B" #xFFFF)
         "11|11|11|11|11|11|11|11")

(test-is "format mellum_addr B" (format nil "~35,'0,'.,8:B" #x0A15D521)
         "00001010.00010101.11010101.00100001")


;; I converted the examples from that page to all using ~D
(test-is "CLHS 22.3.2.1 D a" (format nil "~,,' ,4:D" 3463)
         "3463")

(test-is "CLHS 22.3.2.1 D b" (format nil "~,,' ,4:D" 130463)
         "13 0463")

(test-is "CLHS 22.3.2.1 D c" (format nil "~19,'0,' ,4:D" 33333)
         "0000 0000 0003 3333")

(test-is "CLHS 22.3.2.1 D c2" (format nil "~16,'0D" 3333)
         "0000000000003333")

(test-is "CLHS 22.3.2.1 D d" (format nil "~,,' ,2:D" 17924)
         "1 79 24")

(test-is "CLHS 22.3.2.1 D e" (format nil "~,,'|,2:D" #xFFFF)
         "6|55|35")

(test-is "format mellum_addr D" (format nil "~35,'0,'.,8:D" #x0A15D521)
         "00000000.00000000.00000001.69202977")


;; I converted the examples from that page to all using ~O
(test-is "CLHS 22.3.2.1 O a" (format nil "~,,' ,4:O" 3463)
         "6607")

(test-is "CLHS 22.3.2.1 O b" (format nil "~,,' ,4:O" 130463)
         "37 6637")

(test-is "CLHS 22.3.2.1 O c" (format nil "~19,'0,' ,4:O" 33333)
         "0000 0000 0010 1065")

(test-is "CLHS 22.3.2.1 O c2" (format nil "~16,'0o" 33333)
         "0000000000101065")

(test-is "CLHS 22.3.2.1 O d" (format nil "~,,' ,2:O" 17924)
         "4 30 04")

(test-is "CLHS 22.3.2.1 O e" (format nil "~,,'|,2:O" #xFFFF)
         "17|77|77")

(test-is "format mellum_addr O" (format nil "~35,'0,'.,8:O" #x0A15D521)
         "00000000.00000000.00000012.05352441")

(test-is "format octal 1" (format nil "~12,'0,' ,3:O" 34359738367)
         "377 777 777 777")

(test-is "format octal 2" (format nil "~15,'0,' ,3:O" 2147483646)
         "017 777 777 776")


;; I converted the examples from that page to all using ~X
(test-is "CLHS 22.3.2.1 X a" (format nil "~,,' ,4:X" 3463)
         "d87")

(test-is "CLHS 22.3.2.1 X b" (format nil "~,,' ,4:X" 130463)
         "1 fd9f")

(test-is "CLHS 22.3.2.1 X c" (format nil "~19,'0,' ,4:X" 3333333)
         "0000 0000 0032 dcd5")

(test-is "CLHS 22.3.2.1 X c2" (format nil "~16,'0x" 3333333)
         "000000000032dcd5")

(test-is "CLHS 22.3.2.1 X d" (format nil "~,,' ,2:X" 17924)
         "46 04")

(test-is "CLHS 22.3.2.1 X e" (format nil "~,,'|,2:X" #xFFFF)
         "ff|ff")

(test-is "format mellum_addr X" (format nil "~35,'0,'.,8:X" #x0A15D521)
         "00000000.00000000.00000000.0a15d521")

(test-is "format hex 1" (format nil "~12,'0,' ,4:X" 34359738367)
         "07 ffff ffff")

(test-is "format hex 2" (format nil "~15,'0,' ,4:X" 2147483646)
         " 0000 7fff fffe")


(test-is "Roman 1" (format nil "~@R" 1963)
         "MCMLXIII")

(test-is "Roman 2" (format nil "~@R" 963)
         "CMLXIII")

(test-is "Roman 3" (format nil "~@R" 1944)
         "MCMXLIV")

(test-is "Roman 4" (format nil "~@R" 1437)
         "MCDXXXVII")

(test-is "Roman 6" (format nil "~@R" 1999)
         "MCMXCIX")

(test-err "Roman 7" (format nil "~@R" 4999) #/too large/)

(test-err "Roman 8" (format nil "~@R" 0) #/too small/)

(test-is "Old Roman 1" (format nil "~:@R" 1963)
         "MDCCCCLXIII")

(test-is "Old Roman 2" (format nil "~:@R" 963)
         "DCCCCLXIII")

(test-is "Old Roman 3" (format nil "~@:R" 1944)
         "MDCCCCXXXXIIII")

(test-is "Old Roman 4" (format nil "~@:R" 1437)
         "MCCCCXXXVII")

(test-is "Old Roman 6" (format nil "~:@R" 1999)
         "MDCCCCLXXXXVIIII")

(test-err "Old Roman 7" (format nil "~:@R" 4999) #/too large/)

(test-err "Old Roman 8" (format nil "~:@R" 0) #/too small/)

(test-is "English Cardinal 0" (format nil "~R" 0)
         "zero")

(test-is "English Cardinal 1" (format nil "~R" 1)
         "one")

(test-is "English Cardinal 2" (format nil "~R" 2)
         "two")

(test-is "English Cardinal 3" (format nil "~R" 3)
         "three")

(test-is "English Cardinal 4" (format nil "~R" 4)
         "four")

(test-is "English Cardinal 13" (format nil "~R" 13)
         "thirteen")

(test-is "English Cardinal -1" (format nil "~R" -1)
         "negative one")

(test-is "English Cardinal 14" (format nil "~R" 14)
          "fourteen")

;;; now frr realz!

(test-is "English Cardinal 114" (format nil "~R" 114)
          "one hundred fourteen")

(test-is "English Cardinal 3114" (format nil "~R" 3114)
          "three thousand one hundred fourteen")

(test-is "English Cardinal 43114" (format nil "~R" 43114)
          "fourty-three thousand one hundred fourteen")

(test-is "English Cardinal 543114" (format nil "~R" 543114)
          "five hundred fourty-three thousand one hundred fourteen")

(test-is "English Cardinal 6543114" (format nil "~R" 6543114)
          "six million five hundred fourty-three thousand one hundred fourteen")

(test-is "English Cardinal 76543114" (format nil "~R" 76543114)
         "seventy-six million five hundred fourty-three thousand one hundred fourteen")

(test-is "English Cardinal 876543114" (format nil "~R" 876543114)
         "eight hundred seventy-six million five hundred fourty-three thousand one hundred fourteen")

(test-is "English Cardinal 9876543114" (format nil "~R" 9876543114)
         "nine billion eight hundred seventy-six million five hundred fourty-three thousand one hundred fourteen")

(test-is "English Cardinal 89876543114" (format nil "~R" 89876543114)
         "eighty-nine billion eight hundred seventy-six million five hundred fourty-three thousand one hundred fourteen")

(test-is "English Cardinal 789876543114" (format nil "~R" 789876543114)
         "seven hundred eighty-nine billion eight hundred seventy-six million five hundred fourty-three thousand one hundred fourteen")

(test-is "English Cardinal 6789876543114" (format nil "~R" 6789876543114)
         "six trillion seven hundred eighty-nine billion eight hundred seventy-six million five hundred fourty-three thousand one hundred fourteen")

(test-is "English Cardinal 56789876543114" (format nil "~R" 56789876543114)
         "fifty-six trillion seven hundred eighty-nine billion eight hundred seventy-six million five hundred fourty-three thousand one hundred fourteen")

(test-is "English Cardinal 456789876543114" (format nil "~R" 456789876543114)
         "four hundred fifty-six trillion seven hundred eighty-nine billion eight hundred seventy-six million five hundred fourty-three thousand one hundred fourteen")

(test-is "English Cardinal 3456789876543114" (format nil "~R" 3456789876543114)
         "three quadrillion four hundred fifty-six trillion seven hundred eighty-nine billion eight hundred seventy-six million five hundred fourty-three thousand one hundred fourteen")

;; (test-is "English Cardinal 23456789876543114"
;;          (format nil "~R" 23456789876543114)
;;          "twenty three quadrillion four hundred fifty-six trillion seven hundred eighty-nine billion eight hundred seventy-six million five hundred fourty-three thousand one hundred fourteen")

;; (test-is "English Cardinal 123456789876543114"
;;          (format nil "~R" 123456789876543114)
;;          "one hundred twenty three quadrillion four hundred fifty-six trillion seven hundred eighty-nine billion eight hundred seventy-six million five hundred fourty-three thousand one hundred fourteen")

;; (test-is "English Cardinal 2123456789876543114"
;;          (format nil "~R" 2123456789876543114)
;;          "two quintillion one hundred twenty three quadrillion four hundred fifty-six trillion seven hundred eighty-nine billion eight hundred seventy-six million five hundred fourty-three thousand one hundred fourteen")

;; (test-is "English Cardinal 32123456789876543114"
;;          (format nil "~R" 32123456789876543114)
;;          "thirty-two quintillion one hundred twenty three quadrillion four hundred fifty-six trillion seven hundred eighty-nine billion eight hundred seventy-six million five hundred fourty-three thousand one hundred fourteen")

;;; Holy shit! This stops to work when the "integer" is so large that
;;; the mantissa of the floating point representation isn't long
;;; enough to hold all digits.
;; (test-is "int 2123456789876543114" 2123456789876543114
;;          "2123456789876543114")

(test-is "English Ordinal 0" (format nil "~:R" 0)
         "zeroth")

(test-is "English Ordinal 1" (format nil "~:R" 1)
         "first")

(test-is "English Ordinal 2" (format nil "~:R" 2)
         "second")

(test-is "English Ordinal 3" (format nil "~:R" 3)
         "third")

(test-is "English Ordinal 4" (format nil "~:R" 4)
         "fourth")

(test-is "English Ordinal 13" (format nil "~:R" 13)
         "thirteenth")

(test-is "English Ordinal -1" (format nil "~:R" -1)
         "negative first")

(test-is "English Ordinal 14" (format nil "~:R" -14)
         "negative fourteenth")


;;; 22.3.11 Examples of FORMAT

(test-is "CLHS 22.3.11 1" (format nil "foo")
         "foo")

(defvar x 5)
(test-is "CLHS 22.3.11 2" (format nil "The answer is ~D." x)
         "The answer is 5.")
(test-is "CLHS 22.3.11 3" (format nil "The answer is ~3D." x)
         "The answer is   5.")
(test-is "CLHS 22.3.11 4" (format nil "The answer is ~3,'0D." x)
         "The answer is 005.")
(test-is "CLHS 22.3.11 5" (format nil "The answer is ~:D." (expt 47 x))
         "The answer is 229,345,007.")

(defvar y "elephant")
(test-is "CLHS 22.3.11 6" (format nil "Look at the ~A!" y)
         "Look at the elephant!")

(defun foo (x)
  (format nil "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F"
          x x x x x x))
(test-is "CLHS 22.3.11 7" (foo 3.14159)
         "  3.14| 31.42|  3.14|3.1416|3.14|3.14159")
(test-is "CLHS 22.3.11 8" (foo -3.14159)
         " -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159")
(test-is "CLHS 22.3.11 9" (foo 100.0)
         "100.00|******|100.00| 100.0|100.00|100.0")
(test-is "CLHS 22.3.11 10" (foo 1234.0)
         "1234.00|******|??????|1234.0|1234.00|1234.0")
(test-is "CLHS 22.3.11 11" (foo 0.006)
         "  0.01|  0.06|  0.01| 0.006|0.01|0.006")

;;; ~E format is NYI

;; (defun foo (x)  
;;   (format nil
;;           "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
;;             ~9,3,2,-2,'%@E|~9,2E"
;;           x x x x))
;; (test-is "CLHS 22.3.11 12" (foo 3.14159)
;;          "  3.14E+0| 31.42$-01|+.003E+03|  3.14E+0")
;; (test-is "CLHS 22.3.11 13" (foo -3.14159)
;;          " -3.14E+0|-31.42$-01|-.003E+03| -3.14E+0")
;; (test-is "CLHS 22.3.11 14" (foo 1100.0)
;;          "  1.10E+3| 11.00$+02|+.001E+06|  1.10E+3")
;; (test-is "CLHS 22.3.11 15" (foo 1100.0L0)
;;          "  1.10L+3| 11.00$+02|+.001L+06|  1.10L+3")
;; (test-is "CLHS 22.3.11 16" (foo 1.1E13)
;;          "*********| 11.00$+12|+.001E+16| 1.10E+13")
;; (test-is "CLHS 22.3.11 17" (foo 1.1L120)
;;          "*********|??????????|%%%%%%%%%|1.10L+120")
;; (test-is "CLHS 22.3.11 18" (foo 1.1L1200)
;;          "*********|??????????|%%%%%%%%%|1.10L+1200")

;; ;;; As an example of the effects of varying the scale factor, the code

;; (test-is "CLHS 22.3.11 19" (dotimes (k 13)
;;                             (format nil "~%Scale factor ~2D: |~13,6,2,VE|"
;;                                     (- k 5) (- k 5) 3.14159))
;;          "
;; Scale factor -5: | 0.000003E+06|
;; Scale factor -4: | 0.000031E+05|
;; Scale factor -3: | 0.000314E+04|
;; Scale factor -2: | 0.003142E+03|
;; Scale factor -1: | 0.031416E+02|
;; Scale factor  0: | 0.314159E+01|
;; Scale factor  1: | 3.141590E+00|
;; Scale factor  2: | 31.41590E-01|
;; Scale factor  3: | 314.1590E-02|
;; Scale factor  4: | 3141.590E-03|
;; Scale factor  5: | 31415.90E-04|
;; Scale factor  6: | 314159.0E-05|
;; Scale factor  7: | 3141590.E-06|")

;;; ~G format is NYI

;; (defun foo (x)
;;   (format nil "~9,2,1,,'*G|~9,3,2,3,'?,,'$G|~9,3,2,0,'%G|~9,2G"
;;           x x x x))
;; (test-is "CLHS 22.3.11 20" (foo 0.0314159)
;;          "  3.14E-2|314.2$-04|0.314E-01|  3.14E-2")
;; (test-is "CLHS 22.3.11 21" (foo 0.314159)
;;          "  0.31   |0.314    |0.314    | 0.31    ")
;; (test-is "CLHS 22.3.11 22" (foo 3.14159)
;;          "   3.1   | 3.14    | 3.14    |  3.1    ")
;; (test-is "CLHS 22.3.11 23" (foo 31.4159)
;;          "   31.   | 31.4    | 31.4    |  31.    ")
;; (test-is "CLHS 22.3.11 24" (foo 314.159)
;;          "  3.14E+2| 314.    | 314.    |  3.14E+2")
;; (test-is "CLHS 22.3.11 25" (foo 3141.59)
;;          "  3.14E+3|314.2$+01|0.314E+04|  3.14E+3")
;; (test-is "CLHS 22.3.11 26" (foo 3141.59L0)
;;          "  3.14L+3|314.2$+01|0.314L+04|  3.14L+3")
;; (test-is "CLHS 22.3.11 27" (foo 3.14E12)
;;          "*********|314.0$+10|0.314E+13| 3.14E+12")
;; (test-is "CLHS 22.3.11 28" (foo 3.14L120)
;;          "*********|?????????|%%%%%%%%%|3.14L+120")
;; (test-is "CLHS 22.3.11 29" (foo 3.14L1200)
;;          "*********|?????????|%%%%%%%%%|3.14L+1200")


(test-is "CLHS 22.3.11 30"  (format nil "Written to ~A." "foo.bin")
         "Written to foo.bin.")

(test-is "Dollar all" (format nil "~3,6,13,'+$" 113.2)
         "+++000113.200")

(test-is "Dollar all neg." (format nil "~3,6,13,'+$" -113.2)
         "++-000113.200")


(done-testing)
