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

(test-err "English Cardinal -1" (format nil "~R" -1)
          #/too small/)

(test-err "English Cardinal 14" (format nil "~R" 14)
          #/too large/)

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

(test-err "English Ordinal -1" (format nil "~:R" -1)
          #/too small/)

(test-err "English Ordinal 14" (format nil "~R" 14)
          #/too large/)


(done-testing)
