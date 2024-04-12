(require 'regtests)

(test-is "chars 0" (read "#\\@") #\@)
(test-is "chars 1" (read "#\\100") #\@)
(test-is "chars 2" (read "#\\b01000000") #\@)
(test-is "chars 2e" (errset (read "#\\b01000020") nil) nil)
(test-is "chars 3" (read "#\\x40") #\@)
(test-is "chars 3X" (read "#\\X40") #\@)
(test-is "chars 4" (read "#\\u0040") #\@)
(test-is "chars 5" (errset (read "#\\u40") nil) nil)
(test-is "chars 6" (errset (read "#\\U40") nil) nil)
(test-is "chars 7" (errset (read "#\\U0040") nil) nil)
(test-is "chars 8" (read "#\\U00000040") #\@)
(test-is "chars 15" (read "#\\Space") #\x20)
(test-is "chars 16" (read "#\\Newline") #\x0a)
(test-is "chars 17" (read "#\\Tab") #\x09)
(test-is "chars 18" (read "#\\Page") #\x0c)
(test-is "chars 19" (read "#\\Rubout") #\x7f)
(test-is "chars 20" (read "#\\Linefeed") #\x0a)
(test-is "chars 21" (read "#\\Return") #\x0d)
(test-is "chars 22" (read "#\\Backspace") #\x08)
(test-is "chars 23" (read "#\\Bell") #\x07)

;; we should be able to do at least the ASCII charset
(test-is "char 0x00" (read "#\\ ") #\x00)
(test-is "char 0x01" (read "#\\") #\x01)
(test-is "char 0x02" (read "#\\") #\x02)
(test-is "char 0x03" (read "#\\") #\x03)
(test-is "char 0x04" (read "#\\") #\x04)
(test-is "char 0x05" (read "#\\") #\x05)
(test-is "char 0x06" (read "#\\") #\x06)
(test-is "char 0x07" (read "#\\") #\x07)
(test-is "char 0x08" (read "#\\") #\x08)
(test-is "char 0x09" (read "#\\	") #\x09)
(test-is "char 0x0a" (read "#\\
") #\x0a)
(test-is "char 0x0b" (read "#\\") #\x0b)
(test-is "char 0x0c" (read "#\\") #\x0c)

;; TODO There is some CR->LF translation going on in lower layers,
;; which even here ends the current line in the comment -- the ^M
;; below was a literal C-M originally
;; (test-is "char 0x0d" (read "#^M") #\x0d)

(test-is "char 0x0e" (read "#\\") #\x0e)
(test-is "char 0x0f" (read "#\\") #\x0f)
(test-is "char 0x10" (read "#\\") #\x10)
(test-is "char 0x11" (read "#\\") #\x11)
(test-is "char 0x12" (read "#\\") #\x12)
(test-is "char 0x13" (read "#\\") #\x13)
(test-is "char 0x14" (read "#\\") #\x14)
(test-is "char 0x15" (read "#\\") #\x15)
(test-is "char 0x16" (read "#\\") #\x16)
(test-is "char 0x17" (read "#\\") #\x17)
(test-is "char 0x18" (read "#\\") #\x18)
(test-is "char 0x19" (read "#\\") #\x19)
(test-is "char 0x1a" (read "#\\") #\x1a)
(test-is "char 0x1b" (read "#\\") #\x1b)
(test-is "char 0x1c" (read "#\\") #\x1c)
(test-is "char 0x1d" (read "#\\") #\x1d)
(test-is "char 0x1e" (read "#\\") #\x1e)
(test-is "char 0x1f" (read "#\\") #\x1f)
(test-is "char 0x20" (read "#\\ ") #\x20)
(test-is "char 0x21" (read "#\\!") #\x21)
(test-is "char 0x22" (read "#\\\"") #\x22)
(test-is "char 0x23" (read "#\\#") #\x23)
(test-is "char 0x24" (read "#\\$") #\x24)
(test-is "char 0x25" (read "#\\%") #\x25)
(test-is "char 0x26" (read "#\\&") #\x26)
(test-is "char 0x27" (read "#\\'") #\x27)
(test-is "char 0x28" (read "#\\(") #\x28)
(test-is "char 0x29" (read "#\\)") #\x29)
(test-is "char 0x2a" (read "#\\*") #\x2a)
(test-is "char 0x2b" (read "#\\+") #\x2b)
(test-is "char 0x2c" (read "#\\,") #\x2c)
(test-is "char 0x2d" (read "#\\-") #\x2d)
(test-is "char 0x2e" (read "#\\.") #\x2e)
(test-is "char 0x2f" (read "#\\/") #\x2f)
(test-is "char 0x30" (read "#\\0") #\x30)
(test-is "char 0x31" (read "#\\1") #\x31)
(test-is "char 0x32" (read "#\\2") #\x32)
(test-is "char 0x33" (read "#\\3") #\x33)
(test-is "char 0x34" (read "#\\4") #\x34)
(test-is "char 0x35" (read "#\\5") #\x35)
(test-is "char 0x36" (read "#\\6") #\x36)
(test-is "char 0x37" (read "#\\7") #\x37)
(test-is "char 0x38" (read "#\\8") #\x38)
(test-is "char 0x39" (read "#\\9") #\x39)
(test-is "char 0x3a" (read "#\\:") #\x3a)
(test-is "char 0x3b" (read "#\\;") #\x3b)
(test-is "char 0x3c" (read "#\\<") #\x3c)
(test-is "char 0x3d" (read "#\\=") #\x3d)
(test-is "char 0x3e" (read "#\\>") #\x3e)
(test-is "char 0x3f" (read "#\\?") #\x3f)
(test-is "char 0x40" (read "#\\@") #\x40)
(test-is "char 0x41" (read "#\\A") #\x41)
(test-is "char 0x42" (read "#\\B") #\x42)
(test-is "char 0x43" (read "#\\C") #\x43)
(test-is "char 0x44" (read "#\\D") #\x44)
(test-is "char 0x45" (read "#\\E") #\x45)
(test-is "char 0x46" (read "#\\F") #\x46)
(test-is "char 0x47" (read "#\\G") #\x47)
(test-is "char 0x48" (read "#\\H") #\x48)
(test-is "char 0x49" (read "#\\I") #\x49)
(test-is "char 0x4a" (read "#\\J") #\x4a)
(test-is "char 0x4b" (read "#\\K") #\x4b)
(test-is "char 0x4c" (read "#\\L") #\x4c)
(test-is "char 0x4d" (read "#\\M") #\x4d)
(test-is "char 0x4e" (read "#\\N") #\x4e)
(test-is "char 0x4f" (read "#\\O") #\x4f)
(test-is "char 0x50" (read "#\\P") #\x50)
(test-is "char 0x51" (read "#\\Q") #\x51)
(test-is "char 0x52" (read "#\\R") #\x52)
(test-is "char 0x53" (read "#\\S") #\x53)
(test-is "char 0x54" (read "#\\T") #\x54)
(test-is "char 0x55" (read "#\\U") #\x55)
(test-is "char 0x56" (read "#\\V") #\x56)
(test-is "char 0x57" (read "#\\W") #\x57)
(test-is "char 0x58" (read "#\\X") #\x58)
(test-is "char 0x59" (read "#\\Y") #\x59)
(test-is "char 0x5a" (read "#\\Z") #\x5a)
(test-is "char 0x5b" (read "#\\[") #\x5b)
(test-is "char 0x5c" (read "#\\\\") #\x5c)
(test-is "char 0x5d" (read "#\\]") #\x5d)
(test-is "char 0x5e" (read "#\\^") #\x5e)
(test-is "char 0x5f" (read "#\\_") #\x5f)
(test-is "char 0x60" (read "#\\`") #\x60)
(test-is "char 0x61" (read "#\\a") #\x61)
(test-is "char 0x62" (read "#\\b") #\x62)
(test-is "char 0x63" (read "#\\c") #\x63)
(test-is "char 0x64" (read "#\\d") #\x64)
(test-is "char 0x65" (read "#\\e") #\x65)
(test-is "char 0x66" (read "#\\f") #\x66)
(test-is "char 0x67" (read "#\\g") #\x67)
(test-is "char 0x68" (read "#\\h") #\x68)
(test-is "char 0x69" (read "#\\i") #\x69)
(test-is "char 0x6a" (read "#\\j") #\x6a)
(test-is "char 0x6b" (read "#\\k") #\x6b)
(test-is "char 0x6c" (read "#\\l") #\x6c)
(test-is "char 0x6d" (read "#\\m") #\x6d)
(test-is "char 0x6e" (read "#\\n") #\x6e)
(test-is "char 0x6f" (read "#\\o") #\x6f)
(test-is "char 0x70" (read "#\\p") #\x70)
(test-is "char 0x71" (read "#\\q") #\x71)
(test-is "char 0x72" (read "#\\r") #\x72)
(test-is "char 0x73" (read "#\\s") #\x73)
(test-is "char 0x74" (read "#\\t") #\x74)
(test-is "char 0x75" (read "#\\u") #\x75)
(test-is "char 0x76" (read "#\\v") #\x76)
(test-is "char 0x77" (read "#\\w") #\x77)
(test-is "char 0x78" (read "#\\x") #\x78)
(test-is "char 0x79" (read "#\\y") #\x79)
(test-is "char 0x7a" (read "#\\z") #\x7a)
(test-is "char 0x7b" (read "#\\{") #\x7b)
(test-is "char 0x7c" (read "#\\|") #\x7c)
(test-is "char 0x7d" (read "#\\}") #\x7d)
(test-is "char 0x7e" (read "#\\~") #\x7e)
(test-is "char 0x7f" (read "#\\") #\x7f)

(test-is "char 0xd6" (read "#\\Ö") #\xd6)
(test-is "char 0xdf" (read "#\\ß") #\xdf)
(test-is "char 0xbf" (read "#\\¿") #\xbf)
(test-is "char 0u2318" (read "#\\⌘") #\u2318)
(test-is "char 0u65e5" (read "#\\日") #\u65e5)

;;; TODO these won't work until we deal with real Unicode characters
;; (test-is "char 0U0001f4a9" (read "#\\💩") #\U0001f4a9)
;;
;; (test-err "char 0x7FFFFFFF" (read "#\\U7fffffff")
;;           #/code 0xFFFFFFFF is not a valid Unicode code point/)

(test-is "code-char 0" (code-char 65) #\A)
(test-is "code-char 1" (code-char #xd6) #\xd6)
(test-is "code-char 2" (code-char #xdf) #\xdf)
(test-is "code-char 3" (code-char #x2318) #\u2318)
(test-is "code-char 4" (code-char #x65e5) #\u65e5)
(test-is "code-char 5" (code-char #x1f4a9) #\U0001f4a9)
(test-err "code-char 6" (code-char #xffffffff)
         #/not an assigned Unicode character/)
(test-err "code-char 7" (code-char #xfffffff)
         #/not an assigned Unicode character/)


(test-is "char-int 0" (char-int #\A) 65)
(test-is "char-int 1" (char-int #\xd6) #xd6)
(test-is "char-int 2" (char-int #\xdf) #xdf)
(test-is "char-int 3" (char-int #\u2318) #x2318)
(test-is "char-int 4" (char-int #\u65e5) #x65e5)
;; TODO see above
;; (test-is "char-int 5" (char-int #\U0001f4a9) #x1f4a9)

(test-is "char-equal " (char-equal #\A #\a) t)
(test-is "char/= 1" (char/= #\d #\d) nil)
(test-is "char/= 2" (char/= #\d #\x) t)
(test-is "char/= 3" (char/= #\d #\D) t)
(test-is "char/= 4" (char/= #\d #\d #\d #\d) nil)
(test-is "char/= 5" (char/= #\d #\c #\d) nil)
(test-is "char/= 6" (char/= #\d #\d #\x #\d) nil)
(test-is "char/= 7" (char/= #\d #\y #\x #\c) t)
(test-is "char< 1" (char< #\a #\e #\e #\y) nil)
(test-is "char< 2" (char< #\a #\e #\y #\z) t)
(test-is "char< 3" (char< #\d #\d) nil)
(test-is "char< 4" (char< #\d #\x) t)
(test-is "char<= 1" (char<= #\a #\e #\e #\y) t)
(test-is "char<= 2" (char<= #\a #\e #\y #\z) t)
(test-is "char<= 3" (char<= #\d #\d) t)
(test-is "char<= 4" (char<= #\d #\x) t)
(test-is "char= 1" (char= #\d #\d) t)
(test-is "char= 2" (char= #\A #\a) nil)
(test-is "char= 3" (char= #\d #\x) nil)
(test-is "char= 4" (char= #\d #\D) nil)
(test-is "char= 5" (char= #\d #\d #\d #\d) t)
(test-is "char= 6" (char= #\d #\c #\d) nil)
(test-is "char= 7" (char= #\d #\d #\x #\d) nil)
(test-is "char= 8" (char= #\d #\y #\x #\c) nil)
(test-is "char> 1" (char> #\Z #\a) nil)
(test-is "char> 2" (char> #\d #\c #\b #\a) t)
(test-is "char> 3" (char> #\d #\d #\c #\a) nil)
(test-is "char> 4" (char> #\e #\d #\b #\c #\a) nil)
(test-is "char> 5" (char> #\e #\d) t)
(test-is "char> 6" (char> #\z #\A) t)
(test-is "char>= 1" (char>= #\d #\c #\b #\a) t)
(test-is "char>= 2" (char>= #\d #\d #\c #\a) t)
(test-is "char>= 3" (char>= #\e #\d #\b #\c #\a) nil)
(test-is "char>= 4" (char>= #\e #\d) t)

(test-is "char-not-equal 1" (char-not-equal #\d #\d) nil)
(test-is "char-not-equal 2" (char-not-equal #\d #\x) t)
(test-is "char-not-equal 3" (char-not-equal #\d #\D) nil)
(test-is "char-not-equal 4" (char-not-equal #\d #\d #\d #\d) nil)
(test-is "char-not-equal 5" (char-not-equal #\d #\c #\d) nil)
(test-is "char-not-equal 6" (char-not-equal #\d #\d #\x #\d) nil)
(test-is "char-not-equal 7" (char-not-equal #\d #\y #\x #\c) t)
(test-is "char-lessp 1" (char-lessp #\a #\e #\e #\y) nil)
(test-is "char-lessp 2" (char-lessp #\a #\e #\y #\z) t)
(test-is "char-lessp 3" (char-lessp #\d #\d) nil)
(test-is "char-lessp 4" (char-lessp #\d #\x) t)
(test-is "char-not-greaterp 1" (char-not-greaterp #\a #\e #\e #\y) t)
(test-is "char-not-greaterp 2" (char-not-greaterp #\a #\e #\y #\z) t)
(test-is "char-not-greaterp 3" (char-not-greaterp #\d #\d) t)
(test-is "char-not-greaterp 4" (char-not-greaterp #\d #\x) t)
(test-is "char-equal 1" (char-equal #\d #\d) t)
(test-is "char-equal 2" (char-equal #\A #\a) t)
(test-is "char-equal 3" (char-equal #\d #\x) nil)
(test-is "char-equal 4" (char-equal #\d #\D) t)
(test-is "char-equal 5" (char-equal #\d #\d #\d #\d) t)
(test-is "char-equal 6" (char-equal #\d #\c #\d) nil)
(test-is "char-equal 7" (char-equal #\d #\d #\x #\d) nil)
(test-is "char-equal 8" (char-equal #\d #\y #\x #\c) nil)
(test-is "char-greaterp 1" (char-greaterp #\Z #\a) t)
(test-is "char-greaterp 2" (char-greaterp #\d #\c #\b #\a) t)
(test-is "char-greaterp 3" (char-greaterp #\d #\d #\c #\a) nil)
(test-is "char-greaterp 4" (char-greaterp #\e #\d #\b #\c #\a) nil)
(test-is "char-greaterp 5" (char-greaterp #\e #\d) t)
(test-is "char-greaterp 6" (char-greaterp #\z #\A) t)
(test-is "char-not-lessp 1" (char-not-lessp #\d #\c #\b #\a) t)
(test-is "char-not-lessp 2" (char-not-lessp #\d #\d #\c #\a) t)
(test-is "char-not-lessp 3" (char-not-lessp #\e #\d #\b #\c #\a) nil)
(test-is "char-not-lessp 4" (char-not-lessp #\e #\d) t)


(done-testing)
