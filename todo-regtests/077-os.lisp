(require 'regtests)

(test-is "internal time units" internal-time-units-per-second
         1e9)

(defun check-sleep-seconds (time)
  (let* ((first (get-internal-real-time))
         (blah (sleep time))
         (second (get-internal-real-time))
         ;; the deviation is usually at about 5 ms, so this tolerance
         ;; should steer clear of that
         (tolerance (* 0.05 internal-time-units-per-second))
         (deviation (- second first (* time internal-time-units-per-second)))
         (printdev (lambda (dev cmnt)
                     (format nil "time %s, deviation: %.4f s"
                             cmnt
                             (/ deviation internal-time-units-per-second)))))
    (cond ((< deviation (- tolerance))
           (printdev deviation "too short"))
          ((> deviation tolerance)
           (printdev deviation "too long"))
           (t (printdev deviation "ok")))))

(test-match "real time 0.1 s" (check-sleep-seconds 0.1) #/ ok,/)
(test-match "real time 0.2 s" (check-sleep-seconds 0.2) #/ ok,/)
(test-match "real time 0.5 s" (check-sleep-seconds 0.5) #/ ok,/)

(test-err "delete-file" (let ((fname "generated/test-del-out")
                              (content "this is the content\n"))
                          ;; create file
                          (with-open-file (out fname :direction :output)
                            (princ content out))
                          ;; check file
                          (with-open-file (in fname)
                            (let ((line (read-line in)))
                              (unless (eq line content)
                                (error "file doesn't have expected content"))))
                          ;; delete file
                          (delete-file fname)
                          ;; check again
                          (open fname))
          #/no such file or directory/)

(test-is "file-author 1" (file-author "lingo")
         (let ((user (getenv "USER")))
           (if (equal user "")
               (getenv "LOGNAME")
             user)))
(test-is "file-author 2" (file-author "/etc/passwd") "root")
(test-err "file-author 3" (file-author "sowathamwanich")
          #/no such file or directory/)

(defun write-bytes (fname n)
  (with-open-file (out fname :direction :output)
    (princ (make-string n "@") out)))

(defparameter fname-test-ouput "gen/testcase.out")

(test-is "file-length 0" (let ((fname fname-test-ouput))
                           (write-bytes fname 0)
                           (file-length fname))
         0)
(test-is "file-length 1234" (let ((fname fname-test-ouput))
                              (write-bytes fname 1234)
                              (file-length fname))
         1234)
(test-is "file-length 123456" (let ((fname fname-test-ouput))
                                (write-bytes fname 123456)
                                (file-length fname))
         123456)
(test-err "file-length none" (file-length "sowathamwanich")
          #/no such file or directory/)

;; name pathspec result-of-file-namestring result-of-directory-namestring
(defparameter pathspecs
  #:(("abs-some-file" "/home/shnuggi/lib/boook" "boook" "/home/shnuggi/lib")
     ("abs-some-dir" "/home/shnuggi/lib/" "" "/home/shnuggi/lib")

     ("rel-some-file" "home/shnuggi/lib/boook" "boook" "home/shnuggi/lib")
     ("rel-some-dir" "home/shnuggi/lib/" "" "home/shnuggi/lib")

     ("abs-two-file" "/home/shnuggi" "shnuggi" "/home")
     ("abs-two-dir" "/home/shnuggi/" "" "/home/shnuggi")

     ("rel-two-file" "home/shnuggi" "shnuggi" "home")
     ("rel-two-dir" "home/shnuggi/" "" "home/shnuggi")

     ("abs-one-file" "/home" "home" "/")
     ("abs-one-dir" "/home/" "" "/home")

     ("rel-one-file" "home" "home" "")
     ("rel-one-dir" "home/" "" "home")

     ("abs-zero" "/" "" "/")

     ("rel-zero" "" "" "")))

(dolist (test (table-pairs pathspecs))
  (let (((name pathspec result-file result-dir) test))
    (test-internal (string "file-namestring " name)
                   `(file-namestring ,pathspec) result-file 'cmp)
    (test-internal (string "directory-namestring " name)
                   `(directory-namestring ,pathspec) result-dir 'cmp)
    (test-internal (string "namestring " name)
                   `(namestring ,pathspec) pathspec 'cmp)))

(defun chomp (s)
  (regex-replace "\n$" s ""))

(defun get-homedir (user)
  "Get home directory of USER; not for production use, calls a shell."
  (chomp (car (run-process "sh" "-c" (format nil "echo ~%s" user)))))

(defun get-username ()
  "Get name of current user; not for production use, calls `whoami`."
  (chomp (car (run-process "whoami"))))

(test-is "exp-f-n noexp 1" (expand-file-name ".~root/hullala/bala/bumbibum")
         ".~root/hullala/bala/bumbibum")
(test-is "exp-f-n noexp 2" (expand-file-name "/~root/hullala/bala/bumbibum")
         "/~root/hullala/bala/bumbibum")
(test-is "exp-f-n noexp 3" (expand-file-name "hullala/bala/bumbibum")
         "hullala/bala/bumbibum")
(test-is "exp-f-n noexp 4" (expand-file-name '(a c f r))
         "(a c f r)")
(test-is "exp-f-n noexp 4a" (stringp (expand-file-name '(a c f r)))
         t)
(test-is "exp-f-n noexp 5" (expand-file-name "~hebbtwinich/bala/bumbibum")
         "~hebbtwinich/bala/bumbibum")

(test-is "exp-f-n exp me" (expand-file-name "~/hullala/bala/bumbibum")
         (string (get-homedir "") "/hullala/bala/bumbibum"))
(test-is "exp-f-n exp usr" (expand-file-name
                            (format nil "~%s/bala/bumbibum" (get-username)))
         (string (get-homedir (get-username)) "/bala/bumbibum"))
           
(test-is "exp-f-n exp root" (expand-file-name "~root/hullala/bala/bumbibum")
         (string (get-homedir "root") "/hullala/bala/bumbibum"))
(test-is "exp-f-n .../../..." (expand-file-name "/hullala/bala/../bumbibum")
         "/hullala/bumbibum")

(dolist (case '(("/" "/")
                ("//" "/")
                ("/." "/")
                ("/.." "/")
                ("/./" "/")
                ("/../" "/")
                ("/abc" "/abc")
                ("/abc/" "/abc/")
                ("/abc/." "/abc")
                ("/abc/.." "/")
                ("/abc/../" "/")
                ("/abc/../def" "/def")
                ("/abc/../def/" "/def/")
                ("/abc/.../def" "/abc/.../def")
                ("/abc/.../def/" "/abc/.../def/")
                ("/abc/..../def" "/abc/..../def")
                ("/abc/..../def/" "/abc/..../def/")
                ("/abc/...../def" "/abc/...../def")
                ("/abc/...../def/" "/abc/...../def/")
                ("/abc/./def/" "/abc/def/")
                ("/abc//def" "/abc/def")
                ("/abc//def/" "/abc/def/")
                ("/abc/def/" "/abc/def/")
                ("/abc/def" "/abc/def")
                ("" "")
                ("." ".")
                (".." "..")
                ("./" "./")
                ("../" "../")
                ("../abc" "../abc")
                ("abc" "abc")
                ("abc/" "abc/")
                ("abc/." "abc")
                ("abc/.." "")
                ("abc/../" "")
                ("abc/../def" "def")
                ("abc/../def/" "def/")
                ("abc/./def" "abc/def")
                ("abc/./def/" "abc/def/")
                ("abc//def/" "abc/def/")
                ("abc//def" "abc/def")
                ("abc/def/" "abc/def/")
                ("abc/def" "abc/def")
                ))
  (let (((in out) case))
    (eval `(test-is ,(string "exp-fn " in) (expand-file-name ,in) ,out))))

(test-is "exp-fn long"
         (expand-file-name "/../etc//systemd/system/../posix/./numbers/")
         "/etc/systemd/posix/numbers/")

(test-is "exp-fn 4/" (expand-file-name "//hummeln//im///arsch////und/bienen")
         "/hummeln/im/arsch/und/bienen")
(test-is "exp-fn /././."
         (expand-file-name "//hummeln//im///arsch/././.und/bienen")
         "/hummeln/im/arsch/.und/bienen")
(test-is "exp-fn /././. * 2"
         (expand-file-name "//hummeln//././.im///arsch/././.und/bienen")
         "/hummeln/.im/arsch/.und/bienen")

;; normalize-pathname

(test-is "normpath noexp 1" (normalize-pathname ".~root/hullala/bala/bumbibum")
         ".~root/hullala/bala/bumbibum")
(test-is "normpath noexp 2" (normalize-pathname "/~root/hullala/bala/bumbibum")
         "/~root/hullala/bala/bumbibum")
(test-is "normpath noexp 3" (normalize-pathname "hullala/bala/bumbibum")
         "hullala/bala/bumbibum")
(test-is "normpath noexp 4" (normalize-pathname '(a c f r))
         "(a c f r)")
(test-is "normpath noexp 4a" (stringp (normalize-pathname '(a c f r)))
         t)
(test-is "normpath noexp 5" (normalize-pathname "~hebbtwinich/bala/bumbibum")
         "~hebbtwinich/bala/bumbibum")

(test-is "normpath exp me" (normalize-pathname "~/hullala/bala/bumbibum")
         "~/hullala/bala/bumbibum")
(test-is "normpath exp usr" (normalize-pathname
                             (format nil "~%s/bala/bumbibum" (get-username)))
         (format nil "~%s/bala/bumbibum" (get-username)))
           
(test-is "normpath exp root" (normalize-pathname "~root/hullala/bala/bumbibum")
         "~root/hullala/bala/bumbibum")
(test-is "normpath .../../..." (normalize-pathname "/hullala/bala/../bumbibum")
         "/hullala/bumbibum")

(dolist (case '(("/" "/")
                ("//" "/")
                ("/." "/")
                ("/.." "/")
                ("/./" "/")
                ("/../" "/")
                ("/abc" "/abc")
                ("/abc/" "/abc/")
                ("/abc/." "/abc")
                ("/abc/.." "/")
                ("/abc/../" "/")
                ("/abc/../def" "/def")
                ("/abc/../def/" "/def/")
                ("/abc/.../def" "/abc/.../def")
                ("/abc/.../def/" "/abc/.../def/")
                ("/abc/..../def" "/abc/..../def")
                ("/abc/..../def/" "/abc/..../def/")
                ("/abc/...../def" "/abc/...../def")
                ("/abc/...../def/" "/abc/...../def/")
                ("/abc/./def/" "/abc/def/")
                ("/abc//def" "/abc/def")
                ("/abc//def/" "/abc/def/")
                ("/abc/def/" "/abc/def/")
                ("/abc/def" "/abc/def")
                ("" "")
                ("." ".")
                (".." "..")
                ("./" "./")
                ("../" "../")
                ("../abc" "../abc")
                ("abc" "abc")
                ("abc/" "abc/")
                ("abc/." "abc")
                ("abc/.." "")
                ("abc/../" "")
                ("abc/../def" "def")
                ("abc/../def/" "def/")
                ("abc/./def" "abc/def")
                ("abc/./def/" "abc/def/")
                ("abc//def/" "abc/def/")
                ("abc//def" "abc/def")
                ("abc/def/" "abc/def/")
                ("abc/def" "abc/def")
                ))
  (let (((in out) case))
    (eval `(test-is ,(string "normpath " in) (normalize-pathname ,in) ,out))))

(test-is "normpath long"
         (normalize-pathname "/../etc//systemd/system/../posix/./numbers/")
         "/etc/systemd/posix/numbers/")

(test-is "normpath 4/"
         (normalize-pathname "//hummeln//im///arsch////und/bienen")
         "/hummeln/im/arsch/und/bienen")
(test-is "normpath /././."
         (normalize-pathname "//hummeln//im///arsch/././.und/bienen")
         "/hummeln/im/arsch/.und/bienen")
(test-is "normpath /././. * 2"
         (normalize-pathname "//hummeln//././.im///arsch/././.und/bienen")
         "/hummeln/.im/arsch/.und/bienen")


;; run-process

(test-is "run-process 0" (run-process "echo") '("\n" ""))
(test-is "run-process 1" (run-process "tr" "hxs" "rs!" :input "haxors")
         '("rasor!" ""))

(terpri)
(test-is "run-process 2" (run-process "ls" "-l" "lingo" :output nil :error nil)
         '(nil nil))
(test-err "run-process 3" (run-process "false") #/run-process: exit status \d/)

(test-is "run-process 4" 
         (let ((tmpf "gen/rp4"))
           (with-open-file (out tmpf :direction :output)
             (run-process "tr" "s" "l" :input "sosososo" :output out))
           (with-open-file (in tmpf)
             (car (run-process "tr" "l" "x" :input in))))
         "xoxoxoxo")

;; check if the conditional expansion of the bodyforms of
;; with-open-file is correct, depending on the number of bodyforms

(test-is "with-open-file exp 0"
         (macroexpand '(with-open-file (p "/etc/passwd")))
         '(let ((p (open "/etc/passwd")))
            (unwind-protect
                (progn)
              (close p))))

(test-is "with-open-file exp 1"
         (macroexpand '(with-open-file (p "/etc/passwd")
                         (let ((a 1)(b 2))
                           (cons a b))))
         '(let ((p (open "/etc/passwd")))
            (unwind-protect
                (let ((a 1) (b 2))
                  (cons a b))
              (close p))))

(test-is "with-open-file exp 2"
         (macroexpand '(with-open-file (p "/etc/passwd")
                         (print "there we are")
                         (let ((a 1)(b 2))
                           (cons a b))))
         '(let ((p (open "/etc/passwd")))
            (unwind-protect
                (progn
                  (print "there we are")
                  (let ((a 1) (b 2))
                    (cons a b)))
              (close p))))

(test-is "with-open-file exp 3"
         (macroexpand '(with-open-file (p "/etc/passwd")
                         (print "there we are")
                         (let ((a 1)(b 2))
                           (cons a b))
                         (format nil "and there we were")))
         '(let ((p (open "/etc/passwd")))
            (unwind-protect
                (progn
                  (print "there we are")
                  (let ((a 1) (b 2))
                    (cons a b))
                  (format nil "and there we were"))
              (close p))))

(done-testing)
