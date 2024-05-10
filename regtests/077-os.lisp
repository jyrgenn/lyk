(require 'regtests)

;; time is represented as seconds in a number (i.e. aKotlin Double).

(defun check-sleep-seconds (time)
  (let* ((duration (measure-time (sleep time)))
         ;; the deviation is usually at about 5 ms, so this tolerance
         ;; should steer clear of that
         (tolerance 0.05)
         (deviation (abs (- duration time))))
    (flet ((printdev (dev comment)
             (format nil "time %s, deviation: %.4f s" comment deviation)))
      (cond ((< deviation (- tolerance))
             (printdev deviation "too short"))
            ((> deviation tolerance)
             (printdev deviation "too long"))
            (t (printdev deviation "ok"))))))

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
          #/FileNotFoundException/)

(test-is "file-author 1" (file-author "lyk.jar")
         (let ((user (getenv "USER")))
           (if (equal user "")
               (getenv "LOGNAME")
             user)))
(test-is "file-author 2" (file-author "/etc/passwd") "root")
(test-err "file-author 3" (file-author "sowathamwanich")
          #/NoSuchFileException/)

(defun write-bytes (fname n)
  (with-open-file (out fname :direction :output)
    (princ (make-string n "@") out)))

(defparameter fname-test-ouput "generated/testcase.out")

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
          #/NoSuchFileException/)

;; name pathspec result-of: file-namestring directory-namestring
(defparameter pathspecs
  #:(("abs-some-file" "/home/shnuggi/lib/boook" "boook" "/home/shnuggi/lib")
     ("abs-some-dir" "/home/shnuggi/lib/" "lib" "/home/shnuggi"
                     "/home/shnuggi/lib")

     ("rel-some-file" "home/shnuggi/lib/boook" "boook" "home/shnuggi/lib")
     ("rel-some-dir" "home/shnuggi/lib/" "lib" "home/shnuggi"
                     "home/shnuggi/lib")

     ("abs-two-file" "/home/shnuggi" "shnuggi" "/home")
     ("abs-two-dir" "/home/shnuggi/" "shnuggi" "/home" "/home/shnuggi")

     ("rel-two-file" "home/shnuggi" "shnuggi" "home")
     ("rel-two-dir" "home/shnuggi/" "shnuggi" "home" "home/shnuggi")

     ("abs-one-file" "/home" "home" "/")
     ("abs-one-dir" "/home/" "home" "/" "/home")

     ("rel-one-file" "home" "home" "")
     ("rel-one-dir" "home/" "home" "" "home")

     ("abs-zero" "/" "/" "/")

     ("rel-zero" "" "" "")))

(dolist (test (table-pairs pathspecs))
  (let (((name pathspec result-file result-dir result-name) test))
    (test-internal (string "file-namestring " name)
                   `(file-namestring ,pathspec) result-file 'cmp)
    (test-internal (string "directory-namestring " name)
                   `(directory-namestring ,pathspec) result-dir 'cmp)
    (test-internal (string "namestring " name)
                   `(namestring ,pathspec) (or result-name pathspec) 'cmp)))

(defun chomp (s)
  (regexp-replace "\n$" s ""))

(defun get-homedir (user)
  "Get home directory of USER; not for production use, calls a shell."
  (chomp (get-program-output (format nil "echo ~%s" user))))


(defvar curdir (get-working-directory))
(defvar curpar (let* ((parts (string-split (get-working-directory) "/"))
                      (par-parts (nreverse (cdr (nreverse parts)))))
                 (join par-parts "/")))
                 

(test-is "exp-f-n noexp 1" (expand-file-name ".~root/hullala/bala/bumbibum")
         (string curdir "/" ".~root/hullala/bala/bumbibum"))
(test-is "exp-f-n noexp 2" (expand-file-name "/~root/hullala/bala/bumbibum")
         "/~root/hullala/bala/bumbibum")
(test-is "exp-f-n noexp 3" (expand-file-name "hullala/bala/bumbibum" "/foo")
         "/foo/hullala/bala/bumbibum")
(test-is "exp-f-n noexp 5" (expand-file-name "~hebbtwinich/bala/bumbibum")
         (string curdir "/" "~hebbtwinich/bala/bumbibum"))

(test-is "exp-f-n exp me" (expand-file-name "~/hullala/bala/bumbibum")
         (string (get-homedir "") "/hullala/bala/bumbibum"))
(test-is "exp-f-n exp usr" (expand-file-name
                            (format nil "~%s/bala/bumbibum" (user-name)))
         (string (get-homedir (user-name)) "/bala/bumbibum"))
           
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
                ("/abc/" "/abc")
                ("/abc/." "/abc")
                ("/abc/.." "/")
                ("/abc/../" "/")
                ("/abc/../def" "/def")
                ("/abc/../def/" "/def")
                ("/abc/.../def" "/abc/.../def")
                ("/abc/.../def/" "/abc/.../def")
                ("/abc/..../def" "/abc/..../def")
                ("/abc/..../def/" "/abc/..../def")
                ("/abc/...../def" "/abc/...../def")
                ("/abc/...../def/" "/abc/...../def")
                ("/abc/./def/" "/abc/def")
                ("/abc//def" "/abc/def")
                ("/abc//def/" "/abc/def")
                ("/abc/def/" "/abc/def")
                ("/abc/def" "/abc/def")
                ("" curdir)
                ("." curdir)
                (".." curpar)
                ("./" curdir)
                ("../" curpar)
                ("../abc" (string curpar "/abc"))
                ("abc" (string curdir "/" "abc"))
                ("abc/" (string curdir "/" "abc"))
                ("abc/." (string curdir "/" "abc"))
                ("abc/.." curdir)
                ("abc/../" curdir)
                ("abc/../def" (string curdir "/def"))
                ("abc/../def/" (string curdir "/def"))
                ("abc/./def" (string curdir "/abc/def"))
                ("abc/./def/" (string curdir "/abc/def"))
                ("abc//def/" (string curdir "/abc/def"))
                ("abc//def" (string curdir "/abc/def"))
                ("abc/def/" (string curdir "/abc/def"))
                ("abc/def" (string curdir "/abc/def"))
                ))
  (let (((in out) case))
    (eval `(test-is ,(string "exp-fn " in) (expand-file-name ,in) ,out))))

(test-is "exp-fn long"
         (expand-file-name "/../etc//systemd/system/../posix/./numbers/")
         "/etc/systemd/posix/numbers")

(test-is "exp-fn 4/" (expand-file-name "//hummeln//im///arsch////und/bienen")
         "/hummeln/im/arsch/und/bienen")
(test-is "exp-fn /././."
         (expand-file-name "//hummeln//im///arsch/././.und/bienen")
         "/hummeln/im/arsch/.und/bienen")
(test-is "exp-fn /././. * 2"
         (expand-file-name "//hummeln//././.im///arsch/././.und/bienen")
         "/hummeln/.im/arsch/.und/bienen")

;; namestring

(test-is "normpath noexp 1" (namestring ".~root/hullala/bala/bumbibum")
         ".~root/hullala/bala/bumbibum")
(test-is "normpath noexp 2" (namestring "/~root/hullala/bala/bumbibum")
         "/~root/hullala/bala/bumbibum")
(test-is "normpath noexp 3" (namestring "hullala/bala/bumbibum")
         "hullala/bala/bumbibum")
(test-is "normpath noexp 5" (namestring "~hebbtwinich/bala/bumbibum")
         "~hebbtwinich/bala/bumbibum")

(test-is "normpath exp me" (namestring "~/hullala/bala/bumbibum")
         "~/hullala/bala/bumbibum")
(test-is "normpath exp usr" (namestring
                             (format nil "~%s/bala/bumbibum" (user-name)))
         (format nil "~%s/bala/bumbibum" (user-name)))
           
(test-is "normpath exp root" (namestring "~root/hullala/bala/bumbibum")
         "~root/hullala/bala/bumbibum")
(test-is "normpath .../../..." (namestring "/hullala/bala/../bumbibum")
         "/hullala/bumbibum")

(dolist (case '(("/" "/")
                ("//" "/")
                ("/." "/")
                ("/.." "/")
                ("/./" "/")
                ("/../" "/")
                ("/abc" "/abc")
                ("/abc/" "/abc")
                ("/abc/." "/abc")
                ("/abc/.." "/")
                ("/abc/../" "/")
                ("/abc/../def" "/def")
                ("/abc/../def/" "/def")
                ("/abc/.../def" "/abc/.../def")
                ("/abc/.../def/" "/abc/.../def")
                ("/abc/..../def" "/abc/..../def")
                ("/abc/..../def/" "/abc/..../def")
                ("/abc/...../def" "/abc/...../def")
                ("/abc/...../def/" "/abc/...../def")
                ("/abc/./def/" "/abc/def")
                ("/abc//def" "/abc/def")
                ("/abc//def/" "/abc/def")
                ("/abc/def/" "/abc/def")
                ("/abc/def" "/abc/def")
                ("" "")
                ("." "")
                (".." "..")
                ("./" "")
                ("../" "..")
                ("../abc" "../abc")
                ("abc" "abc")
                ("abc/" "abc")
                ("abc/." "abc")
                ("abc/.." "")
                ("abc/../" "")
                ("abc/../def" "def")
                ("abc/../def/" "def")
                ("abc/./def" "abc/def")
                ("abc/./def/" "abc/def")
                ("abc//def/" "abc/def")
                ("abc//def" "abc/def")
                ("abc/def/" "abc/def")
                ("abc/def" "abc/def")
                ))
  (let (((in out) case))
    (eval `(test-is ,(string "normpath " in) (namestring ,in) ,out))))

(test-is "normpath long"
         (namestring "/../etc//systemd/system/../posix/./numbers/")
         "/etc/systemd/posix/numbers")

(test-is "normpath 4/"
         (namestring "//hummeln//im///arsch////und/bienen")
         "/hummeln/im/arsch/und/bienen")
(test-is "normpath /././."
         (namestring "//hummeln//im///arsch/././.und/bienen")
         "/hummeln/im/arsch/.und/bienen")
(test-is "normpath /././. * 2"
         (namestring "//hummeln//././.im///arsch/././.und/bienen")
         "/hummeln/.im/arsch/.und/bienen")


;; run-program

(test-is "run-program 0" (get-program-output "echo" :capture-all t)
         '(0 "\n" ""))
(test-is "run-program 1" (get-program-output '("tr" "hxs" "rs!")
                                             :input "haxors"
                                             :capture-all t)
         '(0 "rasor!" ""))

(test-is "run-program 2" (cdr (get-program-output '("ls" "-l" "edofjerwofijn")
                                                  :capture-all t
                                                  :raise-error nil))
         '("" "ls: edofjerwofijn: No such file or directory\n"))
(test-err "run-program 2a" (get-program-output '("ls" "-l" "edofjerwofijn")
                                               :error-output nil)
          #/run-program: command exit status/)
(test-err "run-program 3" (get-program-output "false")
          #/run-program: command exit status \d/)

(test-is "run-program 4" 
         (let ((tmpf "generated/rp4"))
           (with-open-file (out tmpf :direction :output)
             (run-program '("tr" "s" "l") :input "sosososo" :output out))
           (with-open-file (in tmpf)
             (get-program-output '("tr" "l" "x") :input in)))
         "xoxoxoxo\n")

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
