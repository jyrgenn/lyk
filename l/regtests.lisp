#!./lingo

(provide 'regtests)

(defvar testdir "regtests" "directory containing the test cases")
(defvar protocol-file-name "testrun.log")
(defvar out nil "test log file")
(defvar devnull (open "/dev/null" :direction :output) "The null device")
(defvar fails nil "list of failed test cases")
(defvar warnings 0 "number of warnings issued")
(defvar ntests 0 "number of tests done")
(defvar verbose nil)


(defvar test-name-table #:()
  "Ensure that all tests have a unique name")

(defun presult (format-string &rest args)
  (when verbose
    (apply #'format t format-string args))
  (apply #'format out format-string args))

(defmacro test (name expr)
  "Run test NAME; success if EXPR evaluates to a true value"
  `(test-internal ,name ',expr t 'true))

(defmacro test-is (name expr expect)
  "Run test NAME; success if EXPR evaluates to EXPECT"
  `(test-internal ,name ',expr ,expect 'cmp))

(defmacro test-match (name expr expect-re)
  "Run test NAME; success if EXPR evaluates to something matching EXPECT-RE"
  `(test-internal ,name ',expr ,expect-re 'match))

(defmacro test-not (name expr)
  "Run test NAME; success if EXPR evaluates to false"
  `(test-internal ,name ',expr nil 'false))

(defmacro test-num (name expr expect)
  "Run test NAME; success if EXPR evaluates to a number close to EXPECT"
  `(test-internal ,name ',expr ,expect 'num))

(defmacro test-form (name form expect)
  "Run test NAME; success if calling FORM results in EXPECT"
  `(test-internal ,name ,form ,expect 'cmp))

(defmacro test-err (name expr expect-re)
  "Run test NAME; success if evaluating EXPR raises an error matching EXPECT-RE"
  `(test-internal ,name ',expr ,expect-re 'error))

(defun test-internal (name expr expect type)
  "Run the test called NAME and print the result.
The test is considered successful if the printed representations of
the evaluation of EXPR and EXPECT are equal.
TYPE can be 'true, 'cmp, 'false, 'match, 'num, or 'error."
  (let ((already-have-fname? (table-get test-name-table name)))
    (if already-have-fname?
        (progn (format t "WARN name %s:%s already seen in %s\n"
                       *load-pathname* name already-have-fname?)
               (incf warnings))
      (table-put test-name-table name *load-pathname*)))
  (let* ((result (errset (if (functionp expr)
                             (expr)     ;allow closures
                           (eval expr))
                         nil))
         (pass (lambda ()
                 (presult "pass: %-23s »%s«\n" name result)))
         (fail (lambda (&rest args)
                 (unless args
                   (setf args
                         (list
                          "FAIL: »%s«\n calculated: »%s«\n   expected: »%s«\n"
                          name (princs result) (princs expect))))
                 (apply #'presult args)
                 (push (cons *load-pathname* name) fails))))
    (incf ntests)
    (if (atom result)
        (progn (setf result *last-error*)
               (if (eq type 'error)
                   (if (regexpp expect)
                       (if (regexp-match expect result)
                           (pass)
                         (fail))
                     (fail "FAIL: »%s« wrong, not a regexp: %s\n"
                           name expect))
                 (fail "FAIL: »%s« RAISED ERROR: %s\n" name *last-error*)))
      ;; no error
      (setf result (car result))
      (cond ((eq type 'true) (if result (pass) (fail)))
            ((eq type 'false) (if result (fail) (pass)))
            ((eq type 'cmp)
             (setf result (princs result))
             (setf expect (princs expect))
             (if (eq result expect) (pass) (fail)))
            ((eq type 'match)
             (setf result (princs result))
             (if (regexp-match expect result) (pass) (fail)))
            ((eq type 'num)
             (setf result (princs (round-deep result)))
             (setf expect (princs (round-deep expect)))
             (if (= result expect) (pass) (fail)))
            ((eq type 'error) (fail "FAIL: »%s« NO ERROR\n" name))
            (t (error "invalid test type %s" type))))))

(test-internal 'test-test ''lala "lala" 'cmp) ;check if the *testing* works
(decf ntests)                           ;but don't count this one

(defmacro roundto5 (n)
  "Round to 5 places."
  `(let ((factor 1e5))
     (/ (round (* ,n factor)) factor)))

(defun round-deep (ob)
  "Round all numbers in the object OB for numeric comparison.
Traverse conses and vectors to find numbers."
  (cond ((numberp ob) (roundto5 ob))
        ((consp ob) (cons (round-deep (car ob))
                          (round-deep (cdr ob))))
        ((vectorp ob) (dotimes (i (length ob) ob)
                        (vector-set ob i (round-deep (vector-get ob i)))))
        (t ob)))

(defvar testing-done nil
  "A test file is not completed until this is true.")

(defun done-testing ()
  "Mark a test file as done."
  (presult "testing done in %s\n" *load-pathname*)
  (setf testing-done t))

;;;;;;;;;;;;;;;;;;; now let the games begin

(defun run-tests ()
  (with-open-file (log protocol-file-name :direction :output)
    (setf out log)
    (run-tests-with-out)))

(defun run-tests-with-out ()
  (unless *warnings-as-errors*
    (error "-W option not set; tests needs *warnings-as-errors*!"))
  (setf fails nil)
  (setf warnings 0)
  (when (eq (car *command-line-args*) "-v")
    (pop *command-line-args*)
    (setf verbose t))

  (let* ((allfiles (directory (string testdir "/" "[0-9]*.lisp")))
         (files (or *command-line-args* allfiles)))
    (when verbose
      (format t "load files: %s\n" files))
    (dolist (f files)
      (let ((number (car (regexp-match #/^[0-9]*$/ f))))
        ;; if we have just a number (as a command-line arg), we still
        ;; find and load the file from the regtests directory
        (when number
          (let ((fname (car (directory (string testdir "/"
                                                  number "*.lisp")))))
            (when fname
              (setf f fname)))))
      (presult "\nloading %s\n" f)
      (setf testing-done nil)
      (unless verbose
        (let ((number (cadr (regexp-match #r{/([0-9]+)} f))))
          (format t " %s " number)))
      (let ((result (errset (load f))))
       (if (atom result)
            (progn (presult "load FAIL: %s %s\n" f *last-error*)
                   (push (cons f "load") fails))
          (unless testing-done
            (let ((message " not completed, done-testing not called\n"))
              (presult "file FAIL: %s%s" f message)
              (push (cons f message) fails)))))))

  (let ((nfails (length fails)))
    (format t "\n%d tests, %d FAILS, %d warning%s\n"
            ntests nfails warnings (plural-s warnings))
    (if (zerop nfails)
        (progn (print "tests ALL PASSED")
               (when (not (zerop warnings))
                 (print "(but non-zero warnings)"))
               (terpri))
      (dolist (err (reverse fails))
        (let ((file (car err))
              (name (cdr err)))
          (format t "   %s: %s\n" file name)))))
  (terpri)
  (format t "test output written to %s\n" protocol-file-name)
  
  ;; return non-zero if there were test fails or warnings
  (+ (length fails) warnings))

;; EOF
