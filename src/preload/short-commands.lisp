;;; short commands for the repl in table *repl-short-commands*
;;; entries are  :name => func

(defvar *repl-short-commands* (make-table)
  "The table (keyword => function) of short commands for the REPL.")


(defun maybe-run-short-command (expr)
  "Run a short command if `expr` is a keyword matching one.
Return true iff a short command was run or at least attempted."
  (unless (keywordp expr)
    (return nil))
  (unless (tablep *repl-short-commands*)
    (warning "*repl-short-commands* is not a table: ~A" *repl-short-commands*)
    (return nil))
  (let ((cmd (select-string-from-prefix expr
                                        (table-keys *repl-short-commands*))))
    (cond ((null cmd)
           (return nil))
          ((listp cmd)
           (warning "`~A` matches multiple short commands: ~A"
		    expr (join cmd ", "))
	   (warning "type :help for more information")
           (return t))
          (t (funcall (table-get *repl-short-commands* cmd))
             (setf + *the-non-printing-object*) ;; replace the repl's
                                                ;; expression to
                                                ;; suppress further
                                                ;; output
             ))))


(add-hook-function '*repl-interactive-input-hook*
                   #'maybe-run-short-command)


(defmacro define-repl-short-command (name docstring &rest bodyforms)
  "Define short command `name` for the repl with `docstring` and `bodyforms`.
`name` must be a :keyword."
  (unless (keywordp name)
    (error "short command name `~A` is not a keyword" name))
  `(table-put *repl-short-commands* ,name
              (lambda () ,docstring
                ,@bodyforms)))


(define-repl-short-command :replsyms
    "print the repl's special symbols and their values"
  (dolist (sym '(* ** *** + ++ +++ / // ///))
    (format t " ~3@A: ~A~%" sym (symbol-value sym))))

(define-repl-short-command :help
    "show help on short commands"
  (println "Defined short commands:")
  (dolist (name (sort (table-keys *repl-short-commands*)))
    (let ((func (table-get *repl-short-commands* name)))
      (when (functionp func)
        (format t "~17@A : ~A~%" name (function-docstring func)))))
  (terpri)
  (println
   "A unique prefix is sufficient to call a short command.
See macro `define-repl-short-command` on how to define short commands."))


(define-repl-short-command :explore
    "show hints for exploring the system"
  (format t "There are a few tools to help explore lyk:

  - `apropos` prints all known symbols that match a substring or a
    regular expression, together with the information if the symbol
    is bound to a function (builtin, lambda, or macro), has a
    variable binding, or properties.

        (apropos \"substr\")
	(apropos #/r.*exp/)

  - `doc` prints the function documentation for a symbol (or a
    function object) with synopsis, description, and place of
    definition.

        (doc 'apropos)

  - `describe` return an alist with an object's attributes.

        (describe 'doc)

  - The directory `~A/doc/` contains not a full
    documentation, but a few chapters describing some aspects of lyk.

  - `~A/doc/DOCSTRINGS.md` contains the docstrings
    of all functions, same as those printed interactively with `doc`.

And then, there is the source code, of course.~%"
          *lyk-install-directory* *lyk-install-directory*))

(define-repl-short-command :perfdata
    "show status counters since the start of the system"
  (let ((perfdata ))
    (format t ";")
    (dolist (item (system-perfdata))
      (format t " ~A ~A" (cdr item) (car item)))
    (terpri)))

(define-repl-short-command :exit "exit the system" (exit))

(defun print-file (pathname &optional (stream *standard-output*))
  "Print the contents of file `pathname` to `stream`."
  (with-lines-from-file (line pathname)
    (princ line stream)))

(define-repl-short-command :license "show the lyk license"
  (print-file (string *lyk-install-directory*
                      "/" "LICENSE")))

(define-repl-short-command :jline-license "show the jline license"
  (print-file (string *lyk-install-directory*
                      "/jline/LICENSE")))

;;; EOF
