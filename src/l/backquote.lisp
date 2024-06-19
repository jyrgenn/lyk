(provide 'backquote)

(defun backquote (command)
  "Run command and capture standard output and standard error output.
Return a list of the exit status, standard output as a string, and standard
error output as a string."
  (let ((capture-stdout (make-string-output-stream))
        (capture-stderr (make-string-output-stream)))
    (list (run-program command
                       :output capture-stdout
                       :error-output capture-stderr)
          (get-output-stream-string capture-stdout)
          (get-output-stream-string capture-stderr))))

    
