(require 'regtests)

(let ((command "uname -a"))
  (format t "\nOutput of %v:\n" command)
  (shell command))

(done-testing)
