(require 'regtests)

(let ((command "uname -a"))
  (format t "\nOutput of %s:\n" command)
  (shell command))

(done-testing)
