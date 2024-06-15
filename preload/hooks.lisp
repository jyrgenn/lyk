;;; more hook things

(defvar lyk-rc-file (expand-file-name "~/.lykrc")
  "User lyk startup file")

(defun load-lyk-rc-file (&rest ignored)
  "Load lyk-rc-file, if it exists."
  (unless (cdr (assoc 'no-user-startup (lyk-command-options)))
    (errset (load lyk-rc-file :error nil) nil)))

(set-hook-function '*startup-hook*
                   #'load-lyk-rc-file)
