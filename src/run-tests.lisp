#!/usr/bin/env -S ./scripts/lyk -J . # -d call

(setf *load-path* (list "l"))
(require 'regtests)
(exit (run-tests))

