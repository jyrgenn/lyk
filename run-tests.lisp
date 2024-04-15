#!/usr/bin/env -S java -jar lyk.jar -W # -d repl,eval

(load "l/regtests.lisp")
(exit (run-tests))

