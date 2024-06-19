Lyk regression tests
====================

These are the regression tests I have imported from a previous
implementation of Lisp in Go. Due to a number of differences in the
implemented language (due to corrected behaviour that was wrong
previously, limitations and bugs in lyk that won't be resolved right
away, different decisions what to implement how, etc.), these
haven't stayed the same, but are adapted to lyk.

In some places there are tests commented out, with a TODO marker;
these are meant to be activated again once I have resolved the
associated bug or limitation in lyk.

The tests are not totally independent from each other. Here and
there one test depends on a previous having been run earlier in the
same process, or some other preparation. But these dependencies are
alway in the same file, never between different files. If that is
the case somewhere, it is not intentional, but a bug that must be
fixed.

The tests are run using the `run-tests.lisp` script in the base
directory, using the code in `l/regtests.lisp`. The `test-*` macros
used in the tests are defined there. The rest of the code is a real
mess, and I would probably do it differently today. But it is also
older code (2016/17).

I have no idea how good the test coverage acutually is. But the idea
is to have relatively comprehensive tests for at least each of the
builtin funcions. 
