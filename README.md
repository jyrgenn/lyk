Lyk -- Lysp yn Kotlin
=====================

This is going to be a Lisp in Kotlin. While I am not the biggest fan
of JVM-based things because you'll always have to schlep that huge
runtime around, I am curious. (I know there is the native option,
but its functionality feels limited. Maybe I'll consider it again
if the backend choice turns out to be a major performance issue --
so far, it doesn't look like that.)

Kotlin feels remarkably similar to Swift, such that I could take the
Swift code from Lys, do a few replacements like s/func/fun/ and
things, and then let the compiler show me where it still needed
work. (It would have been possible to automate some more
transformations, but it didn't seem worth the effort.) This worked
even with bigger and more complex parts like the reader and eval, so
I had a working REPL fast.

Work in progress as of Mai 2024, finally got through the regression
test suite that I originally made for an earlier implementation.
Macros seems to work mostly well now that I have followed Steele's
advice about how they should be dealt with. The RPN calculator
`l/lyc` seems to mostly work now -- it is, admittedly, the only
application that I really use a lot, so that *is* significant.

Bigger things ahead are some debugging help in eval, a non-recursive
eval with an explicit call stack, and of course many more builtin
functions.

Lyk uses the JLine line editor Version 2 for the interactive input
line; see the subdirectory `external/jline/`. This is not the
current release version, but it was rather easy to integrate, which
the current one wasn't.

Please see the files in the `doc/` subdirectory for more
information, in particular `doc/lyk.md`. It contains a few pointers
how to explore lyk.

-- ni@w21.org 2024-05
