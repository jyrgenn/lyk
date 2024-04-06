
Lyk — Lysp yn Kotlin
====================

This is going to be a Lisp in Kotlin. While I am not the biggest fan
of JVM-based things because you'll always have to schlep that huge
runtime around, I am curious. (I know there is the native option,
but its functionality feels limited. Maybe I'll consider it again
if the backend choice turns out to be a major performance issue --
so far, it doesn't look like that.)

So far Kotlin is remarkably similar to Swift, such that I could take
the Swift code from Lys, do a few replacements like s/func/fun/ and
things, and then let the compiler show me where it still needs work.
(Some more transformations could be automated, but for now I think
it isn't worth the effort.) This worked even with bigger and more
complex parts like the reader and eval, so I had a working REPL
fast.

Work in progress as of April 2024, currently working on getting the
regression test suite of lingo to work, an earlier implementation of
Lisp in Go. Bigger things ahead are fixing the macros (there is
still something wrong with them), some debugging help in eval,
integrating a readline editor, and many more builtin functions.
