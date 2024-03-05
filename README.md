
Lyk — Lysp yn Kotlin
====================

This is going to be a Lisp in Kotlin. While I am not the biggest fan
of JVM-based things because you'll always have to schlep that
runtime around (I know there is the native option, but it doesn't
have everything AFAICS), I am curious.

So far Kotlin is remarkably similar to Swift, such that I can take
the Swift code from Lys, do a few replacements like s/func/fun/ and
things, and then let the compiler show me where it still needs work.
(Some other transformations could be automated, but for now I think
it isn't worth the effort.) This worked even with bigger parts like
the reader and eval, so I had a working REPL fast.

Work in progress as of March 2024, currently mainly on importing
more builtins from Lys. Bigger things ahead are macros, place-based
setters, and some debugging help in eval.
