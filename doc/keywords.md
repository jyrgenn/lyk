;;; lyk doc

Keyword arguments to functions
==============================

Builtins
--------

Keyword arguments to builtin functions are declared in the Builtin
description comment with "key" like this:

/// key     "if-does-not-exist" to Symbol.intern("error"), "direction" to Symbol.intern(":input"), ":if-exists" to Symbol.intern(":overwrite")

This is, sadly, a bit over-verbose and boilerplatey, but the easiest
to transform to the actual Builtin(...) call. Maybe there will be a
more sophisticated mechanis later that makes this easier. For now,
I'll live with it.

Major point is, the keywords do have the leading colon here. Also,
they are used with the colon when getting the value from the kwArgs
parameter:

    val direction = kwArgs[intern(":direction")] ?: Nil

This is different with the Lambdas.


Lambdas
-------

Keyword arguments to lambda functions are declared in the lambda
list after the '&key' without colon. They are given with a colon
when calling the function, but available as local variables in the
Lambda without.
