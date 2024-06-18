Hooks
=====

Some places lyk in the interpreter core provides "hooks", meaning in
these places a list of hook functions will be called that are
defined for this hook, meaning the hook is "activated". This is
modeled to be similar in spirit to the hook variables in Emacs.

A hook is identified by a hook symbol, set when the hook is defined.
A hook function can be added to a hook in Lisp using the builtin
function `add-hook-function`; the list of functions can be read
using `get-hook-functions`. When the function list of a hook is
empty, no function will be called when the hook is activated. Hooks
may already have functions set in the Lisp startup code (see the
`preload/` directory in the source distribution).

Hook functions can be passed arguments; the exact nature of the
arguments depend on the hook. It is an error if a hook function does
not accept the hook arguments. Both the number and nature of the
arguments as well as the significance of the return value are part
of the hook's documentation (see below).


Hooks provided in lyk
---------------------

*startup*: `*startup-hook*`

The interpreter startup code calls the hook functions with a list of
the load files specified by the `-l` command line option, the
expression to evaluate specified by the `-e` option as a list (so an
empty list means the `-e` option was not present) and a list of the
remaining command line arguments. So the hook function should be
like this:

    (lambda (load-files expr-list other-args)
      "Hook function for *startup-hook*."
      ...)


*repl-start*: `*repl-interactive-start-hook*`

Called when the interactive repl starts. No arguments are passed.

    (lambda ()
      "Hook function for *repl-interactive-start-hook*."
      ...)


*repl-input*: `*repl-interactive-input-hook*`

An interactive repl calls this hook function with the expression
entered by the user. (This is how repl short commands are
implemented.)

    (lambda (expr)
      "Hook function for *repl-interactive-input-hook*."
      ...)            ; return true iff expr was already handled


Lisp-defined Hooks
------------------

The hook mechanism is implemented in the lyk core. But hooks
can also be defined in Lisp. See the functions `define-hook` and
`run-hook-function` below.


Hook-related Functions
----------------------

The following functions can be used to handle hooks:

 * builtin function `(get-hooks)` => alist  
   Return an alist with (hooksym . function-list) pairs for all defined hooks.
   [builtin function defined in builtins/system.kt:1245:1]  

 * builtin function `(define-hook hook-symbol &optional function)` =>
   hook-symbol  
   Define a hook `hook-symbol` for later use.  
   [builtin function defined in builtins/system.kt:1267:1]  

 * builtin function `(add-hook-function hook-symbol function)` =>
   nil  
   Associate a `function` with the `hook-symbol`, to be called when
   the hook is activated. If `function` is nil, nothing will be
   called when the hook is activated.  
   [builtin function defined in builtins/system.kt:1289:1]

 * builtin function `(get-hook-functions hook-symbol)` => function  
   Return the list of functions of hook `hook-symbol` (may be empty).  
   [builtin function defined in builtins/system.kt:1314:1]

 * builtin function `(run-hook-functions hook-symbol &rest args)` =>
   return-value  
   Run the hook functions of hook `hook-symbol`.
   The `args` are passed to the functions.  
   [builtin function defined in builtins/system.kt:1333:1]

