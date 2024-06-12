Hooks
=====

Some places lyk in the interpreter core provides "hooks", meaning
in these places a hook function will be called if one is defined
for this hook, meaning the hook is "activated". This is modeled
to be similar in spirit to the hook variables in Emacs.

A hook is identified by a hook symbol, set when the hook
is defined. A hook function can be set in Lisp using
the builtin function `set-hook-function` and read using
`get-hook-function`. When the function of a hook is set to nil,
no function will be called when the hook is activated. Hooks
may already have functions set in the Lisp startup code (see the
`preload/` directory in the source distribution).

Hook functions can be passed arguments; the exact nature of
the arguments depend on the hook. It is an error if the hook
function does not accept the hook arguments. The return value of
the hook function can be significant for a hook.  Both the number
and nature of the arguments as well as the significance of the
return value are part of the hook's documentation (see below).


Hooks provided in lyk
---------------------

*startup*: `*startup-hook*`

The interpreter startup code calls the hook function with a list
of the load files specified by the `-l` command line option,
the expression to evaluate specified by the `-e` option as a
list (so an empty list means the `-e` option was not present)
and a list of the remaining command line arguments. So the hook
function should be like this:

    (lambda (load-files expr-list other-args)
      "Hook function for *startup-hook*."
      ...)

The return value of the hook function is ignored.


*repl-start*: `*repl-interactive-start-hook*`

Called when the interactive repl starts. No arguments are passed;
the return value is ignored.

    (lambda ()
      "Hook function for *repl-interactive-start-hook*."
      ...)


*repl-input*: `*repl-interactive-input-hook*`

An interactive repl calls this hook function with the expression
entered by the user. If the return value of the hook function
is true, the repl considers the input already handled, meaning
it starts anew at the top and does not evaluate the expression
entered.

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
   Return an alist with (hooksym . function) pairs. The function may be nil.  
   [builtin function defined in builtins/system.kt:1245:1]  

 * builtin function `(define-hook hook-symbol &optional function)` =>
   hook-symbol  
   Define a hook `hook-symbol` for later use.  
   [builtin function defined in builtins/system.kt:1267:1]  

 * builtin function `(set-hook-function hook-symbol function)` =>
   nil  
   Associate a `function` with the `hook-symbol`, to be called when
   the hook is activated. If `function` is nil, nothing will be
   called when the hook is activated.  
   [builtin function defined in builtins/system.kt:1289:1]

 * builtin function `(get-hook-function hook-symbol)` => function  
   Return the function of hook `hook-symbol` (may be nil).  
   [builtin function defined in builtins/system.kt:1314:1]

 * builtin function `(run-hook-function hook-symbol &rest args)` =>
   return-value  
   Run the hook function of hook `hook-symbol` and return its value.
   The `args` are passed to the function.  
   [builtin function defined in builtins/system.kt:1333:1]

