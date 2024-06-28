Destructuring Bindings
----------------------

Like lingo before (my Lisp in Go), lyk has destructuring bindings in
the `let` and `let*` special forms. This means you cannot only bind
a single variable to a value, but a number of them in a list
structure that matches the structure of the value:

    (let ((*variable-structure* *value-structure*))
      ...)

It must be possible to match the *variable-structure* to the
*value-structure*; otherwise an error is thrown. A few examples:

    (let (((a) '(3))
      a)
    => 3

    (let (((a b . c) '(2 3 4 5 6)))
      (list a b c))
    => (2 3 (4 5 6))

Elements in the *value-structure* that have no corresponding element
in the *variable-structure* are ignored:

    (let (((a b c) '(3 4 5 6)))
      (list a b c))
    => (3 4 5)

The structures can be nested:

    (let (((a (b c) d) '(3 (4 5) 6 7)))
      (list a b c d))
    => (3 4 5 6)

Elements in the *variable-structure* that don't have a corresponding
one in the *value-structure* are bound to nil:

    (let (((a b c d) ' (3 4 5)))
      (list a b c d))
    => (3 4 5 nil)

If the structures do not match, this is an error:

    (let (((a (b c) d) '(3 4 5 6 7)))
      (list a b c d))
    => ValueError: value structure error, not a pair: number 4


A real albeit relatively simple example is the use of destructuring
bind in the macro `dolist`[1], which iterates over the elements of a
list. The first argument of `dolist` is a list of control variables:
(1) the variable to which successively bind the elements of the list,
(2) the list itself, and
(3) optionally a form evaluated as the result:

**dolist** (var list-form [result-form]) statement*

[1] This is a simpler form of the `dolist` macro described in the
    CLHS: http://clhs.lisp.se/Body/m_dolist.htm

To get at the values of the control variables, I could access them
like this:

    (defmacro dolist (control-vars &rest bodyforms)
      (let ((var (car control-vars))
            (list-form (cadr control-vars))
            (result-form (caddr control-vars)))
        ...))

This is clumsy and inefficient, as it has to evaluate three function
calls and `control-vars` in each of them. It is easier and and more
efficient to do it in one go:

    (defmacro dolist (control-vars &rest bodyforms)
      (let (((var list-form result-form) control-vars))
        ...))

[actual implementation in src/preload/20-macros.lisp]

More examples can be found in
src/regtests/070-destructuring-bind.lisp.


Of course it would be even nicer to express this directly in the
parameter list, saving the extra step of the explicit variable
binding:

    (defmacro dolist ((var list-form result-form) &rest bodyforms)
      ...)

As the parameter list of a function or macro is a much more complex
thing anyway with the `&optional`, `&key`, and `&rest` bindings,
*and* has more implications on other parts of the system, this is
much trickier to implement. Perhaps I'll get around to doing that,
too, but don't hold your breath.
