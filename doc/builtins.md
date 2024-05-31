Builtin Functions
=================

Builtin functions are in builtins/*.kt, roughly grouped into a
number of subject areas. The template for defining a builtin
function (actually `builtins/skeleton-builtin` so it can be easily
inserted into the sources) is roughly this:

    /// builtin <NAME> [Lisp symbol]
    /// fun     bi_<NAME> [kotlin identifier]
    /// std     <mandatory arguments, or none> [Lisp symbols]
    /// key     <keyword parameters and defaults, or none> [see below]
    /// opt     <optional parameters and defaults, or none> [see below]
    /// rest    <parameter for additional arguments> [Lisp symbol]
    /// ret     <brief description of return value> [Lisp symbol]
    /// special no [or yes, for special forms]
    /// doc {
    /// [docstring with very descriptive initial line, and maybe more]
    /// }
    /// end builtin
    @Suppress("UNUSED_PARAMETER")
    fun bi_<NAME>(args: LObject, kwArgs: Map<LSymbol, LObject>,
                  suppp: Map<LSymbol, Boolean>): LObject {
        ... [function code]
        return <VALUE> [Lisp object]
    }

The part behind a triple-slash `///` comment is a directive for the
function registration. All the sections with triple-slash comments
are transformed into builtin function registrations by
`scripts/generate-builtin-init`, which is called in the `Makefile`
to generate `generated/init-builtins.kt`. All directives must be
present.

The `@Suppress("UNUSED_PARAMETER")` is in the template to suppress
related warnings because most functions use neither `kwArgs` nor
`suppp`, but the interface must be uniform nevertheless.


Directives
----------

The lines begin with a triple slash, then, after a blank, the name
of the directive, then, after whitespace, its argument(s). The
multi-line syntax of `/// doc {` is a little different; see below.

| Directive | Purpose                                               |
| ----------|-------------------------------------------------------|
| `builtin` | the function name in Lisp                             |
| `fun`     | name of the implementation of the function in Kotlin  |
| `std`     | the mandatory standard arguments as Lisp symbols      |
| `key`     | keyword arguments, with default values (see below)    |
| `opt`     | optional arguments, with default values (see below)   |
| `rest`    | the argument for all additional arguments             |
| `ret`     | a brief description of the return value               |
| `special` | `no` for a function, `yes` for a special form         |
| `doc {`   | the docstring, with a terse summary in the first line |

The concrete syntax to specify these is a bit messy, a slightly
incoherent mix of Lisp and Kotlin. Maybe a future version of
`scripts/generate-builtin-init` will clean that up now that the
extent of the mess is clearly documented here. :–}


**builtin**
> This is the name of the function in Lisp, i.e. a symbol. Example:

    /// builtin  table-put

**fun**
> The name of the Kotlin function implementing the builtin function,
> a Kotlin identifier. By convention, these begin with `bi_`
> followed by the name of the function if possible. If that isn't,
> as with functions like `+`, the name describes the function or the
> symbol(s). Example:

    /// fun  bi_table_put

**std**
> The names of the mandatory standard arguments of the function, if
> any, as Lisp symbols separated by whitespace. Example:

    /// std  table key value

**key**
> The keyword arguments, if any, with default values. Syntactically,
> this is a mapping of a string (which is the argument's symbol) to
> the default value, a Kotlin expression for an LObject. This
> mapping is like in Kotlin's Map literals, "STRING to EXPR".
> The keyword symbol's name (the STRING) must be given *without*
> the colon. Other than with the `opt` 
>
> For the the value, either a Kotlin variable/value name must be
> used (like `T` or `Nil`), or something else that evaluates to an
> LObject -- for numbers, `makeNumber(0)`, for strings
> `makeString(" ")`, for symbols, `intern(":error")` etc. Multiple
> keywords are separated by commas. Example:

    /// key  "k1" to makeNumber(3), "input" to Nil

**opt**
> The optional arguments, if any, with default values, a mapping
> from a Lisp symbol, written as a symbol, to a Kotlin value (see
> above at **key**). Here, though, the symbol is not quoted as a
> string, and the default value follows separated by whitespace. If
> the default value is Nil, it can be omitted. Example:

    /// opt  separator makeString(" "), value-table

**rest**
> The argument for all additional arguments, if intended, a Lisp
> symbol. Example:

    /// rest  bodyforms

**ret**
> A brief description of the return value. Example:

    /// ret  string

**special**
> This is `no` for a function, `yes` for a special form. Example:

    ///special  no

**doc {**
> The docstring of the function, starting on the following line.
> These are often multiple lines, all introduced by the triple-slash
> comment sign (`///`), and ended by a line starting with `/// }`.
> The first line of the docstring should be enough to give the user
> an idea what the function does; following lines can provide
> further all details. By convention, the names of the parameter
> symbols are in backquotes. Example:

    /// doc {  
    /// Return the contents of the address part of the `list` register.  
    /// The car of nil is nil.  
    /// }  
