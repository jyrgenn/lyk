Builtin Functions
=================

Builtin functions are in builtins/*.kt, roughly grouped into some
subject areas. The templace for defining a builtin function is this:

    /// builtin <NAME>
    /// fun     bi_<NAME> [kotlin identifier]
    /// std     <mandatory arguments, or none> [Lisp symbols]
    /// key     <keyword arguments, or none> [see below]
    /// opt     <optional arguments, or none> [see below]
    /// rest    <argument for additional arguments> [Lisp symbol]
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

The part behind a triple slash `///` comment is metadata for the
function registration.
