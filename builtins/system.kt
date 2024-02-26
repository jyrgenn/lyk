// system functions

package org.w21.lyk

/// builtin debug
/// fun     bi_debug
/// std     
/// key     
/// opt     symbols
/// rest    
/// ret     symbol-list
/// special no
/// doc {
/// Activate and deactivate debug topics (symbols), items/areas to be debugegd.
/// Topics are activated by using their name as argument, or deactivated with
/// `-name`. To deactivate all, use `off`. To show what topics are available,
/// use `list`.
/// Return the active debug topics (a list of symbols).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_debug(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
TODO debug symbols    if (args != Nil) {
        Options.debug = ob2bool(arg1(args))
    }
    return bool2ob(Options.debug)
}

