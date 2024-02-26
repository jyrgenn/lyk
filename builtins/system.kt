// system functions

package org.w21.lyk

val debugOffSym = Symbol.intern("off")
val debugListSym = Symbol.intern("list")

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
/// Return the active debug topics (a list of symbols) or all with `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_debug(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val lc = ListCollector()
    for (arg in args) {
        when (arg) {
            debugOffSym -> {
                for (key in Options.debug.keys) {
                    Options.debug[key] = false
                }
                break
            }
            debugListSym -> {
                for (key in Options.debug.keys) {
                    lc.add(key)
                }
                return lc.list()
            }
        }
    }
    for ((key, value) in Options.debug) {
        if (value) {
            lc.add(key)
        }
    }
    return lc.list()
}

