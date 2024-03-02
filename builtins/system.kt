// system functions

package org.w21.lyk

val debugOffSym = Symbol.intern(":off")
val debugListSym = Symbol.intern(":list")
val debugDebugSym = Symbol.intern("debug")

/// builtin debug
/// fun     bi_debug
/// std     
/// key     
/// opt     
/// rest    symbols
/// ret     symbol-list
/// special no
/// doc {
/// Activate and deactivate debug topics (symbols), items/areas to be debugged.
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
        debug(debugDebugSym, "debug sym is", arg, typeOf(arg))
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
            else -> {
                if (!(arg is Symbol && setDebug(arg))) {
                    throw ValueError("$arg is not a valid debug symbol")
                }
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

/// builtin doc
/// fun     bi_doc
/// std     symbol-or-function
/// key     
/// opt     return-as-string
/// rest    
/// ret     object
/// special no
/// doc {
/// Return or print the documentation for `arg`, if available.
/// If optional `return-as-string` is true, return the docstring as a string,
/// otherwise print it and return *the-non-printing-object*.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_doc(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val ob = arg1(args)
    var func: LispObject?
    val as_string = ob2bool(arg2(args))

    if (ob is Symbol) {
        func = ob.function
    } else {
        func = ob
    }
    if (func is Function) {
        val doc = func.documentation()
        if (as_string) {
            return LispString(doc)
        } else {
            println(doc)
            return theNonPrintingObject
        }
    }
    throw FunctionError("`$ob` is not a function or function symbol")
}


/// builtin symbols
/// fun     bi_symbols
/// std     
/// key     
/// opt     
/// rest    
/// ret     symbol-list
/// special no
/// doc {
/// Return a list of all symbols.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_symbols(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val lc = ListCollector()

    for (sym in symbolTable.values) {
        lc.add(sym)
    }
    return lc.list()
}

