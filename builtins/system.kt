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
/// `-name`. To deactivate all, use `:off`. To show what topics are available,
/// use `list`.
/// Return the active debug topics (a list of symbols) or all with `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_debug(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
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
                if (arg !is Symbol) {
                    throw ValueError("$arg is not a symbol")
                }
                var sym = arg
                var is_on = true
                if (sym.name.startsWith("-")) {
                    is_on = false
                    sym = Symbol.intern(sym.name.substring(1))
                }
                if (!setDebug(sym, is_on)) {
                    throw ValueError("$sym is not a valid debug symbol")
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
fun bi_doc(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
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
            print(doc)
            return theNonPrintingObject
        }
    }
    throw FunctionError("`$ob` is not a function or function symbol")
}


/// builtin numbers
/// fun     bi_numbers
/// std     
/// key     
/// opt     
/// rest    
/// ret     number-list
/// special no
/// doc {
/// Return a list of all number objects currently in use.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_numbers(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    return Number.numbers()
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
fun bi_symbols(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    val lc = ListCollector()
    for (sym in symbolTable.values) {
        lc.add(sym)
    }
    return lc.list()
}

/// builtin gc
/// fun     bi_gc
/// std     
/// key     
/// opt     
/// rest    
/// ret     nil
/// special no
/// doc {
/// Trigger a garbage collection.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_gc(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    System.gc()
    return Nil
}

/// builtin assert
/// fun     bi_assert
/// std     test-form
/// key     
/// opt     message
/// rest    
/// ret     nil
/// special yes
/// doc {
/// Evaluate `test-form`, and if the result is nil, raise an error.
/// The error message includes `test-form`, and, if present, `message`
/// (which is evaluated in that case).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_assert(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject
{
    val (form, message) = args2(args)
    if (eval(form) === Nil) {
        throw AssertionFail(form, eval(message))
    }
    return Nil
}

/// builtin apropos
/// fun     bi_apropos
/// std     string
/// key     
/// opt     
/// rest    
/// ret     none
/// special no
/// doc {
/// Print all interned symbols whose name contains `string`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_apropos(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    val string = arg1(args).toString()
    val symlist = mutableListOf<Symbol>()
    var maxsymlen = 0

    for (sym in symbolTable.values) {
        if (sym.name.contains(string)) {
            symlist.add(sym)
            val len = sym.name.length
            if (len > maxsymlen) {
                maxsymlen = len
            }
        }
    }
    for (sym in symlist) {
        var func = ""
        var bound = ""
        var props = ""
        if (sym.function != null) {
            val special = sym.function?.isSpecial ?: false
            func = when (sym.function) {
                // is Macro -> "macro"
                is Lambda -> "lambda"
                is Builtin -> if (special) {"special form"} else {"builtin"}
                else -> "ewot?!"
            }
        }
        if (sym.getValueOptional() != null) {
            bound = "bound"
        }
        if (sym.props.size > 0) {
            props = "properties"
        }
        stdout.print(padString(sym.name, maxsymlen + 2))
        stdout.print(padString(func, 14))
        stdout.print(padString(bound, 7))
        stdout.println(props)                     
    }
    return theNonPrintingObject
}

/// builtin build-info
/// fun     bi_build_info
/// std     
/// key     
/// opt     as-string
/// rest    
/// ret     alist
/// special no
/// doc {
/// Return an alist with data decribing the current program build.
/// If optional argument `as-string` is true, return the info as a string.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_build_info(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    var as_string = ob2bool(arg1(args))
    var lc = ListCollector()
    for ((key, value) in build_info) {
        lc.add(Cons(Symbol.intern(key), LispString.makeString(value)))
    }
    if (as_string) {
        return LispString.makeString(build_info.values.joinToString(" "))
    }
    return lc.list()
}

// EOF
