// system functions

package org.w21.lyk


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
        debug(debugDebugSym) {
            "debug sym is $arg, ${typeOf(arg)}"
        }
        when (arg) {
            debugOffSym -> {
                for (key in Options.debug.keys) {
                    setDebug(key, false)
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
/// opt     as-list
/// rest    
/// ret     none/list
/// special no
/// doc {
/// Print all interned symbols whose name contains `string`.
/// If optional `as-listz` is true, return a list of the symbol names.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_apropos(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    val (pattern, as_list) = args2(args)
    val patternstring = pattern.toString()
    val symlist_lc = ListCollector()
    var maxsymlen = 0

    for (sym in symbolTable.values) {
        if (sym.name.contains(patternstring)) {
            symlist_lc.add(sym)
            val len = sym.name.length
            if (len > maxsymlen) {
                maxsymlen = len
            }
        }
    }
    if (ob2bool(as_list)) {
        return symlist_lc.list()
    }
    for (symbol in symlist_lc.list()) {
        val sym = symbol as Symbol
        var func = "-"
        var bound = "-"
        var props = "-"
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
        stdout.print(sym.name.padEnd(maxsymlen + 2))
        stdout.print(func.padEnd(14))
        stdout.print(bound.padEnd(7))
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

/// builtin desc
/// fun     bi_describe
/// std     object
/// key     
/// opt     
/// rest    
/// ret     description-list
/// special no
/// doc {
/// Describe `object` -- return a alist with the object's attributes.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_describe(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val obj = arg1(args)
    var lc = ListCollector()

    fun entry(name: String, s: String, asSym: Boolean) {
        if (asSym) {
            lc.add(Cons(Symbol.intern(name), Symbol.intern(s)))
        } else {
            lc.add(Cons(Symbol.intern(name), LispString.makeString(s)))
        }
    }
    fun entry(name: String, obj: LispObject) {
        lc.add(Cons(Symbol.intern(name), obj))
    }
    fun entry(name: String, value: Double) {
        lc.add(Cons(Symbol.intern(name), Number.makeNumber(value)))
    }
    fun entry(name: String, value: Int) {
        lc.add(Cons(Symbol.intern(name), Number.makeNumber(value)))
    }
    
    entry("type", typeOf(obj), true)
    when (obj) {
        is Symbol -> {
            entry("name", obj.name, false)
            entry("immutable", bool2ob(obj.immutable))
            entry("desc-name", obj.descName, false)
            entry("function", obj.function ?: Nil)
            entry("props", collectedList() { c ->
                               for ((prop, value) in obj.props) {
                                   c.add(Cons(prop, value))
                               }
                           })
            entry("boundp", bool2ob(obj.getValueOptional() == null))
            entry("value", obj.getValueOptional() ?: Nil)
        }
        is Function -> {
            entry("name", obj.name)
            entry("synopsis",
                  obj.parlist() + " => " + obj.retval.toString(),
                  false)
            entry("special", bool2ob(obj.isSpecial))
        }
        is LispString -> {
            entry("len", obj.value.length)
            entry("value", obj)
        }
        is Environment -> {
            entry("level", obj.level)
            entry("size", obj.map.size)
        }
        is Number -> {
            entry("value", obj)
            entry("intp", bool2ob(obj.isLong()))
        }
        is Vector -> {
            entry("size", obj.the_vector.size)
        }
        is Table -> {
             entry("size", obj.the_table.size)
        }
        is Regexp -> {
            entry("pattern", obj.regex.toString(), false)
        }
        else -> Nil
    }
    return lc.list()
}


/// builtin provide
/// fun     bi_provide
/// std     feature
/// key     
/// opt     
/// rest    
/// ret     feature
/// special no
/// doc {
/// Declare `feature` (a symbol) as provided in case it will be required.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_provide(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val feature = symbolArg(arg1(args), "provide feature")
    featureSet.add(feature)
    return feature
}

/// builtin require
/// fun     bi_require
/// std     feature
/// key     
/// opt     
/// rest    
/// ret     t
/// special no
/// doc {
/// Declare `feature` (a symbol) as required to have been provided earlier.
/// If that is not the case, raise an error
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_require(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val feature = symbolArg(arg1(args), "require feature")
    if (feature in featureSet) {
        return T
    }
    throw NotProvidedError(feature) 
}

val k1Sym = Symbol.intern(":k1")
val k2Sym = Symbol.intern(":k2")

/// builtin barams
/// fun     bi_barams
/// std     a b c
/// key     "k1" to Number.makeNumber(3), "k2" to Nil
/// opt     d, e Number.makeNumber(4)
/// rest    grabbelsack
/// ret     t
/// special no
/// doc {
/// Exercise all kinds of function parameters and return t.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_barams(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val (a, rest1) = args
    val (b, rest2) = rest1
    val (c, rest3) = rest2
    val (d, rest4) = rest3
    val (e, rest5) = rest4
    val grabbelsack = rest5
    val k1 = kwArgs[k1Sym]
    val k2 = kwArgs[k2Sym]
    println("a = $a")
    println("b = $b")
    println("c = $c")
    println("d = $d")
    println("e = $e")
    println("grabbelsack = $grabbelsack")
    println("k1 = $k1")
    println("k2 = $k2")
    return T    
}

// EOF
