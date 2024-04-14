// system functions

package org.w21.lyk


/// builtin set-debug
/// fun     bi_set_debug
/// std     
/// key     
/// opt     
/// rest    symbols
/// ret     symbol-list
/// special no
/// doc {
/// Activate and deactivate debug topics (symbols), items/areas to be debugged.
/// Topics are activated by using their name as argument, or deactivated with
/// `-name`. To deactivate all, use `=off`. To show what topics are available,
/// use `=list`.
/// Return the active debug topics (a list of symbols) or all with `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_set_debug(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
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
                return lc.list
            }
            else -> {
                if (arg !is LSymbol) {
                    throw ValueError("$arg is not a symbol")
                }
                var sym = arg
                var is_on = true
                if (sym.name.startsWith("-")) {
                    is_on = false
                    sym = intern(sym.name.substring(1))
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
    return lc.list
}

/// builtin debug
/// fun     bi_debug
/// std     topic
/// key     
/// opt     
/// rest    data
/// ret     t/nil
/// special yes
/// doc {
/// Print a debug message, if `topic` (a symbol, not evaluated) is enabled.
/// If the topic is enabled, evaluate all other arguments and print a debug
/// message accordingly. If there are no other arguments, just return t if
/// the topic is enabled; nil otherwise.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_debug(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (topic, data) = args
    var result = Nil
    debug(symbolArg(topic, "debug topic")) {
        result = T
        val items = ListCollector()
        for (item in data) {
            items.add(eval(item))
        }
        items.joinToString(" ")
    }
    return result
}

/// builtin doc
/// fun     bi_doc
/// std     symbol-or-function
/// key     
/// opt     return-as-string, synopsis-only
/// rest    
/// ret     object
/// special no
/// doc {
/// Return or print the documentation for `arg`, if available.
/// If optional `return-as-string` is true, return the docstring as a string,
/// otherwise print it and return *the-non-printing-object*.
/// If optional `synopsis-only` is true, print or return the function's
/// synopsis only.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_doc(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (ob, as_string, synopsis_only) = args3(args)
    var func: LObject?

    if (ob is LSymbol) {
        func = ob.function
    } else {
        func = ob
    }
    if (func is LFunction) {
        val doc = if (ob2bool(synopsis_only))
            func.synopsis()
        else
            func.documentation()
        if (ob2bool(as_string)) {
            return makeString(doc)
        } else {
            print(doc)
            return theNonPrintingObject
        }
    }
    throw FunctionError("`${ob.desc()}` is not a function or function symbol")
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
fun bi_numbers(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return LNumber.numbers()
}


/// builtin all-symbols
/// fun     bi_all_symbols
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
fun bi_all_symbols(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val lc = ListCollector()
    for (sym in symbolTable.values) {
        lc.add(sym)
    }
    return lc.list
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
fun bi_gc(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
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
fun bi_assert(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject
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
/// If optional `as-list` is true, return a list of the symbol names.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_apropos(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
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
        return symlist_lc.list
    }
    for (symbol in symlist_lc.list) {
        val sym = symbol as LSymbol
        var func = "-"
        var bound = "-"
        var props = "-"
        if (sym.function != null) {
            val special = sym.function?.isSpecial ?: false
            func = when (sym.function) {
                is LMacro -> "macro"
                is Lambda -> "lambda"
                is LBuiltin -> if (special) {"special form"} else {"builtin"}
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
fun bi_build_info(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    var as_string = ob2bool(arg1(args))
    var lc = ListCollector()
    for ((key, value) in build_info) {
        lc.add(LCons(intern(key), makeString(value)))
    }
    if (as_string) {
        return makeString(build_info.values.joinToString(" "))
    }
    return lc.list
}

/// builtin describe
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
fun bi_describe(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val obj = arg1(args)
    var map = LTable()

    fun entry(name: String, s: String, asSym: Boolean) {
        if (asSym) {
            map.put(intern(name), intern(s))
        } else {
            map.put(intern(name), makeString(s))
        }
    }
    fun entry(name: String, obj: LObject) {
        map.put(intern(name), obj)
    }
    fun entry(name: String, value: Double) {
        map.put(intern(name), makeNumber(value))
    }
    fun entry(name: String, value: Int) {
        map.put(intern(name), makeNumber(value))
    }
    
    entry("type", typeOf(obj).lowercase(), true)
    when (obj) {
        is LSymbol -> {
            entry("name", obj.name, false)
            entry("immutable", bool2ob(obj.immutable))
            entry("desc-name", obj.descName, false)
            entry("function", obj.function ?: Nil)
            entry("props", collectedList { c ->
                               for ((prop, value) in obj.props) {
                                   c.add(LCons(prop, value))
                               }
                           })
            entry("boundp", bool2ob(obj.getValueOptional() != null))
            entry("value", obj.getValueOptional() ?: Nil)
        }
        is LFunction -> {
            entry("name", obj.name)
            entry("synopsis",
                  LCons(LCons(obj.name, obj.parlist()),
                        LCons(returnSym,
                              LCons(obj.retval ?: no_returnSym, Nil))))
            entry("special", bool2ob(obj.isSpecial))
        }
        is LString -> {
            entry("len", obj.the_string.length)
            entry("value", obj)
        }
        is LEnv -> {
            entry("level", obj.level)
            entry("size", obj.the_env.size)
            entry("mapping", LTable(obj.the_env))
        }
        is LNumber -> {
            entry("value", obj)
            entry("intp", bool2ob(obj.isLong()))
        }
        is LVector -> {
            entry("size", obj.the_vector.size)
        }
        is LTable -> {
             entry("size", obj.the_table.size)
        }
        is LRegexp -> {
            entry("pattern", obj.the_regexp.toString(), false)
        }
        else -> Nil
    }
    return map
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
fun bi_provide(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
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
fun bi_require(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val feature = symbolArg(arg1(args), "require feature")
    if (feature in featureSet) {
        return T
    }
    throw NotProvidedError(feature) 
}

val k1Sym = intern(":k1")
val k2Sym = intern(":k2")

/// builtin barams
/// fun     bi_barams
/// std     a b c
/// key     "k1" to makeNumber(3), "k2" to Nil
/// opt     d, e makeNumber(4)
/// rest    grabbelsack
/// ret     t
/// special no
/// doc {
/// Exercise all kinds of function parameters and return t.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_barams(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
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

/// builtin exit
/// fun     bi_exit
/// std     
/// key     
/// opt     exit-status makeNumber(0)
/// rest    
/// ret     none
/// special no
/// doc {
/// End the lisp interpreted with (optional) exit status.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_exit(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    exitLyk(longArg(arg1(args), "exit-status").toInt())
    @Suppress("UNREACHABLE_CODE")
    return Nil                          // yeah, right
}

/// builtin getenv
/// fun     bi_getenv
/// std     variable
/// key     
/// opt     
/// rest    
/// ret     string-value
/// special no
/// doc {
/// Return the value of `variable` in the process environment.
/// If it is not defined, return an empty string.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_getenv(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeString(System.getenv(arg1(args).toString()) ?: "")
}

/// builtin process-env
/// fun     bi_process_env
/// std     
/// key     
/// opt     
/// rest    
/// ret     table
/// special no
/// doc {
/// Return a table with all process environment variables and their values.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_process_env(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val table = LTable()

    for ((key, value) in System.getenv()) {
        table.put(makeString(key), makeString(value))
    }
    return table
}

/// builtin env-table
/// fun     bi_env_table
/// std     
/// key     
/// opt     environment
/// rest    
/// ret     table
/// special no
/// doc {
/// Return a table with all variables and values in the current environment.
/// Optionally, specify an environment.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_env_table(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val table = LTable()
    val arg = arg1(args)
    val environment = if (arg === Nil) {
        currentEnv
    } else{
        envArg(arg, "env-table")
    }

    for ((key, value) in environment.the_env) {
        table.put(key, value)
    }
    return table
}

/// builtin env-vars
/// fun     bi_env_vars
/// std     
/// key     
/// opt     environment
/// rest    
/// ret     var-list
/// special no
/// doc {
/// Return a list with all variables in the current environment.
/// Optionally, specify an environment.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_env_vars(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val arg = arg1(args)
    val environment = if (arg === Nil) {
        currentEnv
    } else{
        envArg(arg, "env-vars")
    }

    return list2lisp(environment.the_env.keys)
}

/// builtin function-definition
/// fun     bi_function_definition
/// std     function
/// key     
/// opt     
/// rest    
/// ret     function-definition-form
/// special no
/// doc {
/// Return `function`'s definition form.
/// `function` must be a lambda function or a macro, not a builtin.
/// The function definition returned contains the real function body,
/// not a copy.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_function_definition(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return functionArg(arg1(args), "function-definition").definition()
}

/// builtin function-docstring
/// fun     bi_function_docstring
/// std     function
/// key     
/// opt     
/// rest    
/// ret     docstring
/// special no
/// doc {
/// Return `function`'s docstring.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_function_docstring(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return makeString(functionArg(arg1(args), "function-docstring").docstring())
}

/// builtin function-parameters
/// fun     bi_function_parameters
/// std     function
/// key     
/// opt     
/// rest    
/// ret     parameter-list
/// special no
/// doc {
/// Return the parameter list of `function`.
/// The returned list is a reconstruction of the original parameter list;
/// modifying it will not change the function's behaviour.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_function_parameters(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val function = functionArg(arg1(args), "function-parameters")
    return function.parlist()
}


// EOF
