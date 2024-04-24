// system functions

package org.w21.lyk

import java.io.File
//import kotlinx.coroutine.*


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
            "debug sym is $arg, ${arg.type}"
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
/// std     pattern
/// key     
/// opt     as-list
/// rest    
/// ret     none/list
/// special no
/// doc {
/// Print all interned symbols whose name contains `pattern`.
/// If optional `as-list` is true, return a list of the symbol names.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_apropos(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (pattern, as_list) = args2(args)
    var pattern_re = Regex("")
    var pattern_s = ""
    val match_fun: (String) -> Boolean

    fun match_s(s: String) = s.contains(pattern_s)
    fun match_r(s: String) = pattern_re.find(s) != null

    if (pattern is LRegexp) {
        pattern_re = pattern.the_regexp
        match_fun = ::match_r
    } else {
        pattern_s = pattern.toString()
        match_fun = ::match_s
    }

    val symlist_lc = ListCollector()
    var maxsymlen = 0

    for (sym in symbolTable.values) {
        if (match_fun(sym.name)) {
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
    
    entry("type", obj.type, true)
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

/// builtin make-symbol
/// fun     bi_make_symbol
/// std     name
/// key     
/// opt     
/// rest    
/// ret     symbol
/// special no
/// doc {
/// Create and return a fresh, uninterned symbol whose name is `name`.
/// The new symbol is neither bound nor fbound and has a null property list.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_make_symbol(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val name = arg1(args)
    return LSymbol(stringArg(name, "make-symbol"), false)
}

/// builtin symbol-value
/// fun     bi_symbol_value
/// std     symbol
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the value of `symbol`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_symbol_value(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val sym = arg1(args)
    return symbolArg(sym, "symbol-value").getValue()
}

/// builtin collect-performance-data
/// fun     bi_collect_perfdata
/// std     
/// key     
/// opt     
/// rest    bodyforms
/// ret     alist
/// special yes
/// doc {
/// Evaluate `bodyforms` and return a list of performance data.
/// This list is an alist with the number of conses created, the number
/// of evals performed, and the number of seconds taken to perform the
/// evaluation, e.g.
///     ((conses . 6055097) (evals . 4755187) (secs . 0.265043))
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_collect_perfdata(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val bodyforms = args
    val (conses, evals, seconds) = measurePerfdataDetail {
        evalProgn(bodyforms)
    }
    return LCons(LCons(intern("conses"), makeNumber(conses)),
                 LCons(LCons(intern("evals"), makeNumber(evals)),
                       LCons(LCons(intern("secs"),
                                   makeNumber(seconds)),
                             Nil)))
}

/// builtin no-warnings
/// fun     bi_no_warnings
/// std     
/// key     
/// opt     
/// rest    bodyforms
/// ret     result
/// special yes
/// doc {
/// Eval bodyforms and return the result while suppressing warnings
/// (and other info/notice messages). If *warnings-as-errors* is true,
/// the error is raised nevertheless.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_no_warnings(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val previous_verbosity = Options.verbosity
    try {
        Options.verbosity = 0
        return evalProgn(args)
    } finally {
        Options.verbosity = previous_verbosity
    }
}

val inShellKSym = intern(":in-shell")
val inputKSym = intern(":input")
val outputKSym = intern(":output")
val errorOutputKSym = intern(":error-output")
val envKSym = intern(":env")
val raiseErrorKSym = intern(":raise-error")

val shell_meta_characters = "\"'`|&,;[{(<>)}]*?$".toSet()

fun has_shellmeta(s: String): Boolean {
    for (ch in s) {
        if (ch in shell_meta_characters) {
            return true
        }
    }
    return false
}

/// builtin run-program
/// fun     bi_run_program
/// std     command
/// key     "in-shell" to Nil, "input" to Nil, "output" to T, "error-output" to T, "env" to T, "raise-error" to Nil
/// opt     
/// rest    command-arguments
/// ret     exit-status
/// special no
/// doc {
/// Run an external command and return its exit status. If the command is
/// more than one string, run it directly. Otherwise, if it is a single
/// string:
///   - if &key `in-shell` is t, run command as a shell command line with
///     `/bin/sh`.
///   - if &key `in-shell` is a string, use it as the shell and run the
///     command in it.
///   - if &key `in-shell` is nil (the default), run command with `/bin/sh`
///     if it contains shell meta characters (`"'\`|&;[(<>)]*?$`). Otherwise,
///     split the string on whitespace and run it directly.
///
/// If &key `input` is a string or a stream, use it as the standard input
/// stream of the command. If it is t, the command reads from the normal
/// standard input; if it is nil, redirect it from the null device.
///
/// If &key `output` is a stream, use it as the standard output stream
/// of the command. You can use `make-string-output-stream` (with
/// `get-output-stream-string`) to capture the output of the command in the
/// program. The same goes for `error-output` and the standard error output.
/// If `output` is t, use the standard output; if it is nil,  redirect the
/// command's output to the null device. The same goes for `error-output`.
///
/// If &key `env` (a table) is non-nil, use it as the process environment of
/// the command.
///
/// If &key `raise-error` is true, raise an error if the command returns a
/// non-zero exit status.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_run_program(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (command, rest) = args
    val command_s = stringArg(command, "run-program command")
    val key_in_shell = kwArgs[inShellKSym] ?: Nil
    var cmd_arg =
        if (rest == Nil) {
            when (key_in_shell) {
                T -> listOf("/bin/sh", "-c", command_s)
                Nil -> if (has_shellmeta(command_s)) {
                           listOf("/bin/sh", "-c", command_s)
                       } else {
                           command_s.split(' ', '\t')
                       }
                is LString -> listOf(key_in_shell.the_string, "-c", command_s)
                else ->
                    throw ArgumentError("&key in-shell is not t or nil or a "
                                        + "string: ${key_in_shell.type} "
                                        + "$key_in_shell")
            }
        } else {
            stringlistArg(args, "run-program")
        }

    val pb = ProcessBuilder(cmd_arg)

    val key_input = kwArgs[inputKSym] ?: Nil
    if (key_input is LString) {
        pb.redirectInput(ProcessBuilder.Redirect.PIPE)
    } else if (key_input === T) {
        pb.redirectInput(ProcessBuilder.Redirect.INHERIT)
    } else if (key_input === Nil) {
        pb.redirectInput(ProcessBuilder.Redirect.from(File(devNullPath)))
    } else {
        throw ArgumentError("&key input is not a string or t or nil:"
                            +" ${key_input.type} $key_input")
    }
    val key_output = kwArgs[outputKSym] ?: Nil
    if (key_output is LStream) {
        pb.redirectOutput(ProcessBuilder.Redirect.PIPE)
    } else if (key_output === T) {
        pb.redirectOutput(ProcessBuilder.Redirect.INHERIT)
    } else if (key_output === Nil) {
        pb.redirectOutput(ProcessBuilder.Redirect.to(File(devNullPath)))
    } else {
        throw ArgumentError("&key output is not a stream or t or nil:"
                            +" ${key_output.type} $key_output")
    }
    val key_error_output = kwArgs[errorOutputKSym] ?: Nil
    if (key_error_output is LStream) {
        pb.redirectError(ProcessBuilder.Redirect.PIPE)
    } else if (key_error_output === T) {
        pb.redirectError(ProcessBuilder.Redirect.INHERIT)
    } else if (key_error_output === Nil) {
        pb.redirectError(ProcessBuilder.Redirect.to(File(devNullPath)))
    } else {
        throw ArgumentError("&key error-stream is not a stream or t or nil:"
                            +" ${key_error_output.type} $key_error_output")
    }
    // TODO prepare process environment

    val proc = pb.start()

    // Of course, what we *really* want to do there is multiplexed reading and
    // writing, so we don't run into any blocking situations. This would also
    // give us the option to interact with the subprocess, by way of callback
    // functions. That would be *nice*.
    //
    // But for the moment I think I can get by with this, hoping that all
    // streams involded have a sufficiently large buffer size for my little
    // applications.

    if (key_input is LString) {
        val writer = proc.getOutputStream().bufferedWriter()
        writer.write(key_input.the_string)
        writer.close()
        // proc.getOutputStream().close()
    }
    val exit_status = proc.waitFor()

    if (key_output is LStream) {
        val reader = proc.getInputStream().bufferedReader()
        val buf = CharArray(4096)
        while (reader.ready()) {
            val n = reader.read(buf)
            key_output.write(buf, n)
        }
        reader.close()
    }
    if (key_error_output is LStream) {
        val reader = proc.getErrorStream().bufferedReader()
        val buf = CharArray(4096)
        while (reader.ready()) {
            val n = reader.read(buf)
            key_error_output.write(buf, n)
        }
        reader.close()
    }

    if (exit_status != 0 && kwArgs[raiseErrorKSym] !== Nil) {
        throw ProcessError(exit_status, "run-program", command_s)
    }
    return makeNumber(exit_status)
}




// EOF
