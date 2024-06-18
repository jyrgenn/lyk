// system functions

package org.w21.lyk

import java.io.File
import java.io.InputStream
import kotlin.concurrent.thread

import kotlin.time.Duration
import kotlin.time.DurationUnit
import kotlin.time.measureTime


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
fun bi_set_debug(args: LObject, kwArgs: Map<LSymbol, LObject>,
                 suppp: Map<LSymbol, Boolean>): LObject {
    val lc = ListCollector()
    for (arg in args) {
        debug(debugDebugSym) {
            "debug sym is $arg, ${arg.obtype}"
        }
        when (arg) {
            debugOffSym -> {
                for (key in Options.debug.keys) {
                    setDebug(key, false)
                }
                break
            }
            debugListSym -> {
                for ((key, value) in Options.debug) {
                    lc.add(LCons(key, bool2ob(value)))
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
/// message from all of the value.
/// Return t if the topic is enabled; nil otherwise.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_debug(args: LObject, kwArgs: Map<LSymbol, LObject>,
             suppp: Map<LSymbol, Boolean>): LObject {
    val (topic, data) = args
    var result = Nil
    debug(symbolArg(topic, " topic")) {
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
/// otherwise print it and return `*the-non-printing-object*`.
/// If optional `synopsis-only` is true, print or return the function's
/// synopsis only.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_doc(args: LObject, kwArgs: Map<LSymbol, LObject>,
           suppp: Map<LSymbol, Boolean>): LObject {
    val (ob, as_string, synopsis_only) = args3(args)
    var func: LObject?

    if (ob is LSymbol) {
        func = ob.function
    } else {
        func = ob
    }
    if (func is LFunction) {
        val doc = if (synopsis_only.toBoolean())
            func.synopsis()
        else
            func.documentation()
        if (as_string.toBoolean()) {
            return makeString(doc)
        } else {
            print(doc)
            return theNonPrintingObject
        }
    }
    throw FunctionError("`${ob.desc(null)}` is not a function or function symbol")
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
fun bi_numbers(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
    return LNumber.numbers()
}


/// builtin do-symbols
/// fun     bi_do_symbols
/// std     control-vars
/// key     
/// opt     
/// rest    bodyforms
/// ret     result
/// special yes
/// doc {
/// (do-symbols (var [result-form]) bodyforms...)
/// Iterate over all symbols, bind them to `var`, and execute `bodyforms`.
/// If `result-form` is specified, evaluate and return it afterwards.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_do_symbols(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
    val (control_vars, bodyforms) = args
    val control = consArg(control_vars, " control-vars")
    val varsym = symbolArg(control.car)
    val resultform = control.cdr.car
    for (sym in symbolTable.values) {
        varsym.bind(sym)
        evalProgn(bodyforms)
    }
    return eval(resultform)
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
fun bi_gc(args: LObject, kwArgs: Map<LSymbol, LObject>,
          suppp: Map<LSymbol, Boolean>): LObject {
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
fun bi_assert(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject
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
/// `pattern` may be a string or a regular expression.
/// If optional `as-list` is true, return a list of the symbol names.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_apropos(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
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
    if (as_list.toBoolean()) {
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
fun bi_build_info(args: LObject, kwArgs: Map<LSymbol, LObject>,
                  suppp: Map<LSymbol, Boolean>): LObject {
    var as_string = arg1(args).toBoolean()
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
fun bi_describe(args: LObject, kwArgs: Map<LSymbol, LObject>,
                suppp: Map<LSymbol, Boolean>): LObject {
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
    
    entry("type", obj.obtype, true)
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
            entry("docstring", makeString(obj.docstring()))
            entry("definition", obj.definition())
            entry("body", obj.body())
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
fun bi_provide(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
    val feature = symbolArg(arg1(args), " feature")
    push(modulesSym, feature)
    return feature
}

/// builtin require
/// fun     bi_require
/// std     feature
/// key     
/// opt     filename
/// rest    
/// ret     t
/// special no
/// doc {
/// If `feature` (a symbol) is not already provided, load it from `filename`
/// (default: name of the feature). If the feature is still not provided,
/// throw an error.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_require(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
    val (sym, fname) = args2(args)
    val feature = symbolArg(sym, " feature")
    if (feature in modulesSym.getValueOptional() ?: Nil) {
        return T
    }
    val filename = (if (fname !== Nil) fname else sym).toString()
    load(filename, true, false, true)
    if (feature in modulesSym.getValueOptional() ?: Nil) {
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
fun bi_barams(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
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
fun bi_exit(args: LObject, kwArgs: Map<LSymbol, LObject>,
            suppp: Map<LSymbol, Boolean>): LObject {
    exitLyk(longArg(arg1(args)).toInt())
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
fun bi_getenv(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
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
fun bi_process_env(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
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
fun bi_env_table(args: LObject, kwArgs: Map<LSymbol, LObject>,
                 suppp: Map<LSymbol, Boolean>): LObject {
    val table = LTable()
    val arg = arg1(args)
    val environment = if (arg === Nil) {
        currentEnv
    } else{
        envArg(arg)
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
fun bi_env_vars(args: LObject, kwArgs: Map<LSymbol, LObject>,
                suppp: Map<LSymbol, Boolean>): LObject {
    val arg = arg1(args)
    val environment = if (arg === Nil) {
        currentEnv
    } else{
        envArg(arg)
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
fun bi_function_definition(args: LObject, kwArgs: Map<LSymbol, LObject>,
                           suppp: Map<LSymbol, Boolean>): LObject {
    return functionArg(arg1(args)).definition()
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
fun bi_function_docstring(args: LObject, kwArgs: Map<LSymbol, LObject>,
                          suppp: Map<LSymbol, Boolean>): LObject {
    return makeString(functionArg(arg1(args)).docstring())
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
fun bi_function_parameters(args: LObject, kwArgs: Map<LSymbol, LObject>,
                           suppp: Map<LSymbol, Boolean>): LObject {
    val function = functionArg(arg1(args))
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
fun bi_make_symbol(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
    val name = arg1(args)
    return LSymbol(stringArg(name), false, false)
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
fun bi_symbol_value(args: LObject, kwArgs: Map<LSymbol, LObject>,
                    suppp: Map<LSymbol, Boolean>): LObject {
    val sym = arg1(args)
    return symbolArg(sym).getValue()
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
fun bi_collect_perfdata(args: LObject, kwArgs: Map<LSymbol, LObject>,
                        suppp: Map<LSymbol, Boolean>): LObject {
    val bodyforms = args
    val (calls, conses, evals, seconds) = measurePerfdata {
        evalProgn(bodyforms)
    }
    return LCons(LCons(intern("calls"), makeNumber(calls)),
                 LCons(LCons(intern("conses"), makeNumber(conses)),
                       LCons(LCons(intern("evals"), makeNumber(evals)),
                             LCons(LCons(intern("secs"), makeNumber(seconds)),
                                   Nil))))
}

/// builtin measure-time
/// fun     bi_measure_time
/// std     
/// key     
/// opt     
/// rest    bodyforms
/// ret     duration
/// special yes
/// doc {
/// Run bodyforms and return the elapsed time in seconds.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_measure_time(args: LObject, kwArgs: Map<LSymbol, LObject>,
                    suppp: Map<LSymbol, Boolean>): LObject {
    return makeNumber(measureTime {
                          evalProgn(args)
                      }.toDouble(DurationUnit.SECONDS))
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
fun bi_no_warnings(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
    val previous_verbosity = Options.verbosity
    try {
        Options.verbosity = 0
        return evalProgn(args)
    } finally {
        Options.verbosity = previous_verbosity
    }
}


val onSym = intern("on")

/// builtin warnings-as-errors
/// fun     bi_warnings_as_errors
/// std     
/// key     
/// opt     on
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if warnings as handled as errors, nil otherwise.
/// With optional `on` argument, set warnings to be handled as errors iff
/// `on` is true. In this case, return the status from *before* setting it.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_warnings_as_errors(args: LObject, kwArgs: Map<LSymbol, LObject>,
                          suppp: Map<LSymbol, Boolean>): LObject {
    val result = bool2ob(Options.warnIsError)
    if (suppp[onSym] ?: false) {
        Options.warnIsError = arg1(args).toBoolean()
        warningsAsErrors.setROValue(bool2ob(Options.warnIsError))
    }
    return result
}


val inShellKSym = intern(":in-shell")
val inputKSym = intern(":input")
val outputKSym = intern(":output")
val errorOutputKSym = intern(":error-output")
val envKSym = intern(":env")
val raiseErrorKSym = intern(":raise-error")

val shell_meta_characters = "~\"'`|&,;[{(<>)}]*?$".toSet()

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
/// key     "in-shell" to Nil, "input" to Nil, "output" to T, "error-output" to T, "env" to Nil, "raise-error" to Nil
/// opt     
/// rest    
/// ret     exit-status
/// special no
/// doc {
/// Run an external command and return its exit status. If command  is a
/// list of strings, run it directly. Otherwise, if it is a single string:
///   - if &key `in-shell` is t, run command as a shell command line with
///     `/bin/sh`.
///   - if &key `in-shell` is a string, use it as the shell and run the
///     command in it, using the "-c" option like for `/bin/sh`.
///   - if &key `in-shell` is nil (the default), run command with `/bin/sh`
///     if it contains shell meta characters (~"'`|&,;[{(<>)}]*?$).
///     Otherwise, split the string on whitespace and run it directly.
///
/// If &key `input` is a string or a stream, use it as the standard input
/// stream of the command. If it is t, the command reads from the normal
/// standard input; if it is nil, redirect it from the null device.
///
/// If &key `output` is a stream, use it as the standard output stream
/// of the command. You can use `make-string-output-stream` (with
/// `get-output-stream-string`) to capture the output of the command in the
/// program. The same goes for `error-output` and the standard error output.
/// If `output` is t, use the standard output; if it is nil, redirect the
/// command's output to the null device. The same goes for `error-output`.
///
/// If &key `env` (a table) is non-nil, use it as the process environment of
/// the command. Otherwise, use the default process environment.
///
/// If &key `raise-error` is true, raise an error if the command returns a
/// non-zero exit status.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_run_program(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
    val command = arg1(args)
    val key_in_shell = kwArgs[inShellKSym] ?: Nil
    val env = kwArgs[envKSym] ?: Nil
    
    var cmd_arg =
        when (command) {
            is LString -> {
                val command_s = command.the_string
                when (key_in_shell) {
                    T -> listOf("/bin/sh", "-c", command_s)
                    Nil -> if (has_shellmeta(command_s)) {
                               listOf("/bin/sh", "-c", command_s)
                           } else {
                               command_s.split(' ', '\t')
                           }
                    is LString -> listOf(key_in_shell.the_string, "-c",
                                         command_s)
                    else ->
                        throw ArgumentError("&key in-shell is not t or nil or "
                                            + "a string: ${key_in_shell.obtype}"
                                            + " $key_in_shell")
                }
            }
            is LCons -> stringlistArg(command)
            else ->
                throw TypeError(command, "list or string",
                                "run-program command")
        }

    val pb = ProcessBuilder(cmd_arg)

    when (env) {
        Nil -> {}
        is LTable -> {
            val pb_table = pb.environment()
            pb_table.clear()
            for ((key, value) in env.items()) {
                pb_table.put(key.toString(), value.toString())
            }
        }
        else ->
            throw TypeError(env, "table or nil", "run-process :env argument")
    }

    val key_input = kwArgs[inputKSym] ?: Nil
    when (key_input) {
        is LStream, is LString ->
            pb.redirectInput(ProcessBuilder.Redirect.PIPE)
        T ->
            pb.redirectInput(ProcessBuilder.Redirect.INHERIT)
        Nil ->
            pb.redirectInput(ProcessBuilder.Redirect.from(File(devNullPath)))
        else ->
            throw ArgumentError("&key input is not a stream or string or t or "
                                + " nil: ${key_input.obtype} $key_input")
    }
    val key_output = kwArgs[outputKSym] ?: Nil
    when (key_output) {
        is LStream ->
            pb.redirectOutput(ProcessBuilder.Redirect.PIPE)
        T ->
            pb.redirectOutput(ProcessBuilder.Redirect.INHERIT)
        Nil ->
            pb.redirectOutput(ProcessBuilder.Redirect.to(File(devNullPath)))
        else ->
            throw ArgumentError("&key output is not a stream or t or nil:"
                                +" ${key_output.obtype} $key_output")
    }
    val key_error_output = kwArgs[errorOutputKSym] ?: Nil
    when (key_error_output) {
        is LStream ->
            pb.redirectError(ProcessBuilder.Redirect.PIPE)
        T ->
            pb.redirectError(ProcessBuilder.Redirect.INHERIT)
        Nil -> {
            pb.redirectError(ProcessBuilder.Redirect.to(File(devNullPath)))
        }
        else ->
            throw ArgumentError("&key error-stream is not a stream or t or nil:"
                                +" ${key_error_output.obtype} "
                                + "$key_error_output")
    }
    // TODO prepare process environment

    val proc = pb.start()

    // What I do there is multiplexed reading and writing, so I don't run into
    // any blocking situations. This should also give me the option to interact
    // with the subprocess, by way of callback functions or the like. That would
    // be *nice*.
    //
    // I wanted to do this with coroutines, which seems to be the more modern
    // lightweight way to go, but the parallel execution that I hoped for
    // wasn't there. With threads, it worked instantly. In the end, I don't
    // really mind. The three threads I create here still cheap compared to the
    // effort of creating a separate process and starting an external program.

    fun handleProcessOutput(in_stream: InputStream, writer: LStream) {
        val reader = in_stream.bufferedReader()
        val buf = CharArray(4096)
        while (true) {
            val n = reader.read(buf)
            if (n < 0) {
                break
            }
            writer.write(buf, n)
        }
        reader.close()
    }

    thread {
        if (key_input is LString || key_input is LStream) {
            val reader: LStream =
                when (key_input) {
                    is LString -> StringReaderStream(key_input.the_string)
                    is LStream -> key_input
                    else -> throw InternalError(
                                "unpossible! key_input = key_input "
                                + "(${key_input.obtype})")
                }
            val writer = proc.getOutputStream().bufferedWriter()
            while (true) {
                val ch = reader.read()
                if (ch == null) {
                    break
                }
                writer.write(ch.code)
            }
            reader.close()
            writer.close()
        }
    }
    thread {
        if (key_output is LStream) {
            handleProcessOutput(proc.getInputStream(), key_output)
        }
    }
    thread {
        if (key_error_output is LStream) {
            handleProcessOutput(proc.getErrorStream(), key_error_output)
        }
    }
    val exit_status = proc.waitFor()


    if (exit_status != 0 && kwArgs[raiseErrorKSym] !== Nil) {
        throw ProcessError(exit_status, "run-program", command.toString())
    }
    return makeNumber(exit_status)
}

/// builtin error-object-p
/// fun     bi_error_object_p
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t iff `object` is an error object, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_error_object_p(args: LObject, kwArgs: Map<LSymbol, LObject>,
                      suppp: Map<LSymbol, Boolean>): LObject {
    return bool2ob(arg1(args) is ErrorObject)
}

/// builtin sleep
/// fun     bi_sleep
/// std     seconds
/// key     
/// opt     
/// rest    
/// ret     nil
/// special no
/// doc {
/// Suspend execution for approximately the number of `seconds` and return.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_sleep(args: LObject, kwArgs: Map<LSymbol, LObject>,
             suppp: Map<LSymbol, Boolean>): LObject {
    val ms = (numberArg(arg1(args)) * 1000).toLong()
    if (ms < 0) {
        throw ArgumentError("seconds argument must not be negative")
    }
    Thread.sleep(ms)
    return Nil
}

/// builtin return
/// fun     bi_return
/// std     
/// key     
/// opt     value
/// rest    
/// ret     none
/// special no
/// doc {
/// Return from the innermost function with `value` (defaults to nil).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_return(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
    throw ReturnSignal(arg1(args))
}

/// builtin user-name
/// fun     bi_user_name
/// std     
/// key     
/// opt     
/// rest    
/// ret     string
/// special no
/// doc {
/// Return the username of the current user.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_user_name(args: LObject, kwArgs: Map<LSymbol, LObject>,
                 suppp: Map<LSymbol, Boolean>): LObject {
    return makeString(System.getProperty("user.name"))
}

/// builtin load-preload-code
/// fun     bi_load_preload_code
/// std     
/// key     
/// opt     verbose
/// rest    
/// ret     object
/// special no
/// doc {
/// Load the preload code.
/// This makes only sense if it hasn't been loaded before.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_load_preload_code(args: LObject, kwArgs: Map<LSymbol, LObject>,
                         suppp: Map<LSymbol, Boolean>): LObject {
    return load_string(preload_code, "*preload-code*",
                       print = arg1(args).toBoolean())
}


/// builtin system-time
/// fun     bi_system_time
/// std     
/// key     
/// opt     
/// rest    
/// ret     milliseconds
/// special no
/// doc {
/// Return the current system time in milliseconds.
/// This is the number of milliseconds since the beginning of the Unix
/// epoch, in theory. It may not be a good source for determining the
/// current calendar time.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_system_time(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
    return makeNumber(System.currentTimeMillis())
}

/// builtin system-running-time
/// fun     bi_system_running_time
/// std     
/// key     
/// opt     
/// rest    
/// ret     milliseconds
/// special no
/// doc {
/// Return the number of milliseconds since the lyk system started.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_system_running_time(args: LObject, kwArgs: Map<LSymbol, LObject>,
                           suppp: Map<LSymbol, Boolean>): LObject {
    return makeNumber(System.currentTimeMillis() - systemStartedTime)
}

/// builtin system-started-time
/// fun     bi_system_started_time
/// std     
/// key     
/// opt     
/// rest    
/// ret     milliseconds
/// special no
/// doc {
/// Return the number of milliseconds since the lyk system started.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_system_started_time(args: LObject, kwArgs: Map<LSymbol, LObject>,
                           suppp: Map<LSymbol, Boolean>): LObject {
    return makeNumber(systemStartedTime)
}

/// builtin system-perfdata
/// fun     bi_system_perfdata
/// std     
/// key     
/// opt     
/// rest    
/// ret     result
/// special no
/// doc {
/// Return the overall system performance data as an alist.
/// The items in the returned list are, in this order:
///   `call`: the number of function calls
///   `cons`: the number of conses created
///   `eval`: the number of evalulations done
///   `secs`: the number of seconds elapsed since the start of lyk
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_system_perfdata(args: LObject, kwArgs: Map<LSymbol, LObject>,
                       suppp: Map<LSymbol, Boolean>): LObject {
    val runtime = (System.currentTimeMillis() - systemStartedTime) / 1000.0
    return list(LCons(intern("call"), makeNumber(callCounter)),
                LCons(intern("cons"), makeNumber(consCounter)),
                LCons(intern("eval"), makeNumber(evalCounter)),
                LCons(intern("secs"), makeNumber(runtime)))
}

/// builtin get-hooks
/// fun     bi_get_hooks
/// std     
/// key     
/// opt     
/// rest    
/// ret     alist
/// special no
/// doc {
/// Return an alist with (hooksym . function-list) pairs for all defined hooks.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_get_hooks(args: LObject, kwArgs: Map<LSymbol, LObject>,
                 suppp: Map<LSymbol, Boolean>): LObject {
    val lc = ListCollector()
    for ((hooksym, funlist) in getHooks()) {
        lc.add(LCons(hooksym, list2lisp(funlist)))
    }
    return lc.list
}

/// builtin define-hook
/// fun     bi_define_hook
/// std     hook-symbol
/// key     
/// opt     function
/// rest    
/// ret     hook-symbol
/// special no
/// doc {
/// Define a hook `hook-symbol` for later use.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_define_hook(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
    val hooksym = arg1(args)
    defineHook(symbolArg(hooksym))
    return hooksym
}

/// builtin add-hook-function
/// fun     bi_add_hook_function
/// std     hook-symbol function
/// key     
/// opt     
/// rest    
/// ret     nil
/// special no
/// doc {
/// Associate `function` with the `hook-symbol`, to be called when
/// the hook is activated.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_add_hook_function(args: LObject, kwArgs: Map<LSymbol, LObject>,
                         suppp: Map<LSymbol, Boolean>): LObject {
    val (hooksym, function) = args2(args)
    val func = functionArg(function)
    addHookFunction(symbolArg(hooksym), func)
    return Nil
}

/// builtin remove-hook-function
/// fun     bi_remove_hook_function
/// std     hook-symbol function
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Remove `function` with the `hook-symbol`, so it os no longer
/// called when the hook is activated. Return t if the function was
/// in the hook functions.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_remove_hook_function(args: LObject, kwArgs: Map<LSymbol, LObject>,
                            suppp: Map<LSymbol, Boolean>): LObject {
    val (hooksym, function) = args2(args)
    val func = functionArg(function)
    return bool2ob(removeHookFunction(symbolArg(hooksym), func))
}

/// builtin get-hook-functions
/// fun     bi_get_hook_function
/// std     hook-symbol
/// key     
/// opt     
/// rest    
/// ret     function-list
/// special no
/// doc {
/// Return the function of hook `hook-symbol` (may be nil).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_get_hook_function(args: LObject, kwArgs: Map<LSymbol, LObject>,
                         suppp: Map<LSymbol, Boolean>): LObject {
    return list2lisp(getHookFunctions(symbolArg(arg1(args))))
}

/// builtin run-hook-functions
/// fun     bi_run_hook_functions
/// std     hook-symbol
/// key     
/// opt     
/// rest    args
/// ret     t/nil
/// special no
/// doc {
/// Run the hook function of hook `hook-symbol` and return its value.
/// The `args` are passed to the function.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_run_hook_functions(args: LObject, kwArgs: Map<LSymbol, LObject>,
                         suppp: Map<LSymbol, Boolean>): LObject {
    val (hooksym, hookargs) = args
    return bool2ob(runHookFunctions(symbolArg(hooksym), hookargs))
}

/// builtin lyk-command-options
/// fun     bi_lyk_command_options
/// std     
/// key     
/// opt     
/// rest    
/// ret     alist
/// special no
/// doc {
/// Return a alist of lyk's command line options and their values.
/// The debug options will not be shown; use `(set-debug)` for those.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_lyk_command_options(args: LObject, kwArgs: Map<LSymbol, LObject>,
                           suppp: Map<LSymbol, Boolean>): LObject {
    val lc = ListCollector()
    lc.add(LCons(intern("print-estack"), bool2ob(Options.print_estack)))
    lc.add(LCons(intern("max-recurse"), makeNumber(Options.maxrecurse)))
    lc.add(LCons(intern("verbosity"),  makeNumber(Options.verbosity)))
    lc.add(LCons(intern("warn-is-error"), bool2ob(Options.warnIsError)))
    lc.add(LCons(intern("no-preload"), bool2ob(Options.noPreload)))
    lc.add(LCons(intern("show-version"), bool2ob(Options.showVersion)))
    lc.add(LCons(intern("no-user-startup"), bool2ob(Options.noUserStartup)))

    return lc.list
}







// EOF
