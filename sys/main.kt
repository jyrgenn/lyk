
package org.w21.lyk


val verbosityNotice = 1
val verbosityInfo = 2

object Options {
    var debug = mutableMapOf<LSymbol, Boolean>(
        debugEvalSym to false,
        debugCallSym to false,
        debugErrorSym to false,
        debugDebugSym to false,
        debugEvalFunSym to false,
	debugCatchThrowSym to false,
	debugBindSymSym to false,
	debugLetBindSym to false,
        debugIOSym to false,
        debugBindParSym to false,
        debugReaderSym to false,
        debugConsSym to false,
        debugStepEval to false,
        debugMacroSym to false,
        debugLambdaParamsSym to false,
        debugPreloadSym to false,
        debugFormatSym to false,
        debugDefunSym to false,
        debugAtexitSym to false,
        debugFinalizeSym to false,
        debugLoadlineSym to false,
        debugReadCharSym to false,
        debugStartupSym to false,
    )
    var print_estack = false
    var maxrecurse = 0
    var verbosity = 2           // 0: errors-only, 1: notice, 2: info
    var warnIsError = false     // let warnings be errors
}


fun usage() {
    println(buildtag())
    println("\nUsage: lyk [-Ehq?] [-d debug-options] [-e expression] [-l load-file] [file [arg1 ...]]")

    println("""
  -d debug-options : set debug options, comma separated, see below
  -e expression    : evaluate Lisp expression, print result, and exit
  -l load-file     : load the named Lisp files (multiple possible)
  -h, -?           : print this help on options
  -q               : suppress info messages (-qq: also notice/warning)
  -v               : increase verbosity
  -E               : print exception stack
  -W               : let warnings be errors
""")
    // -R maxrecurse    : maximum eval recursion depth 

    print("Available debug options:")
    for (opt in Options.debug.keys.sorted()) {
	print(" $opt")
    }
    println()
    exitLyk(0)
}


fun main(args: Array<String>) {
    try {
        val argl = mutableListOf<String>(*args)
        val load_files = mutableListOf<String>()
        var lispExpression: String? = null
        var opterrors = false
        debug(debugStartupSym) { "beginning startup" }
        
        fun getOptVal(what: String): String? {
            try {
                return argl.removeFirst()
            } catch (_: Exception) {
                printErr("no value for $what option")
                opterrors = true
            }
            return null
        }
        
        fun getOptValInt(what: String): Int? {
            val optstr = getOptVal(what) ?: return null
            try {
                return optstr.toInt()
            } catch (_: Exception) {
                printErr("value for $what option not an integer")
                opterrors = true
            }
            return null
        }

        while (argl.size > 0) {
            if (!argl[0].startsWith("-")) {
                break
            }
            val arg = argl.removeFirst()
            if (!arg.startsWith("-") || arg == "--") {
                break
            }

            for (ch in arg.substring(1)) {
                when (ch) {
                    'E' -> { Options.print_estack = true }
                    'q' -> { Options.verbosity -= 1 }
                    'v' -> { Options.verbosity += 1 }
                    'd' -> { setDebug(getOptVal("debug options")) }
                    'e' -> { lispExpression = getOptVal("Lisp expression") }
                    'l' -> {
                        val fname = getOptVal("load file")
                        if (fname != null) {
                            load_files.add(fname)
                        }
                    }
                    'h', '?' -> { usage() }
                    'R' -> {
                        Options.maxrecurse = getOptValInt("maxrecurse") ?: 0
                    }
                    'W' -> { Options.warnIsError = true }
                    else -> {
                        printErr("unknown option `-$ch`")
                        opterrors = true
                    }
                }
            }
            if (opterrors) {
                errExit()
            }
        }
        debug(debugStartupSym) { "have options: $Options" }

        if (lispExpression != null || argl.size > 0) {
            // i.e. running non-interactively, so no buildtag printing etc.
            Options.verbosity -= 1
        }
        // now start the machine!
        info(buildtag())
        debug(debugStartupSym) { "init builtin functions" }
        init_Builtins()
        debug(debugStartupSym) { "init variables" }
	init_Variables()
        debug(debugStartupSym) { "init streams" }
        init_Streams()
        debug(debugStartupSym) { "load preload code" }
        load_string(preload_code, "*preload-code*")
        
        for (fname in load_files) {
            debug(debugStartupSym) { "load file \"$fname\"" }
            load_file(fname)
        }

        val args_lc = ListCollector()
        for (arg in argl) {
            args_lc.add(makeString(arg))
        }
        debug(debugStartupSym) { "found command line args: ${args_lc.list}" }
        
        
        if (lispExpression != null) {
            // in this case, all command line args are in *command-line-args*
            commandLineArgs.setValue(args_lc.list)
            debug(debugStartupSym) { "eval -e argument \"$lispExpression\"" }
            try {
                val reader = Reader(StringReaderStream(lispExpression),
                                    "*command-arg*")
                val (expr, where) = reader.read()
                if (expr == null) {
                    throw EOFError("$where: no Lisp expression to eval")
                }
                var result: LObject = Nil
                val perfdata = measurePerfdata {
                    result = eval(expr)
                }
                stdout.println(result)
                info(perfdata)
            } catch (e: LispError) {
                printEvalStack(e)
                errExit(e)
            }
            exitLyk(0)
        }
        if (args_lc.list !== Nil) {
            // first argument is the file to run, rest goes to
            // *command-line-args*"
            val file = (args_lc.list.car as LString).the_string
            commandLineArgs.setValue(args_lc.list.cdr)

            try {
                debug(debugStartupSym) { "run load file \"$file\"" }
                load_file(file)
            } catch (e: LispError) {
                printEvalStack(e)
            } catch (e: Exception) {
                if (Options.print_estack) {
                    e.printStackTrace()
                } else {
                    printErr(e)
                }
            }
        } else {
            debug(debugStartupSym) { "start REPL" }
            repl(Reader(ConsoleReaderStream()), "> ")
        }
        exitLyk()
    } catch (e: Exception) {
        if (Options.print_estack) {
            e.printStackTrace()
        }
        errExit(e)
    }
}
