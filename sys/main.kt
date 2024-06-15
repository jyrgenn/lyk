
package org.w21.lyk


import sun.misc.Signal
import sun.misc.SignalHandler

import java.util.Locale


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
        debugReplSym to false,
        debugReadSymSym to false,
        debugStopSym to false,
        debugSetqSym to false,
        debugFormatstringSym to false,
        debugEnglishSym to false,
        debugHooksSym to false,
        debugCompleterSym to false
    )
    var print_estack = false
    var maxrecurse = 0
    var verbosity = 2           // 0: errors-only, 1: notice, 2: info
    var warnIsError = false     // let warnings be errors
    var noPreload = false       // don't load preload code
    var showVersion = false
    var noUserStartup = false   // don't load user startup file
    var debug_out: String? = null // file path to print debug output to
}
val debugOptions = Options.debug.keys.sorted()


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
  -N               : don't load preload Lisp code
  -V               : show version information and exit
  -W               : let warnings be errors
""")
    // -R maxrecurse    : maximum eval recursion depth 

    println("Available debug options:")
    printThingsWrapped(debugOptions.joinToString(", ").split(" "),
                       prefix = "   ")
    println()
    exitLyk(0)
}

val startupHookSym = intern("*startup-hook*")


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
                    'N' -> { Options.noPreload = true }
                    'n' -> { Options.noUserStartup = true }
                    'E' -> { Options.print_estack = true }
                    'q' -> { Options.verbosity -= 1 }
                    'v' -> { Options.verbosity += 1 }
                    'V' -> { Options.showVersion = true }
                    'd' -> { setDebug(getOptVal("debug options")) }
                    'D' -> { Options.debug_out = getOptVal("debug output") }
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
                    'W' -> {
                        Options.warnIsError = true
                        warningsAsErrors.setROValue(T)
                    }
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

        if (Options.showVersion) {
            lispExpression = "(build-info t)"
        }
        if (lispExpression != null || argl.size > 0) {
            // i.e. running non-interactively, so no buildtag printing etc.
            Options.verbosity -= 1
        }

        // at least try to handle an interrupt signal, even if it seem to work
        // only sporadically
        Signal.handle(Signal("INT"),
                      object : SignalHandler {
                          override fun handle(sig: Signal) {
                              abortEval = true
                          }
                      })
        // no weird number formatting and things
        Locale.setDefault(Locale.Builder().setLanguage("en").build())

        // now start the machine!
        info(buildtag())
        debug(debugStartupSym) { "init builtin functions" }
        init_Builtins()
        debug(debugStartupSym) { "init variables" }
	init_Variables()
        debug(debugStartupSym) { "init streams" }
        init_Streams()
        debug(debugStartupSym) { "init repl" }
        init_repl()
        defineHook(startupHookSym)
        if (!Options.noPreload) {
            debug(debugStartupSym) { "load preload code" }
            load_string(preload_code, "*preload-code*")
        }
        
        val args_lc = ListCollector()
        for (arg in argl) {
            args_lc.add(makeString(arg))
        }
        runHookFunction(startupHookSym,
                        list(collectedList {
                                 for (load_file in load_files) {
                                     it.add(makeString(load_file))
                                 }
                             },
                             if (lispExpression == null) {
                                 Nil
                             } else {
                                 list(makeString(lispExpression))
                             },
                             args_lc.list))                             

        for (fname in load_files) {
            debug(debugStartupSym) { "load file \"$fname\"" }
            load(fname)
        }

        debug(debugStartupSym) { "found command line args: ${args_lc.list}" }
        
        
        if (lispExpression != null) {
            // in this case, all command line args are in *command-line-args*
            commandLineArgs.setROValue(args_lc.list)
            debug(debugStartupSym) { "eval -e argument \"$lispExpression\"" }
            val result = load_string(lispExpression, "*cmd-expr*",
                                     true, false, true)
            exitLyk(if (result === Nil) 1 else 0)
        }
        if (args_lc.list !== Nil) {
            // first argument is the file to run, rest goes to
            // *command-line-args*"
            val file = (args_lc.list.car as LString).the_string
            commandLineArgs.setROValue(args_lc.list.cdr)

            try {
                debug(debugStartupSym) { "run load file \"$file\"" }
                load(file)
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
            info("type :help to get information on short commands")
            repl(Reader(ConsoleReaderStream()), true)
        }
        exitLyk()
    } catch (e: LispError) {
        if (Options.print_estack) {
            printEvalStack(e)
        }
        errExit(e)
    } catch (e: Exception) {
        if (Options.print_estack) {
            e.printStackTrace()
        }
        errExit(e)
    }
}
