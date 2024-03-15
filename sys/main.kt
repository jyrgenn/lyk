
package org.w21.lyk

import kotlin.system.exitProcess

val verbosityNotice = 1
val verbosityInfo = 2

object Options {
    var debug = mutableMapOf<Symbol, Boolean>(
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
    )
    var print_estack = false
    var maxrecurse = 0
    var verbosity = 2           // 0: errors-only, 1: notice, 2: info
}


fun usage() {
    println(buildtag())
    println("\nUsage: lyk [-Ehq?] [-d debug-options] [-e expression] [-R maxrecurse]")

    println("""
    -d debug-options : set debug options, comma separated, see below
    -e expression    : evaluate Lisp expression, print result, and exit
    -l load-file     : load the named Lisp files (multiple possible)
    -h, -?           : print this help on options
    -q               : suppress info messages (-qq: also notice/warning)
    -E               : print exception stack
    -R maxrecurse    : maximum eval recursion depth 
""")
    print("Available debug options:")
    for (opt in Options.debug.keys.sorted()) {
	print(" $opt")
    }
    println()
    exitProcess(0)
}


fun main(args: Array<String>) {
    val argl = mutableListOf<String>(*args)
    val load_files = mutableListOf<String>()
    var lispExpression: String? = null
    var opterrors = false
    
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
        val arg = argl.removeFirst()
        if (!arg.startsWith("-") || arg == "--") {
            break
        }

        for (ch in arg.substring(1)) {
            when (ch) {
                'E' -> { Options.print_estack = true }
                'q' -> { Options.verbosity -= 1 }
                'd' -> { setDebug(getOptVal("debug options")) }
                'e' -> { lispExpression = getOptVal("Lisp expression") }
                'l' -> {
                    val fname = getOptVal("load file")
                    if (fname != null) {
                        load_files.add(fname)
                    }
                }
                'h', '?' -> { usage() }
                'R' -> { Options.maxrecurse = getOptValInt("maxrecurse") ?: 0 }
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

    init_Builtins()

    val lc = ListCollector()
    for (arg in argl) {
        lc.add(LispString(arg))
    }
    commandLineArgs.setValue(lc.list())
    
    for (file in load_files) {
        try {
            load_file(file)
        } catch (e: Exception) {
            errExit(e.toString())
        }
    }

    if (lispExpression != null) {
        try {
            val reader = Reader(StringReaderStream(lispExpression),
                                "*command-arg*")
            val expr = reader.read()
            if (expr == null) {
                throw EOFError("no Lisp expression to eval")
            }
            val perfdata = measurePerfdata {
                println(eval(expr))
            }
            info(perfdata)
        } catch (e: Exception) {
            errExit(e.toString())
        }
        exitProcess(0)
    }
    notice(buildtag())
    repl(Reader(stdin), "> ")
}
