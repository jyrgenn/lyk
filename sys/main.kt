
package org.w21.lyk

import kotlin.system.exitProcess

val traceEvalSym = Symbol.intern("eval")
val traceEvalFunSym = Symbol.intern("evalfun")
val traceCallSym = Symbol.intern("call")
val debugErrorSym = Symbol.intern("error")
val debugIOSym = Symbol.intern("io")
val debugBindParSym = Symbol.intern("bindpar")
val debugReaderSym = Symbol.intern("reader")

object Options {
    var debug = mutableMapOf<Symbol, Boolean>(
        traceEvalSym to false,
        traceCallSym to false,
        debugErrorSym to false,
        debugDebugSym to false,
        traceEvalFunSym to false,
	catchThrowSym to false,
	bindSymSym to false,
	letBindSym to false,
        debugIOSym to false,
        debugBindParSym to false,
        debugReaderSym to false,
    )
    var warnings = true
    var print_estack = false
    var maxrecurse = 0
}


fun usage() {
    println(buildtag())
    println("\nUsage: lyk [-EWh?] [-d debug-options] [-e expression] [-R maxrecurse]")

    println("""
    -d debug-options : set debug options, comma separated, see below
    -e expression    : evaluate Lisp expression, print result, and exit
    -l load-file     : load the named Lisp files (multiple possible)
    -h, -?           : print this help on options
    -q               : suppress warnings
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
                'q' -> { Options.warnings = false }
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
    
    for (file in load_files) {
        try {
            load_file(file)
        } catch (e: Exception) {
            errExit(e.toString())
        }
    }

    if (lispExpression != null) {
        try {
            println("-e arg: $lispExpression")
            val stream = StringReaderStream(lispExpression,
                                            "\"$lispExpression\"")
            println("stream: $stream")
            val reader = Reader(StringReaderStream(lispExpression),
                                "*command-arg*")
            println("reader: $reader")
            val expr = reader.read()
            println("expr: $expr")
            if (expr == null) {
                throw EOFError("no Lisp expression to eval")
            }
            println(eval(expr))
        } catch (e: Exception) {
            errExit(e.toString())
        }
        exitProcess(0)
    }
    warn(buildtag())
    repl(Reader(stdin), "> ")
}
