
package org.w21.lyk


val traceEvalSym = Symbol.intern("eval")
val traceEvalFunSym = Symbol.intern("evalfun")
val traceCallSym = Symbol.intern("call")
val debugErrorSym = Symbol.intern("error")

object Options {
    var debug = mutableMapOf<Symbol, Boolean>(
        traceEvalSym to false,
        traceCallSym to false,
        debugErrorSym to false,
        traceEvalFunSym to false,
    )
    var warn = true
    var print_estack = false
}



fun main(args: Array<String>) {
    val argl = mutableListOf<String>(*args)
    val inOptions = true
    
    while (argl.size > 0 && inOptions) {
        val arg = argl.removeFirst()
        if (!arg.startsWith("-") || arg == "--") {
            break
        }
        for (ch in arg.substring(1)) {
            when (ch) {
                'E' -> { Options.print_estack = true }
                'd' -> { setDebug(argl.removeFirst()); }
                else -> { println("unknown option $ch") }
            }
        }
    }

    println(";; this is lyk")
    init_Builtins()    
    repl()
}
