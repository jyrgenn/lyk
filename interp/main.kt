
import org.w21.lyk.*


val traceEvalSym = Symbol.intern("eval")
val traceCallSym = Symbol.intern("call")
val traceArgsSym = Symbol.intern("args")

object Options {
    var debug = mutableMapOf<Symbol, Boolean>(
        traceEvalSym to false,
        traceCallSym to false,
    )
    var warn = true
    var print_estack = false
}

fun debug(topic: Symbol, closure: () -> Unit) {
    if (Options.debug[topic] ?: false) {
        closure()
    }
}

fun debug(topic: Symbol, vararg args: LispObject) {
    if (Options.debug[topic] ?: false) {
        print("DBG")
        for (arg in args) {
            print(" ")
            print(arg)
        }
        println()
    }
}

fun debug(topic: Symbol, msg: String, vararg args: Any) {
    if (Options.debug[topic] ?: false) {
        println("DBG $msg ${args}")
    }
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
                else -> { println("unknown option $ch") }
            }
        }
    }

    println(";; this is lyk")
    init_Builtins()    
    repl()
}
