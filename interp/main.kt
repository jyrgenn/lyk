
import org.w21.lyk.*

object Options {
    var debug = false
    var warn = true
    var print_estack = false
}


fun debug(msg: String, vararg args: Any) {
    if (Options.debug) {
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
                'd' -> { Options.debug = true }
                'e'-> { Options.print_estack }
                else -> { println("unknown option $ch") }
            }
        }
    }

    println(";; this is lyk")
    init_Builtins()    
    repl()
}
