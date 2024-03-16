// system messages printed to the user, including debugging

package org.w21.lyk

import kotlin.system.exitProcess


fun printErr(vararg things: Any) {
    stderr.print("ERROR:")
    for (thing in things) {
        stderr.print(" " + thing)
    }
    stderr.println()
}

fun printErr(e: LispError) {
    stderr.println(e.toString())
}

fun errExit(message: String? = null) {
    if (message != null) {
        printErr(message)
    }
    exitProcess(1)
}

fun info(message: String) {
    if (Options.verbosity >= verbosityInfo) {
        stderr.println("; " + message)
    }
}

fun notice(message: String) {
    if (Options.verbosity >= verbosityNotice) {
        stderr.println("; " + message)
    }
}

fun warn(warning: String) {
    notice("WARNING " + warning)
}

//
fun anyDebugOn(): Boolean {
    for (value in Options.debug.values) {
        if (value) return true
    }
    return false
}

fun setDebug(sym: LSymbol, value: Boolean = true): Boolean { // false if error
    debug(debugDebugSym) {
        "set $sym <= $value"
    }
    if (sym in Options.debug.keys) {
        Options.debug[sym] = value
        debugOn = anyDebugOn()
        return true
    }
    return false
}

fun setDebug(vararg syms: LSymbol, value: Boolean = true): Boolean {
    for (sym in syms) {
        val result = setDebug(sym, value)
        if (!result) {
            return false
        }
    }
    return true
}

fun setDebug(options: String?): Boolean {
    options ?: return false
    var isgood = true
    for (option in options.split(',')) {
        val sym = intern(option)
        if (!setDebug(sym)) {
            isgood = false
            printErr("`$option` is not a valid debug option")
        }
    }
    return isgood
}


fun debug(topic: LSymbol, closure: () -> Any?) {
    if (debugOn && Options.debug[topic] ?: false) {
        debug_out.print("DBG:$topic ")
        val value = closure()
        if (value != null) {
            debug_out.println(value.toString())
        }
    }
}

