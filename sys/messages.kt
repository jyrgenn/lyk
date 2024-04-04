// system messages printed to the user, including debugging

package org.w21.lyk

import kotlin.system.exitProcess


fun printErr(vararg things: Any) {
    if (things[0] !is LispError) {
        stderr.print("Error: ")
    }
    stderr.print(things.joinToString(" "))
    stderr.println()
}

fun printErr(e: Exception) {
    stderr.println(e.toString())
}

fun errExit(message: Any? = null) {
    if (message != null) {
        printErr(message)
    }
    exitProcess(1)
}

fun info(message: String) {
    if (Options.verbosity >= verbosityInfo) {
        stderr.println(";", message)
    }
}

fun notice(message: String) {
    if (Options.verbosity >= verbosityNotice) {
        stderr.println(";", message)
    }
}

fun warn(warning: String) {
    if (Options.warnIsError) {
        throw WarningError(warning)
    }
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
            debug_out.println(value)
        }
    }
}

