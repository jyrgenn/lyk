// Debugging stuff

package org.w21.lyk

fun setDebug(sym: Symbol, value: Boolean = true): Boolean { // false if error
    if (sym in Options.debug.keys) {
        Options.debug[sym] = value
        return true
    }
    return false
}

fun setDebug(vararg syms: Symbol, value: Boolean = true): Boolean {
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
        val sym = Symbol.intern(option)
        if (!setDebug(sym)) {
            isgood = false
            printErr("`$option` is not a valid debug option")
        }
    }
    return isgood
}

fun clearDebug(sym: Symbol) {
    Options.debug[sym] = false
}


fun debug(topic: Symbol, closure: () -> Any) {
    if (Options.debug[topic] ?: false) {
        debug_out.println("DBG:$topic " + closure().toString())
    }
}

