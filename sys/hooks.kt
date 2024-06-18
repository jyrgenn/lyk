// Hooks -- where the interpreter core calls Lisp functions

package org.w21.lyk


// per hook we have a list of Lisp function to be called
val hookFunctions = mutableMapOf<LSymbol, MutableList<LFunction>>()

// Return an with (hooksym . function-list) pairs
fun getHooks() = hookFunctions.entries


fun defineHook(hookSymbol: LSymbol) {
    debug(debugHooksSym) { "define hook $hookSymbol" }
    if (hookSymbol in hookFunctions.keys) {
        throw ArgumentError("hook $hookSymbol already defined")
    }
    hookFunctions.put(hookSymbol, mutableListOf<LFunction>())
}


// Add a Lisp function to the function list of hookSymbol.
fun addHookFunction(hookSymbol: LSymbol, function: LFunction) {
    debug(debugHooksSym) { "add $hookSymbol function $function" }
    val funlist = hookFunctions[hookSymbol]
    if (funlist != null) {
        funlist.add(function)
    } else {
        throw ArgumentError("cannot add function to unknown hook $hookSymbol")
    }
}

// Add a Lisp function to the function list of hookSymbol.
fun removeHookFunction(hookSymbol: LSymbol, function: LFunction): Boolean {
    debug(debugHooksSym) { "remove $hookSymbol function $function" }
    val funlist = hookFunctions[hookSymbol]
    if (funlist != null) {
        return funlist.remove(function)
    } else {
        throw ArgumentError("cannot remove function from unknown"
                            + " hook $hookSymbol")
    }
}


fun getHookFunctions(hookSymbol: LSymbol): List<LFunction> {
    val funlist = hookFunctions[hookSymbol]
    if (funlist != null) {
        debug(debugHooksSym) { "get $hookSymbol functions: $funlist" }
        return funlist
    }
    throw ArgumentError("cannot get functions of unknown hook $hookSymbol")
}

/// args is an argument list for the function
fun runHookFunctions(hookSymbol: LSymbol, args: LObject): Boolean {
    val funlist = hookFunctions[hookSymbol]
    if (funlist != null) {
        debug(debugHooksSym) { "run $hookSymbol functions: $funlist" }
        var called_one = false
        for (func in funlist) {
            called_one = true
            func.call(args)
        }
        return called_one
    }
    throw ArgumentError("cannot run functions of unknown hook $hookSymbol")
}
