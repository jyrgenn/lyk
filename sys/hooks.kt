// Hooks -- where the interpreter core calls Lisp functions

package org.w21.lyk


val hookFunctions = mutableMapOf<LSymbol, LFunction?>()

// Return an alist with (hooksym . function) pairs
fun getHooks() = hookFunctions.entries


fun defineHook(hookSymbol: LSymbol) {
    debug(debugHooksSym) { "define hook $hookSymbol" }
    if (hookSymbol in hookFunctions.keys) {
        throw ArgumentError("hook $hookSymbol already defined")
    }
    hookFunctions.put(hookSymbol, null)
}


// Associate a Lisp function to the hookSymbol. If `function` is null, remove an
// existing hook.
fun setHookFunction(hookSymbol: LSymbol, function: LFunction?) {
    debug(debugHooksSym) { "set $hookSymbol function $function" }
    if (hookSymbol in hookFunctions.keys) {
        hookFunctions.put(hookSymbol, function)
    } else {
        val what = if (function == null) "remove" else "set function for"
        throw ArgumentError("cannot $what unknown hook $hookSymbol")
    }
}


fun getHookFunction(hookSymbol: LSymbol): LFunction? {
    if (hookSymbol in hookFunctions.keys) {
        val func = hookFunctions.get(hookSymbol)
        debug(debugHooksSym) { "get $hookSymbol function: $func" }
    }
    throw ArgumentError("cannot get function for unknown hook $hookSymbol")
}

/// args is an argument list for the function
fun runHookFunction(hookSymbol: LSymbol, args: LObject): LObject {
    if (hookSymbol in hookFunctions.keys) {
        val func = hookFunctions.get(hookSymbol)
        debug(debugHooksSym) { "run $hookSymbol function $func" }
        if (func != null) {
            return func.call(args)
        }
        return Nil
    }
    throw ArgumentError("cannot run function for unknown hook $hookSymbol")
}
