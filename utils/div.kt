package org.w21.lyk

import kotlin.system.exitProcess

fun padString(s: String, width: Int, pad: Char = ' '): String {
    val len = s.length
    return s.padEnd(width - len, pad).toString()
}

fun typeOf(obj: Any): String {
    if (obj is LispString) {
        return "string"
    }
    return "${obj::class.simpleName}"//.lowercase()
}


class CharBuf {
    val chars = mutableListOf<Char>()

    constructor() {
        
    }
    constructor(ch: Char) {
        chars.add(ch)
    }

    fun add(ch: Char) {
        chars.add(ch)
    }
    
    override fun toString(): String {
        return chars.joinToString(separator = "")
    }
}

class StrBuf() {
    val buf = mutableListOf<String>()

    constructor(s: String) : this() {
        buf.add(s)
    }
    fun add(ch: Char) {
        buf.add(ch.toString())
    }
    fun add(s: String) {
        buf.add(s)
    }
    
    override fun toString(): String {
        return buf.joinToString(separator = "")
    }

    fun join(separator: String): String {
        return buf.joinToString(separator = separator)
    }
}

fun arrayIntern(array: Array<String>): List<Symbol> {
    val symbols = mutableListOf<Symbol>()
    for (elem in array) {
        symbols.add(Symbol.intern(elem))
    }
    return symbols
}

fun mapInternKeys(map: Map<String, LispObject>): Map<Symbol, LispObject> {
    val result = mutableMapOf<Symbol, LispObject>()
    for ((key, value) in map) {
        result[Symbol.intern(key)] = value
    }
    return result
}

fun pairsInternFirst(pairs: Array<Pair<String, LispObject>>
): List<Pair<Symbol, LispObject>> {
    var result = mutableListOf<Pair<Symbol, LispObject>>()
    for ((key, value) in pairs) {
        result.add(Pair<Symbol, LispObject>(Symbol.intern(key), value))
    }
    return result
}

fun printErr(vararg things: Any) {
    System.err.print("Error:")
    for (thing in things) {
        System.err.print(" " + thing)
    }
    System.err.println()
}

fun printErr(e: LispError) {
    System.err.println(e)
}

fun errExit(message: String? = null) {
    if (message != null) {
        printErr(message)
    }
    exitProcess(1)
}

fun warn(warning: String) {
    if (Options.warnings) {
        System.err.println(";; " + warning )
    }
}
