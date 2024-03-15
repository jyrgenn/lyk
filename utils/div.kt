// diverse utility functions

package org.w21.lyk

import kotlin.time.measureTime
import kotlin.system.exitProcess

fun mulString(s: String, n: Int): String {
    val sb = StringBuilder()

    for (a in 1..n) {
        sb.append(s)
    }
    return sb.toString()
}

fun padString(s: String, width: Int, pad: Char = ' '): String {
    val result = s.padEnd(width, pad).toString()
    return result
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
        result[Symbol.intern(":" + key)] = value
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

// Measure performance data while executing the passed closure. The returned
// value is a Pair of the string with the performance data and the returned
// value.
fun measurePerfdata(closure: () -> Unit): String {
    val consCountBefore = consCounter
    val evalCountBefore = evalCounter

    val timeTaken = measureTime {
        closure()
    }
    val conses = consCounter - consCountBefore
    val evals  = evalCounter - evalCountBefore
    val millis = timeTaken.inWholeMilliseconds
    val eval_s =
        (evals.toDouble() / timeTaken.inWholeMicroseconds * 1000000).toLong()

    // 347 pairs 558 evals in 0 ms, 844974 evals/s
    return "$conses conses $evals evals in $millis ms, $eval_s evals/s"
}
