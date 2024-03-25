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

fun typeOf(obj: Any) =
    when (obj) {
        is LEnv ->  "Environment"
        is Lambda -> "Lambda"
        else -> {
            val typ = "${obj::class.simpleName}"
            if (typ.startsWith("L")) {
                typ.substring(1)
            } else {
                typ
            }
        }
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

    fun join(separator: String = " ",
             prefix: String = "",
             postfix: String = ""): String {
        return buf.joinToString(separator = separator,
                                prefix = prefix,
                                postfix = postfix)
    }
}

fun glob2regexp(glob_pattern: String): Regex {
    val sb = StrBuf()
    var had_quote = false               // state: after backslash escape
    var in_cclass = false               // state: in character class
    
    sb.add("^")
    for (ch in glob_pattern) {
        var char = ch
        if (had_quote) {
            sb.add(char)
            had_quote = false
            continue
        }
        when (char) {
            '\\' -> {
                had_quote = true
            }
            '[' -> in_cclass = true
            ']' -> in_cclass = false
            '!' -> if (in_cclass) char = '^'
            '*' -> if (!in_cclass) sb.add('.')
            '?' -> if (!in_cclass) char = '.'
            '.', '|', '+', ')', '(', '^', '$' -> sb.add('\\')
            
        }
        sb.add(char)
    }
    sb.add("$")
    return Regex(sb.toString())
}

fun arrayIntern(array: Array<String>): List<LSymbol> {
    val symbols = mutableListOf<LSymbol>()
    for (elem in array) {
        symbols.add(intern(elem))
    }
    return symbols
}

fun mapInternKeys(map: Map<String, LObject>): Map<LSymbol, LObject> {
    val result = mutableMapOf<LSymbol, LObject>()
    for ((key, value) in map) {
        result[intern(":" + key)] = value
    }
    return result
}

fun pairsInternFirst(pairs: Array<Pair<String, LObject>>
): List<Pair<LSymbol, LObject>> {
    var result = mutableListOf<Pair<LSymbol, LObject>>()
    for ((key, value) in pairs) {
        result.add(Pair<LSymbol, LObject>(intern(key), value))
    }
    return result
}

// Measure performance data while executing the passed closure. The returned
// value is a Pair of the string with the performance data and the returned
// value.
fun measurePerfdataValue(closure: () -> LObject): Pair<String, LObject> {
    var result: LObject = Nil

    val perfdata = measurePerfdata {
        result = closure()
    }
    return Pair(perfdata, result)
}

// Measure performance data while executing the passed closure. The returned
// value is a string with the performance data.
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
