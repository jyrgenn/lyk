// diverse utility functions

package org.w21.lyk

import kotlin.time.measureTime
import kotlin.text.CharCategory.*

val printables = setOf(
    kotlin.text.CharCategory.UPPERCASE_LETTER,
    kotlin.text.CharCategory.LOWERCASE_LETTER,
    kotlin.text.CharCategory.TITLECASE_LETTER,
    kotlin.text.CharCategory.MODIFIER_LETTER,
    kotlin.text.CharCategory.OTHER_LETTER,
    kotlin.text.CharCategory.DECIMAL_DIGIT_NUMBER,
    kotlin.text.CharCategory.LETTER_NUMBER,
    kotlin.text.CharCategory.OTHER_NUMBER,
    kotlin.text.CharCategory.DASH_PUNCTUATION,
    kotlin.text.CharCategory.START_PUNCTUATION,
    kotlin.text.CharCategory.END_PUNCTUATION,
    kotlin.text.CharCategory.CONNECTOR_PUNCTUATION,
    kotlin.text.CharCategory.OTHER_PUNCTUATION,
    kotlin.text.CharCategory.MATH_SYMBOL,
    kotlin.text.CharCategory.CURRENCY_SYMBOL,
    kotlin.text.CharCategory.MODIFIER_SYMBOL,
    kotlin.text.CharCategory.OTHER_SYMBOL,
    kotlin.text.CharCategory.INITIAL_QUOTE_PUNCTUATION,
    kotlin.text.CharCategory.FINAL_QUOTE_PUNCTUATION,
)

fun isPrintable(ch: Char): Boolean {
    return ch.category in printables
}

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
        is LEnv ->  "environment"
        is LFunction -> "function"
        is LispError -> "Error"
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

    val size: Int
        get () = chars.size
    
    override fun toString(): String {
        return chars.joinToString(separator = "")
    }
}

class StrBuf() {
    val buf = mutableListOf<String>()

    constructor(vararg inits: Any) : this() {
        for (s in inits) {
            add(s)
        }
    }

    fun add(thing: Any) {
        buf.add(thing.toString())
    }
    
    override fun toString(): String {
        return buf.joinToString(separator = "")
    }

    val size: Int
        get() = buf.size

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

// Of a string => object map, intern the strings as symbols and return
// the result as a new map.
fun mapInternKeys(map: Map<String, LObject>): Map<LSymbol, LObject> {
    val result = mutableMapOf<LSymbol, LObject>()
    for ((key, value) in map) {
        result[intern(":" + key)] = value
    }
    return result
}

// Of an array of string, object pairs, intern the strings as symbols and return
// the result as a list.
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

// Return the smaller of the two numbers.
fun min(i1: Int, i2: Int) =
    if (i1 < i2) i1 else i2

// Return the bigger of the two numbers.
fun max(i1: Int, i2: Int) =
    if (i1 > i2) i1 else i2


// Return the string `s` abbreviated to `width` if it is longer, otherwise
// return the string. Optional `suffix` will be placed at the end of the
// abbreviated string the show the string was abbreviated.
fun abbreviate(s: String, width: Int, suffix: String = ""): String {
    if (s.length <= width) {
        return s
    }
    return s.substring(0, min(s.length, width - suffix.length)) + suffix
}


// print the eval stack frames accumulated in `error` -- the first line shows
// the depth of the eval recursion, the approximate location of the code, and
// the environment of that frame; the second line shows the expression being
// evaluated
fun printEvalStack(error: LispError) {
    val width = getTermWidth()
    val abbr = ob2bool(evalStackAbbrLines.getValueOptional() ?: T)
    
    for (frame in error.evalStack) {
        val (level, expr, env, location) = frame as LVector
        val frameno = "#%d".format((level as LNumber).toInt())

        // print frame number, location, environment line
        var line = StrBuf(frameno, location, env.desc()).join()
        if (abbr) line = abbreviate(line, width, "[…]")
        stderr.println(line)

        // print the expression line; indent it as far as the frame number in
        // the line above is wide
        line = StrBuf(mulString(" ", frameno.length), expr).join()
        if (abbr) line = abbreviate(line, width, "[…]")
        stderr.println(line)
    }
}
