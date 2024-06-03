// diverse utility functions

package org.w21.lyk

import kotlin.math.round
import kotlin.time.Duration
import kotlin.time.DurationUnit
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

// powers of Long numbers; does not throw an exception on overflow!
fun pow(base: Long, exp: Int): Long {
    var value = 1L
    for (i in 1..exp) {
        value *= base
    }
    return value
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

// Return true iff `needle` is in `haystack` at `index`
fun lookingAt(haystack: CharSequence, index: Int, needle: CharSequence): Boolean
{
    for (i in 0..< min(needle.length, haystack.length - index)) {
        if (haystack.get(index + i) != needle.get(i)) {
            return false
        }
    }
    return true
}

// Return the characters in `chars` between `index` and the next newline
// character
fun restOfLine(chars: CharSequence, index: Int): String {
    val cb = CharBuf()
    var i = index

    while (i < chars.length) {
        val ch = chars.get(i++)
        if (ch == '\n') {
            break
        }
        cb.add(ch)
    }
    return cb.toString()
}

// convert camelCase name to lisp-style-name
fun lispifyName(name: String): String {
    var first = true
    val cb = CharBuf()

    for (ch in name) {
        if (ch.isUpperCase()) {
            if (!first) {
                cb.add('-')
            }
            cb.add(ch.lowercase()[0])
        } else if (ch == '_') {
            cb.add('-')
        } else {
            cb.add(ch)
        }
        first = false
    }
    return cb.toString()
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
    
    override fun toString() = buf.joinToString(separator = "")

    fun desc() = "#<StrBuf[" + join("|") + "]>"

    val size: Int
        get() = toString().length

    fun join(separator: String = " ",
             prefix: String = "",
             postfix: String = "") =
        buf.joinToString(separator = separator,
                         prefix = prefix,
                         postfix = postfix)
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

val dense_float_format = "%.3g"
val float_regexp = Regex("e+0?")

fun dense_format(value: Long): String {
    if (value > 999999) {
        return dense_float_format.format(value.toDouble())
            .replace(float_regexp, "e")
    }
    return value.toString()
}

fun dense_format(value: Double): String {
    return dense_float_format.format(value).replace(float_regexp, "e")
}

data class Perfdata(val calls: Long, val conses: Long, val evals: Long,
                    val seconds: Double) {
    fun desc(): String {
        val ca_f = dense_format(calls)
        val co_f = dense_format(conses)
        val ev_f = dense_format(evals)
        val eps  = dense_format(evals.toDouble() / seconds)
        val se_f = dense_format(seconds)
        return "%s call %s cons %s eval %s s %s eval/s".format(
            ca_f, co_f, ev_f, se_f, eps).replace("e+0", "e")
    }
}

// Measure performance data while executing the passed closure. The returned
// value is a Pair of the string with the performance data and the returned
// value.
fun measurePerfdataValue(closure: () -> LObject): Pair<Perfdata, LObject> {
    var result: LObject = Nil

    val perfdata = measurePerfdata {
        result = closure()
    }
    return Pair(perfdata, result)
}

// Measure performance data while executing the passed closure. The returned
// value is a Perfdata object with the performance data.
fun measurePerfdata(closure: () -> Unit): Perfdata {
    val consCountBefore = consCounter
    val evalCountBefore = evalCounter
    val callCountBefore = callCounter

    val time = measureTime {
        closure()
    }
    val conses = consCounter - consCountBefore
    val evals  = evalCounter - evalCountBefore
    val calls  = callCounter - callCountBefore
    return Perfdata(calls, conses, evals, time.toDouble(DurationUnit.SECONDS))
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
    val abbr = (evalStackAbbrLines.getValueOptional() ?: T).toBoolean()
    
    for (frame in error.evalStack) {
        val (level, expr, env, location) = frame as LVector
        val frameno = "#%d".format((level as LNumber).toInt())

        // print frame number, location, environment line
        var line = StrBuf(frameno, location, env.desc(null)).join()
        if (abbr) line = abbreviate(line, width, "[…]")
        stderr.println(line)

        // print the expression line; indent it as far as the frame number in
        // the line above is wide
        line = StrBuf(mulString(" ", frameno.length), expr).join()
        if (abbr) line = abbreviate(line, width, "[…]")
        stderr.println(line)
    }
}

// Print things; if the next thing would not fit into the linewidth any more,
// print a newline and the prefix before printing thing. Print a separator
// between things. startpos is where we are on the line before starting. Return
// the reached position on the current line; this can be used as startpos on the
// next call.
fun printThingsWrapped(things: Collection<Any>, linewidth: Int = 79,
                       separator: String = " ", prefix: String = "",
                       startpos: Int = 0): Int {
    var position = startpos
    var first = true
    for (thing in things) {
        if (first) {
            if (position == 0) {
                print(prefix)
                position += prefix.length
            }
            first = false
        } else {
            print(separator)
            position += separator.length
        }
        position = printThingWrapped(thing, linewidth = linewidth,
                                     prefix = prefix, startpos = position)
    }
    return position
}

fun printThingWrapped(thing: Any, linewidth: Int = 79,
                      prefix: String = "", startpos: Int = 0): Int {
    val s = thing.toString()
    var position = startpos
    
    if (position + s.length > linewidth) {
        print("\n" + prefix)
        position = prefix.length
    }
    print(s)
    return position + s.length
}

fun identity(item: Any): Any {
    return item
}

class LOStack(private var top: LObject = Nil) {
    var size = 0
    
    fun push(obj: LObject) {
        size++
        top = LCons(obj, top)
    }

    fun pop(): LObject {
        val result = top.car
        if (top !== Nil) {
            size--
            top = top.cdr
        } else {
            size = 0
        }
        return result
    }

    operator fun iterator() = top.iterator()
}
