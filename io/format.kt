// The format function, to be. I write the requirements up in doc/format.md as I
// learn and (hopefully) understand them.

package org.w21.lyk

import kotlin.math.abs
import kotlin.math.pow

// Mapping between a format string and the chain of format directives (including
// for literal pieces of text in the format string). A format string parsed into
// a chain of directives will be stored here to avoid repeated parsing.

val formatMapping = mutableMapOf<String, List<FormatDirective>>()

// format args according to the format string
fun formatArgs(stream: LStream, fstring: String, args: LObject) {
    if (!formatMapping.containsKey(fstring)) {
        formatMapping[fstring] = parseFormatString(fstring)
    }
    debug(debugFormatstringSym) {
        "fdirs ${formatMapping[fstring]}, args $args"
    }
    var argptr = args
    for (directive in (formatMapping[fstring] ?:
                           throw InternalError("`$fstring` not in "
                                               + "formatMapping??"))) {
        val d_args = mutableListOf<LObject>()
        val needed = directive.argsNeeded()
        for (index in 0 ..< needed) {
            if (argptr === Nil) {
                throw FormatError("not enough arguments (wants $needed) for",
                                  directive)
            }
            d_args.add(argptr.car)
            argptr = argptr.cdr
        }
        stream.write(directive.format(stream, d_args))
    }
}

enum class Await {
    Tilde,                              // initial, outside of a format
                                        // directive
    Param_flag_type,                    // parameters or flag or the type
    Quote_char,                         // arbitrary char after '
    Flag_type,                          // a flag char or the type
}


fun parseFormatString(fstring: String): List<FormatDirective> {
    val directives = mutableListOf<FormatDirective>()
    var params = mutableListOf<String>()
    var await = Await.Tilde
    var paramBuf: StrBuf? = null        // parameter value or just text
    var colonFlag = false
    var atsignFlag = false
    var dirBuf = StrBuf()
    var ignoreWhitespace = false        // ignore after NewlineDirective
    
    fun setFlag(ch: Char) {             // must only be ':' or '@'
        if (ch == ':') {
            colonFlag = true
        } else {
            atsignFlag = true
        }
    }

    // finish one directive off -- split the parameters, create and chain the
    // directive, and reset the status variables
    fun finishDirective(dirType: Char) {
        debug(debugFormatstringSym) {
            "finishDirective $dirType, params `${paramBuf?.toString()}`"
        }
        if (paramBuf != null) {
            // we cannot use str.split() here because an arbitrary character may
            // follow #\'
            var pbuf = StrBuf()
            var in_quote = false
            for (ch in paramBuf.toString()) {
                if (in_quote) {
                    pbuf.add(ch)
                    in_quote = false
                } else if (ch == ',') {
                    params.add(pbuf.toString())
                    pbuf = StrBuf()
                } else if (ch == '\'') {
                    pbuf.add(ch)
                    in_quote = true
                } else {
                    pbuf.add(ch)
                }
            }
            params.add(pbuf.toString())
        }
        val directive = makeDirective(fstring, "~" + dirBuf.toString(),
                                      dirType.lowercase()[0], params,
                                      colonFlag, atsignFlag)
        if (directive is NewlineDirective && !directive.colonFlag) {
            ignoreWhitespace = true
        }
        directives.add(directive)

        paramBuf = null
        colonFlag = false
        atsignFlag = false
        dirBuf = StrBuf()
    }

    fun setStatus(new: Await) {
        debug(debugFormatstringSym) {
            "parser status $await => $new"
        }
        await = new
    }

    for (ch in fstring) {
        debug(debugFormatstringSym) {
            "see char $ch"
        }
        if (ignoreWhitespace) {
            if (ch.isWhitespace()) {
                debug(debugFormatstringSym) {
                    "ignore whitespace"
                }
                continue
            }
            ignoreWhitespace = false    // switch this off at the end of whtspc
        }
        if (await != Await.Tilde) {
            dirBuf.add(ch)
        }
        when (await) {
            Await.Tilde -> {
                if (ch == '~') {
                    if (paramBuf != null) {
                        directives.add(TextDirective(
                                           fstring, paramBuf.toString()))
                        paramBuf = null
                    }
                    params = mutableListOf<String>()
                    setStatus(Await.Param_flag_type)
                } else {
                    paramBuf = paramBuf ?: StrBuf()
                    paramBuf!!.add(ch)
                }
            }
            Await.Param_flag_type -> {
                if (ch in "+-01234567890v,#") {
                    paramBuf = paramBuf ?: StrBuf()
                    paramBuf!!.add(ch)
                } else if (ch == '\'') {
                    paramBuf = paramBuf ?: StrBuf()
                    paramBuf!!.add(ch)
                    await = Await.Quote_char
                } else if (ch in ":@") {
                    setFlag(ch)
                    await = Await.Flag_type
                } else {
                    finishDirective(ch)
                    setStatus(Await.Tilde)
                }
            }
            Await.Quote_char -> {
                paramBuf!!.add(ch)
                setStatus(Await.Param_flag_type)
            }
            Await.Flag_type -> {
                if (ch in ":@") {
                    setFlag(ch)
                } else {
                    finishDirective(ch)
                    setStatus(Await.Tilde)
                }
            }
        }
    }
    if (await == Await.Tilde) {
        val append = paramBuf?.toString() ?: ""
        if (append != "") {
            directives.add(TextDirective(fstring, append))
        }
    } else {
        FormatError("incomplete format directive `${dirBuf.toString()}`",
                    fstring)
    }
    return directives
}

/// helper function(s) for the format directives

// parse an Int; return default if s is empty, null if it is "v"
fun paramInt(s: String, default: Int, where: FormatDirective): Int? {
    if (s == "") {
        return default
    }
    if (s == "v") {
        return null
    }
    try {
        return s.toInt()
    } catch (e: NumberFormatException) {
        throw FormatError("invalid format parameter `$s` in", where)
    }
}

// return a character from a "'." parameter; return default if it is empty, null
// if it is "v"
fun paramChar(s: String, default: Char, where: FormatDirective): Char? {
    if (s == "") {
        return default
    }
    if (s == "v") {
        return null
    }
    if (s.length != 2 || s[0] != '\'') {
        throw FormatError("invalid padchar parameter", where)
    }
    return s[1]
}

// return a character from a "'." parameter as a string; return empty if it is
// empty, null if it is "v"
fun paramString(s: String, where: FormatDirective): String? {
    if (s == "") {
        return ""
    }
    if (s == "v") {
        return null
    }
    if (s.length != 2 || s[0] != '\'') {
        throw FormatError("invalid padchar parameter", where)
    }
    return s.substring(1)
}

// pop the first string value off the list
fun popS(l: MutableList<String>): String {
    val result = l[0]
    l.removeAt(0)
    return result
}

fun popO(l: MutableList<LObject>): LObject {
    val result = l[0]
    l.removeAt(0)
    return result
}

val romanNumberCodes = arrayOf(
    //    size, code, new
    Triple(1000, "M", false),
    Triple(900, "CM", true),
    Triple(500,  "D", false),
    Triple(400, "CD", true),
    Triple(100,  "C", false),
    Triple(90,  "XC", true),
    Triple(50,   "L", false),
    Triple(40,  "XL", true),
    Triple(10,   "X", false),
    Triple(9,   "IX", true),
    Triple(5,    "V", false),
    Triple(4,   "IV", true),
    Triple(1,    "I", false),
)

// convert to Roman numeral
fun formatRoman(arg: Long, oldRoman: Boolean, where: FormatDirective): String {
    if (arg < 1) {
        throw FormatError("value $arg too small for Roman numeral", where)
    }
    if (arg >= 4000) {
        throw FormatError("value $arg too large for Roman numeral", where)
    }
    val sb = StrBuf()
    var remain = arg.toInt()
    while (remain >= 1000) {
        sb.add("M")
        remain -= 1000
    }
    for ((size, code, new) in romanNumberCodes) {
        if (new && oldRoman) {
            continue
        }
        while (remain >= size) {
            sb.add(code)
            remain -= size
        }
    }    
    return sb.toString()
}

val englishBelow20 = arrayOf(
    "zero", "one", "two", "three", "four", "five", "six", "seven", "eight",
    "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
    "sixteen", "seventeen", "eighteen", "nineteen",
)

val englishBelow20th = arrayOf(
    "zeroth", "first", "second", "third", "fourth", "fifth", "sixth",
    "seventh", "eightth", "nineth", "tenth", "eleventh", "twelfth",
    "thirteenth", "fourteenth", "fifteenth", "sixteenth", "seventeenth",
    "eighteenth", "nineteenth",
)

val englishTens = arrayOf(
    Pair(90, "ninety"),
    Pair(80, "eighty"),
    Pair(70, "seventy"),
    Pair(60, "sixty"),
    Pair(50, "fifty"),
    Pair(40, "fourty"),
    Pair(30, "thirty"),
    Pair(20, "twenty"),
)

val englishDimensions = arrayOf(             // dimension => name
    Pair(pow(10, 18), "quintillion"), // enough for 64-bit Longs
    Pair(pow(10, 15), "quadrillion"),
    Pair(pow(10, 12), "trillion"),
    Pair(pow(10,  9), "billion"),
    Pair(pow(10,  6), "million"),
    Pair(pow(10,  3), "thousand"),
    Pair(pow(10,  2), "hundred"),
)

// Due to the fault of our pretend integers (which are Doubles
// actually) being imprecise at the end of their range, this will
// deliver wrong results for very large integers.
fun formatEnglish(arg: Long, ordinal: Boolean, where: FormatDirective): String {
    val array = if (ordinal) englishBelow20th else englishBelow20
    val what = if (ordinal) "ordinals" else "cardinals"
    var value = arg
    val result = StrBuf()

    debug(debugEnglishSym) {
        "formatEnglish($value, $what, $where)"
    }
    if (value < 0L) {
        value = -value
        result.add("negative")
        debug(debugEnglishSym) {
            "now negative $value, ${result.desc()}"
        }
    }
    if (value < 20L) {
        // as we need to handle zero separately anyway, we can
        // optimize for the assumed most common case < 20 as well
        // without extra cost
        result.add(array[value.toInt()])
    } else {
        for ((dimension, name) in englishDimensions) {
            val factor = value / dimension
            if (value >= dimension) {
                debug(debugEnglishSym) {
                    "dimension $dimension factor $factor, ${result.desc()}"
                }
                result.add(formatEnglish(factor, false, where))
                result.add(name)
            }
            value -= factor * dimension
            debug(debugEnglishSym) {
                ("remaining value after dimension $dimension: $value,"
                 + " ${result.desc()}")
            }
        }
        if (value >= 20L) {
            debug(debugEnglishSym) {
                "value $value >= 20L, ${result.desc()}"
            }
            var tens = ""           // *will* be changed in the loop below
            for ((size, name) in englishTens) {
                if (value >= size) {
                    tens = name
                    value -= size
                    debug(debugEnglishSym) {
                        "tens $tens, value remains $value, ${result.desc()}"
                    }
                    break
                }
            }
            if (value > 0L) {
                result.add(tens + "-" + array[value.toInt()])
                debug(debugEnglishSym) {
                    "singles ${array[value.toInt()]}, ${result.desc()}"
                }
            } else {
                result.add(tens)
            }
        } else if (value > 0L) {
            result.add(array[value.toInt()])
            debug(debugEnglishSym) {
                "value $value >= 20L, ${result.desc()}"
            }
        }
    }
       
    return result.join()
}

// this is not generally usable, returns (maybe) "+" for zero
fun val_sign(n: Long, forced: Boolean): String {
    if (n < 0L) {
        return "-"
    }
    if (forced) {
        return "+"
    }
    return ""
}


fun format_radix(directive: FormatDirective, radix: Int, mincol_: Int?,
                 padchar_: Char?, commachar_: Char?, comma_int_: Int?,
                 colonFlag: Boolean, atsignFlag: Boolean,
                 stream: LStream, args: MutableList<LObject>): String {
    var mincol = mincol_
    var padchar = padchar_
    var commachar = commachar_
    var comma_int = comma_int_
    if (radix < 2 || radix > 36) {
        FormatError("invalid radix $radix, must be in 2..36", directive)
    }
    if (mincol == null) {
        mincol = intArg(popO(args),
                        " directive `${directive.directive}`: mincol")
    }
    if (padchar == null) {
        padchar = charArg(popO(args),
                          " directive `${directive.directive}`: padchar")
            .the_char
    }
    if (commachar == null) {
        commachar = charArg(popO(args),
                            " directive `${directive.directive}`: commachar")
            .the_char
    }
    if (comma_int == null) {
        comma_int = intArg(popO(args),
                           " directive `${directive.directive}`: comma_int")
    }
    
    val arg_val = args[0]
    if (arg_val !is LNumber || !arg_val.isLong()) {
        return format_aesthetic(directive, mincol, 1, 0, padchar,
                                colonFlag, atsignFlag, stream, args)
    }

    val arg = arg_val.toLong()
    var sign = val_sign(arg, atsignFlag)
    var the_string = sign + abs(arg).toString(radix)
    var slen = the_string.length

    if (colonFlag) {                // --> print commachars
        val rev_string = the_string.reversed()
        val sb = StrBuf()
        var index = 0
        var length = 0
        while (index < slen || length < mincol) {
            if (index > 0 && index % comma_int == 0) {
                sb.add(commachar)
                length++
            }
            if (index < slen) {
                sb.add(rev_string[index])
            } else if (length < mincol) {
                sb.add(padchar)
            }
            index++
            length++
        }
        the_string = sb.toString().reversed()
    } else {
        the_string =
            mulString(padchar.toString(), mincol - slen) + the_string
    }
    return the_string
}


@Suppress("UNUSED_PARAMETER")
fun format_aesthetic(directive: FormatDirective, mincol_: Int?, colinc_: Int?,
                     minpad_: Int?, padchar_: Char?, colonFlag: Boolean,
                     atsignFlag: Boolean,
                     stream: LStream, args: MutableList<LObject>): String {
    var mincol = mincol_
    var colinc = colinc_
    var minpad = minpad_
    var padchar = padchar_
    if (mincol == null) {
        mincol = intArg(popO(args),
                        " directive `${directive.directive}`: mincol")
    }
    if (colinc == null) {
        colinc = intArg(popO(args),
                        " directive `${directive.directive}`: colinc")
    }
    if (minpad == null) {
        minpad = intArg(popO(args),
                        " directive `${directive.directive}`: minpad")
    }
    if (padchar == null) {
        padchar = charArg(popO(args),
                          " directive `${directive.directive}`: padchar")
            .the_char
    }
    val arg = args[0]
    var the_string =
        if (arg === Nil) {
            if (colonFlag) {
                "()"
            } else {
                "nil"
            }
        } else {
            directive.present(arg)
        }
    if (minpad > 0) {
        if (atsignFlag) {
            the_string =
                mulString(padchar.toString(), minpad) + the_string
        } else {
            the_string =
                the_string + mulString(padchar.toString(), minpad)
        }
    }
    if (mincol > 0) {
        val pad = mulString(padchar.toString(), colinc)
        val missing = mincol - the_string.length
        var need = missing / colinc
        need += if (need * colinc < missing) 1 else 0

        if (atsignFlag) {
            the_string = mulString(pad, need) + the_string
        } else {
            the_string = the_string + mulString(pad, need)
        }
    }
    return the_string
}


val dirClassType = mapOf(
    'a' to ::AestheticDirective,
    'b' to ::BinaryDirective,
    'c' to ::CharDirective,
    'd' to ::DecimalDirective,
    'f' to ::FixedFPDirective,
    'o' to ::OctalDirective,
    'x' to ::HexadecimalDirective,
    's' to ::StandardDirective,
    '%' to ::PercentDirective,
    '&' to ::AmpDirective,
    '|' to ::PageDirective,
    '~' to ::TildeDirective,
    '\n' to ::NewlineDirective,
    'r' to ::RadixDirective,
)
    

// Return a Format directive of the appropriate type according to `ch` with the
// specified parameters and flags. The format string and the directive string
// are for the use in error messages.
fun makeDirective(fstring: String,      // the format string with the dir.
                  directiveS: String,   // the directive as a string
                  dirType: Char,        // the character denoting the type
                  params: List<String>, // the prefix parameters
                  colonFlag: Boolean,   // true iff colon flag was set
                  atsignFlag: Boolean,  // true iff atsign flag was set
): FormatDirective {
    debug(debugFormatstringSym) {
        "makeDirective(`$fstring`, `$directiveS` $dirType, $params,\n                  colon = $colonFlag, atsign = $atsignFlag)"
    }
    val dirClass = dirClassType.get(dirType)
    if (dirClass != null) {
        return dirClass(fstring, directiveS, params, colonFlag, atsignFlag)
    }
    throw FormatError("directive for `$dirType` not implemented,", fstring)
}


// This is meant to be one element of a chain of directives extracted from a
// format string.
abstract class FormatDirective(val formatString: String,
                               val directive: String)
{
    abstract val fd_type: String
    
    // return the number of arguments needed for this directive
    abstract fun argsNeeded(): Int

    // format the appropriate number of arguments and return the formatted
    // String; output may depend on the stream
    abstract fun format(stream: LStream, args: MutableList<LObject>): String

    // present the argument like this
    open fun present(arg: LObject) = arg.toString()

    override fun toString() = "#<$fd_type format \"$directive\">"
    init {
        // println("create $this")
    }
}

open class FixedFPDirective(formatString: String,
                            directive: String,
                            params: List<String>,
                            val colonFlag: Boolean,
                            val atsignFlag: Boolean,
): FormatDirective(formatString, directive) {
    override val fd_type = "decimal"
    var width: Int? = -1
    var d_frac: Int? = -1
    var skale: Int? = 0
    var overflowchar: String? = ""
    var padchar: Char? = ' '
    var needArgs = 1

    init {
        val nparams = params.size
        if (nparams >= 1) {
            width = paramInt(params[0], width!!, this)
            if (width == null) {
                needArgs++
            }
            if (nparams >= 2) {
                d_frac = paramInt(params[1], d_frac!!, this)
                if (d_frac == null) {
                    needArgs++
                }
                if (nparams >= 3) {
                    skale = paramInt(params[2], skale!!, this)
                    if (skale == null) {
                        needArgs++
                    }
                    if (nparams >= 4) {
                        overflowchar = paramString(params[3], this)
                        if (overflowchar == null) {
                            needArgs++
                        }
                        if (nparams >= 5) {
                            padchar = paramChar(params[4], padchar!!, this)
                            if (padchar == null) {
                                needArgs++
                            }
                        }
                    }
                }
            }
        }
    }

    override fun argsNeeded() = needArgs
    
    override fun format(stream: LStream, args: MutableList<LObject>): String {
        if (width == null) {
            width = intArg(popO(args), " directive `$directive` width")
        }
        if (d_frac == null) {
            d_frac = intArg(popO(args), " directive `$directive` d_frac")
        }
        if (skale == null) {
            skale = intArg(popO(args), " directive `$directive` skale")
        }
        if (overflowchar == null) {
            overflowchar = charArg(popO(args),
                                   " directive `$directive` overflowchar")
                .toString()
        }
        if (padchar == null) {
            padchar = charArg(popO(args), " directive `$directive` padchar")
                .the_char
        }

        val arg = args[0]
        if (arg !is LNumber) {
            return format_aesthetic(
                this, (if (width == -1) 0 else width!!), 1, 0, padchar!!,
                colonFlag, atsignFlag, stream, args)
        }
        val fformat = "%%.%df".format(if (d_frac!! >= 0) d_frac!! else 20)
        var the_string =
            fformat.format(arg.the_number * 10.0.pow(skale!!.toDouble()))
        var (before, after) = the_string.split(".")
        if (d_frac!! >= 0) {
            if (d_frac!! > after.length) {
                after += mulString("0", d_frac!! - after.length)
            } else {
                after = after.substring(0, d_frac!!)
            }
            the_string = before + "." + after
        } else {
            the_string = the_string.trimEnd('0')
            if (the_string.endsWith(".")) {
                the_string += "0"
            }
        }
        if (width!! >= 0) {
            if (width!! < the_string.length) {
                if (d_frac!! < 0) {
                    val roundto = width!! - before.length - 1
                    if (roundto >= 0) {
                        val round_format = "%%.%df".format(roundto)
                        return round_format.format(arg.the_number)
                    }
                }
                if (overflowchar == "") {
                    return the_string
                }
                return mulString(overflowchar!!, width!!)
            }
            return (mulString(padchar!!.toString(), width!! - the_string.length)
                    + the_string)
        }
        return the_string
    }

}


class BinaryDirective(formatString: String,
                       directive: String,
                       params: List<String>,
                       colonFlag: Boolean,
                       atsignFlag: Boolean,
): DecimalDirective(formatString, directive, params, colonFlag, atsignFlag) {
    override val fd_type = "decimal"
    override val radix = 0b10
}

class OctalDirective(formatString: String,
                       directive: String,
                       params: List<String>,
                       colonFlag: Boolean,
                       atsignFlag: Boolean,
): DecimalDirective(formatString, directive, params, colonFlag, atsignFlag) {
    override val fd_type = "Octal"
    override val radix = 8  // "Octal literals are not supported in Kotlin." :-(
}

class HexadecimalDirective(formatString: String,
                       directive: String,
                       params: List<String>,
                       colonFlag: Boolean,
                       atsignFlag: Boolean,
): DecimalDirective(formatString, directive, params, colonFlag, atsignFlag) {
    override val fd_type = "Hexadecimal"
    override val radix = 0x10
}

open class DecimalDirective(formatString: String,
                            directive: String,
                            params: List<String>,
                            val colonFlag: Boolean,
                            val atsignFlag: Boolean,
): FormatDirective(formatString, directive) {
    override val fd_type = "decimal"
    open val radix = 10
    var mincol: Int? = 0
    var padchar: Char? = ' '
    var commachar: Char? = ','
    var comma_int: Int? = 3
    var needArgs = 1

    init {
        val nparams = params.size
        if (nparams >= 1) {
            mincol = paramInt(params[0], mincol!!, this)
            if (mincol == null) {
                needArgs++
            }
            if (nparams >= 2) {
                padchar = paramChar(params[1], padchar!!, this)
                if (padchar == null) {
                    needArgs++
                }
                if (nparams >= 3) {
                    commachar = paramChar(params[2], commachar!!, this)
                    if (commachar == null) {
                        needArgs++
                    }
                    if (nparams >= 4) {
                        comma_int = paramInt(params[3], comma_int!!, this)
                        if (comma_int == null) {
                            needArgs++
                        }
                    }
                }
            }
        }
    }
    override fun argsNeeded() = needArgs
    override fun format(stream: LStream, args: MutableList<LObject>) =
        format_radix(this, radix, mincol, padchar, commachar, comma_int,
                     colonFlag, atsignFlag, stream, args)
}

open class RadixDirective(formatString: String,
                          directive: String,
                          params: List<String>,
                          val colonFlag: Boolean,
                          val atsignFlag: Boolean,
): FormatDirective(formatString, directive) {
    override val fd_type = "radix"
    var radix: Int? = 10
    var mincol: Int? = 0
    var padchar: Char? = ' '
    var commachar: Char? = ','
    var comma_int: Int? = 3
    var needArgs = 1
    var doRoman = false
    var doEnglish = false

    init {
        val nparams = params.size
        if (nparams == 0) {
            doRoman = atsignFlag
            doEnglish = !atsignFlag
        } else if (nparams >= 1) {
            radix = paramInt(params[0], radix!!, this)
            if (radix == null) {
                needArgs++
            }
            if (nparams >= 2) {
                mincol = paramInt(params[1], mincol!!, this)
                if (mincol == null) {
                    needArgs++
                }
                if (nparams >= 3) {
                    padchar = paramChar(params[2], padchar!!, this)
                    if (padchar == null) {
                        needArgs++
                    }
                    if (nparams >= 4) {
                        commachar = paramChar(params[3], commachar!!, this)
                        if (commachar == null) {
                            needArgs++
                        }
                        if (nparams >= 5) {
                            comma_int = paramInt(params[4], comma_int!!, this)
                            if (comma_int == null) {
                                needArgs++
                            }
                        }
                    }
                }
            }
        }
    }
    override fun argsNeeded() = needArgs

    override fun format(stream: LStream, args: MutableList<LObject>): String {
        if (doRoman) {
            return formatRoman(longArg(args[0], " directive `$directive`"),
                                       colonFlag, this)
        }
        if (doEnglish) {
            return formatEnglish(longArg(args[0], " directive `$directive`"),
                                 colonFlag, this)
        }
        if (radix == null) {
            radix = intArg(popO(args), " directive `$directive`: radix")
            if (radix!! < 2 || radix!! > 36) {
                FormatError("invalid radix $radix, must be in 2..36", this)
            }
        }
        return format_radix(this, radix!!, mincol, padchar, commachar,
                            comma_int, colonFlag, atsignFlag, stream, args)
    }
}


class StandardDirective(formatString: String,
                        directive: String,
                        params: List<String>,
                        colonFlag: Boolean,
                        atsignFlag: Boolean,
): AestheticDirective(formatString, directive, params, colonFlag, atsignFlag) {
    override fun present(arg: LObject) = arg.desc(null)
}

open class AestheticDirective(formatString: String,
                              directive: String,
                              params: List<String>,
                              val colonFlag: Boolean,
                              val atsignFlag: Boolean,
): FormatDirective(formatString, directive) {
    override val fd_type = "aesthetic"
    var mincol: Int? = 0
    var colinc: Int? = 1
    var minpad: Int? = 0
    var padchar: Char? = ' '
    var needArgs = 1
    
    init {
        val nparams = params.size
        if (nparams >= 1) {
            mincol = paramInt(params[0], mincol!!, this)
            if (mincol == null) {   // marker for "get arg"
                needArgs++
            }
            if (nparams >= 2) {
                colinc = paramInt(params[1], colinc!!, this)
                if (colinc == null) {       // marker for "get arg"
                    needArgs++
                }
                if (nparams >= 3) {
                    minpad = paramInt(params[2], minpad!!, this)
                    if (minpad == null) {   // marker for "get arg"
                        needArgs++
                    }
                    if (nparams >= 4) {
                        padchar = paramChar(params[3], padchar!!, this)
                        if (padchar == null) {
                            needArgs++
                        }
                    }
                }
            }
        }
    }
    override fun argsNeeded(): Int {
        return needArgs
    }
    override fun format(stream: LStream, args: MutableList<LObject>) =
        format_aesthetic(this, mincol, colinc, minpad, padchar, colonFlag,
                         atsignFlag, stream, args)
}

@Suppress("UNUSED_PARAMETER")
class CharDirective(formatString: String,
                    directive: String,
                    params: List<String>,
                    val colonFlag: Boolean,
                    val atsignFlag: Boolean
): FormatDirective(formatString, directive) {
    override val fd_type = "char"
    
    override fun argsNeeded() = 1

    override fun format(stream: LStream, args: MutableList<LObject>): String {
        val ch = charArg(args[0], " directive " + directive)
        if (colonFlag) {
            // we skip the ":@": (like ~:C with unusual shift keys mentioned)
            // and do it just like ~:C
            val name = LChar.charName[ch.the_char]
            if (name != null) {
                return "#\\" + name
            }
            return ch.toString()
        } else {
            if (atsignFlag) {           // @ only: spelled out for all
                return ch.desc(null)
            } else {                    // none: the character itself
                return ch.toString()
            }
        }
    }
}

@Suppress("UNUSED_PARAMETER")
class NewlineDirective(formatString: String,
                       directive: String,
                       params: List<String>,
                       val colonFlag: Boolean,
                       val atsignFlag: Boolean,
): FormatDirective(formatString, directive) {
    override val fd_type = "ig.newline"
    override fun argsNeeded() = 0
    override fun format(stream: LStream, args: MutableList<LObject>) =
        if (atsignFlag) "\n" else ""
}

// count is the number of newlines to print; if it is null, an argument is
// needed for the number of newlines
class PercentDirective(formatString: String,
                       directive: String,
                       params: List<String>,
                       val colonFlag: Boolean,
                       val atsignFlag: Boolean,
): FormatDirective(formatString, directive) {
    override val fd_type = "newline"
    var count: Int? = 1
    var needArgs = 0

    init {
        if (params.size == 1) {
            count = paramInt(params[0], count!!, this)
            if (count == null) {
                needArgs++
            }
        } else if (params.size > 0) {
            throw FormatError("too many parameters in", this)
        }
    }

    override fun argsNeeded() = needArgs
    override fun format(stream: LStream, args: MutableList<LObject>) =
        mulString("\n", count ?: intArg(args[0], " directive " + directive))
}

class AmpDirective(formatString: String,
                   directive: String,
                   params: List<String>,
                   val colonFlag: Boolean,
                   val atsignFlag: Boolean,
): FormatDirective(formatString, directive) {
    override val fd_type = "freshline"
    init {
        if (params.size > 0) {
            throw FormatError("too many parameters in", this)
        }
    }
    override fun argsNeeded() = 0
    override fun format(stream: LStream, args: MutableList<LObject>): String {
        if (stream.newlineLast()) {
            return ""
        }
        return "\n"
    }
}

// count is the number of page separators to print; if it is null, an argument
// is needed for the number of page separators
class PageDirective(formatString: String,
                    directive: String,
                    params: List<String>,
                    val colonFlag: Boolean,
                    val atsignFlag: Boolean,
): FormatDirective(formatString, directive) {
    override val fd_type = "page"
    var count: Int? = 1
    var needArgs = 0

    init {
        if (params.size == 1) {
            count = paramInt(params[0], count!!, this)
            if (count == null) {
                needArgs++
            }
        } else if (params.size > 1) {
            throw FormatError("too many parameters in", this)
        }
    }

    override fun argsNeeded() = needArgs
    override fun format(stream: LStream, args: MutableList<LObject>)
        = mulString("\u000c",
                    count ?: intArg(args[0], " directive " + directive))
}

// count is the number of tildes to print; if it is null, an argument is needed
// for the number of tildes
class TildeDirective(formatString: String,
                       directive: String,
                       params: List<String>,
                       val colonFlag: Boolean,
                       val atsignFlag: Boolean,
): FormatDirective(formatString, directive) {
    override val fd_type = "tilde"
    var count: Int? = 1
    var needArgs = 0

    init {
        if (params.size == 1) {
            count = paramInt(params[0], count!!, this)
            if (count == null) {
                needArgs++
            }
        }
        if (params.size > 1) {
            throw FormatError("too many parameters in", this)
        }
    }

    override fun argsNeeded() = needArgs
    override fun format(stream: LStream, args: MutableList<LObject>)
        = mulString("~",
                    count ?: intArg(args[0], " directive " + directive))
}

// pseudo directive, a piece of literal text
class TextDirective(formatString: String,
                    directive: String,
): FormatDirective(formatString, directive) {
    override val fd_type = "text"
    init {
        debug(debugFormatstringSym) {
            "TextDirective(`$formatString`, `$directive`)"
        }
    }
    override fun argsNeeded() = 0
    override fun format(stream: LStream, args: MutableList<LObject>)
        = directive
}

// EOF
