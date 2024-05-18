// The format function, to be. I write the requirements up in doc/format.md as I
// learn and (hopefully) understand them.

package org.w21.lyk

// Mapping between a format string and the chain of format directives (including
// for literal pieces of text in the format string). A format string parsed into
// a chain of directives will be stored here to avoid repeated parsing.

val formatMapping = mutableMapOf<String, List<FormatDirective>>()

// format args according to the format string
fun formatArgs(stream: LStream, fstring: String, args: LObject): String {
    if (!formatMapping.containsKey(fstring)) {
        formatMapping[fstring] = parseFormatString(fstring)
    }
    debug(debugFormatstringSym) {
        "fdirs ${formatMapping[fstring]}, args $args"
    }
    val sb = StrBuf()
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
        sb.add(directive.format(stream, d_args))
    }
    return sb.toString()
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

// parses an Int; returns null if s is empty
fun paramInt(s: String, where: Any? = null): Int? {
    if (s == "") {
        return null
    }
    try {
        return s.toInt()
    } catch (e: NumberFormatException) {
        if (where is FormatDirective) {
            throw FormatError("invalid format parameter `$s` in", where)
        }
        if (where == null) {
            throw FormatError("invalid integer `$s`")
        }
        throw FormatError("invalid integer `$s` in $where")
    }
}



val dirClassType = mapOf(
    'c' to ::CharDirective,
    '%' to ::PercentDirective,
    'a' to ::AestheticDirective,
    's' to ::StandardDirective,
    '&' to ::AmpDirective,
    '|' to ::PageDirective,
    '~' to ::TildeDirective,
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
    abstract val type: String
    
    // return the number of arguments needed for this directive
    abstract fun argsNeeded(): Int

    // format the appropriate number of arguments and return the formatted
    // String; output may depend on the stream
    abstract fun format(stream: LStream, args: MutableList<LObject>): String

    override fun toString() = "#<$type format \"$directive\">"
}

class StandardDirective(formatString: String,
                        directive: String,
                        params: List<String>,
                        colonFlag: Boolean,
                        atsignFlag: Boolean,
): AestheticDirective(formatString, directive, params, colonFlag, atsignFlag) {
    override fun show(arg: LObject) = arg.desc(null)
}

open class AestheticDirective(formatString: String,
                              directive: String,
                              params: List<String>,
                              val colonFlag: Boolean,
                              val atsignFlag: Boolean,
): FormatDirective(formatString, directive) {
    override val type = "aesthetic"
    var mincol: Int? = 0
    var colinc: Int? = 1
    var minpad: Int? = 0
    var padchar: Char? = ' '
    var needArgs = 1
    
    init {
        val nparams = params.size
        if (nparams >= 1) {
            if (params[0] == "v") {
                mincol = null           // marker for "get arg"
                needArgs++
            } else {
                mincol = paramInt(params[0], this) ?: mincol
            }
            if (nparams >= 2) {
                if (params[1] == "v") {
                    colinc = null       // marker for "get arg"
                    needArgs++
                } else {
                    colinc = paramInt(params[1], this) ?: colinc
                }
                if (nparams >= 3) {
                    if (params[2] == "v") {
                        minpad = null   // marker for "get arg"
                        needArgs++
                    } else {
                        minpad = paramInt(params[2], this) ?: minpad
                    }
                    if (nparams >= 4) {
                        val padc_param = params[3]
                        if (padc_param == "v") {
                            padchar = null
                        } else {
                            if (padc_param.length != 2
                                    || padc_param[0] != '\'') {
                                throw FormatError("invalid padchar parameter",
                                                  this)
                            }
                            padchar = padc_param[1]
                        }
                    }
                }
            }
        }
    }
    open fun show(arg: LObject) = arg.toString()
    override fun argsNeeded() = needArgs
    override fun format(stream: LStream, args: MutableList<LObject>): String {
        if (mincol == null) {
            mincol = intArg(args[0], " directive `$directive`: mincol")
            args.removeAt(0)
        }
        if (colinc == null) {
            colinc = intArg(args[0], " directive `$directive`: colinc")
            args.removeAt(0)
        }
        if (minpad == null) {
            minpad = intArg(args[0], " directive `$directive`: minpad")
            args.removeAt(0)
        }
        if (padchar == null) {
            padchar = charArg(args[0], " directive `$directive`: padchar")
                .the_char
            args.removeAt(0)
        }
        val arg = args[0]
        var s =
            if (arg === Nil) {
                if (colonFlag) {
                    "()"
                } else {
                    "nil"
                }
            } else {
                show(arg)
            }
        if (minpad!! > 0) {
            if (atsignFlag) {
                s = mulString(padchar.toString(), minpad!!) + s
            } else {
                s = s + mulString(padchar.toString(), minpad!!)
            }
        }
        if (mincol!! > 0) {
            val pad = mulString(padchar.toString(), colinc!!)
            val missing = mincol!! - s.length
            var need = missing / colinc!!
            need += if (need * colinc!! < missing) 1 else 0

            if (atsignFlag) {
                s = mulString(pad, need) + s
            } else {
                s = s + mulString(pad, need)
            }
        }
        return s
    }
}

@Suppress("UNUSED_PARAMETER")
class CharDirective(formatString: String,
                    directive: String,
                    params: List<String>,
                    val colonFlag: Boolean,
                    val atsignFlag: Boolean
): FormatDirective(formatString, directive) {

    override val type = "char"
    
    override fun argsNeeded() = 1

    override fun format(stream: LStream, args: MutableList<LObject>): String {
        val lchar = args[0]
        val ch = charArg(lchar, " directive " + directive)
        if (colonFlag) {
            // we skip the ": + @": (like ~:C with unusual shift keys mentioned)
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
    override val type = "ig.newline"
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
    val count: Int?

    init {
        when (params.size) {
            0 -> count = 1
            1 -> {
                val param = params[0]
                if (param == "v") {
                    count = null
                } else {
                    count = paramInt(param, this)
                }
            }
            else -> throw FormatError("too many parameters in", this)
        }
    }

    override val type = "newline"
    override fun argsNeeded() = if (count == null) 1 else 0
    override fun format(stream: LStream, args: MutableList<LObject>) =
        mulString("\n", count ?: intArg(args[0], " directive " + directive))
}

class AmpDirective(formatString: String,
                   directive: String,
                   params: List<String>,
                   val colonFlag: Boolean,
                   val atsignFlag: Boolean,
): FormatDirective(formatString, directive) {
    init {
        if (params.size > 0) {
            throw FormatError("too many parameters in", this)
        }
    }
    override val type = "freshline"
    override fun argsNeeded() = 0
    override fun format(stream: LStream, args: MutableList<LObject>): String {
        if (stream.newlineLast()) {
            return "\n"
        }
        return ""
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
    val count: Int?

    init {
        when (params.size) {
            0 -> count = 1
            1 -> {
                val param = params[0]
                if (param == "v") {
                    count = null
                } else {
                    count = paramInt(param, this)
                }
            }
            else -> throw FormatError("too many parameters in", this)
        }
    }

    override val type = "page"
    override fun argsNeeded() = if (count == null) 1 else 0
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
    override val type = "tilde"
    var count: Int? = 1

    init {
        if (params.size == 1) {
            val param = params[0]
            if (param == "v") {
                count = null
            } else {
                count = paramInt(param, this)
            }
        }
        if (params.size > 1) {
            throw FormatError("too many parameters in", this)
        }
    }

    override fun argsNeeded() = if (count == null) 1 else 0
    override fun format(stream: LStream, args: MutableList<LObject>)
        = mulString("~",
                    count ?: intArg(args[0], " directive " + directive))
}

// pseudo directive, a piece of literal text
class TextDirective(formatString: String,
                    directive: String,
): FormatDirective(formatString, directive) {
    init {
        debug(debugFormatstringSym) {
            "TextDirective(`$formatString`, `$directive`)"
        }
    }
    override val type = "text"
    override fun argsNeeded() = 0
    override fun format(stream: LStream, args: MutableList<LObject>)
        = directive
}

// EOF
