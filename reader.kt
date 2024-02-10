// Reader and Readertoken (which comes first)


// Token types for the Reader. Of the most, the function is fully described by
// their type; tokens for strings, symbols, numbers, and regexps also have some
// value.

open class ReaderToken(reader: Reader): LocationHolder {
    val readerLocation = reader.location()

    open override fun location() = readerLocation
    
    open fun description() = "${this.type}"
}

class OparenToken(reader: Reader): ReaderToken(reader)
class CparenToken(reader: Reader): ReaderToken(reader) {}
class PeriodToken(reader: Reader): ReaderToken(reader) {}
class QuoteToken(reader: Reader): ReaderToken(reader) {}
class FunctionToken(reader: Reader): ReaderToken(reader) {}
class UnquoteToken(reader: Reader): ReaderToken(reader) {}
class QuasiquoteToken(reader: Reader): ReaderToken(reader) {}
class UnquoteSplicingToken(reader: Reader): ReaderToken(reader) {}
open class StringToken(reader: Reader, val value: String): ReaderToken(reader) {
    override fun description() = super.description() + "($value)"
}
class SymbolToken(reader: Reader, value: String):
    StringToken(reader, value) {}
class NumberToken(reader: Reader, val value: Double): ReaderToken(reader) {
    override fun description() = super.description() + "($value)"
}
class TableStartToken(reader: Reader): ReaderToken(reader) {}
class VectorStartToken(reader: Reader): ReaderToken(reader) {}
class RegexpToken(reader: Reader, value: String):
    StringToken(reader, value) {}
class EOFToken(reader: Reader): ReaderToken(reader) {}


val commentChar = ';'

// special characters to be escaped in string literals and similar items

// backward: the escaped char after a backslash => real char
val escape2specialChar = mapOf(
  "a" to "\u0007", "b" to "\b", "f" to "\u000c", "n" to "\n",
  "r" to "\r", "t" to "\t", "v" to "\u000b", "\"" to "\"", "\\" to "\\",
)
var specialChar2escaped = mapOf(
  "\u0007" to "a", "\b" to "b", "\u000c" to "f", "\n" to "n",
  "\r" to "r", "\t" to "t", "\u000b" to "v", "\"" to "\"", "\\" to "\\",
)

var QuoteSymbol = intern("quote", immutable=true)
var UnquoteSymbol = intern("unquote", immutable=true)
var QuasiquoteSymbol = intern("quasiquote", immutable=true)
var UnquoteSplicingSymbol = intern("unquote-splicing")
var FunctionSymbol = intern("function", immutable=true)

fun closingOf(opening: Char): Char {
    // Return the closing character for the opening character. For brackets
    // of any kind it is the matching opposite, for others it is the same
    // character.
    var matching_bracket = mapOf(
        "{" to "}", "[" to "]", "(" to ")", "<" to ">"
    )
    return matching_bracket[opening] ?: opening
}


fun the_int(s: String, radix = 10): Int? {
    try {
        return s.toInt(radix)
    } catch (e: NumberFormatException) {
        return null
    }
}

class Reader(val input: Stream, val sourceName: String): LocationHolder {
    // This is a Lisp reader that on each call to read() returns an Objects as
    // found in the input stream, as long as it finds one. Then it returns nil,
    // meaning the input is read to the end.
    
    var line = 1                        // current line
    var column = 0                      // current column read
    var pushbackToken: ReaderToken? = null
    

    fun location(): String {
        return "$sourceName:$line:$column"
    }

    fun unreadToken(token: ReaderToken) {
        if (pushbackToken != null) {
            // InternalReaderError
            throw InternalReaderError("pushbackToken $existing exists", this)
        }
        pushbackToken = token
    }
    
    fun nextChar(): Char? {
        try {
            val ch = input.readchar()
            if (ch != null) {
                if (ch == '\n') {
                    line += 1
                    column = 0
                } else {
                    column += 1
                }
                return ch
            }
        } catch (e: Exception) {
            // IOError
            throw Exception("reading character from $input: #error")
        }
        return null
    }
    
    fun unreadChar(ch: Char) {
        if (ch == '\n') {
            line -= 1                   // this will never happen, right?
        } else {
            column -= 1
        }
        input.unreadChar(ch)
    }
    
    fun nextNonCommentChar(): Char? {
        // Return the next char that is not in a comment. Actually, return a
        // space for a comment, so we have a delimiter.
        var in_comment = false
        while (true) {
            ch = nextChar()
            if (ch == null) {
                return null
            }
            if (in_comment) {
                if (ch == '\n') {
                    return ' '
                }
            } else if (ch == commentChar) {
                in_comment = true
                continue
            } else {
                return ch
            }
        }
        return null
    }
    
    fun nextNonSpaceChar(): Char? {
        // Return the next char that is not whitespace or in a comment.
        var ch: Char? = null
        while (true) {
            ch = nextNonCommentChar()
            if (ch == null) {
                return null
            }
            if (ch.isWhitespace) {
                continue
            }
            return ch
        }
        return null
    }
    
    fun nextToken(): ReaderToken {
        // Deliver the next token from the input stream. This may be a token
        // that has been unread before.

        var token = pushbackToken
        if (token != null) {
            pushbackToken = null
            return token
        }

        var ch: Char? = null
        while (true) {
            ch = nextNonSpaceChar()
            if (ch == null) {
                return EOFToken(this)
            }
            when (ch) {
                '(' -> return OparenToken(this)
                ')' -> return CparenToken(this)
                '.' -> return PeriodToken(this)
                '\'' -> return QuoteToken(this)
                '`' -> return QuasiquoteToken(this)
                '\"' -> return readStringToken(this)
                ',' -> {
                    val ch2: Char? = null
                    ch2 = nextChar()
                    if (ch2 != null) {
                        if (ch2 == '@') {
                            return UnquoteSplicingToken(this)
                        }
                        unreadChar(ch2)
                    }
                    return UnquoteToken(this)
                }
                '#' -> {
                    val token = octothorpeMacro()
                    if (token != null) {
                        return token
                    }
                    continue                // may also be #! in line 1
                }
                else -> break
            }
            unreadChar(ch)
            return readAtomToken()  //  symbol or number
        }
        return EOFToken(this)
    }
    
    fun octothorpeMacro(): ReaderToken? {
        // Return an octothorpe token after having alread read the '#'.
        //
        // Return null if reading a '#!' in the first line.
        //
        // The type of the octothorpe token depends on the next char (*not* the
        // next non-space char!).
        var ch = nextChar()
        if (ch != null) {
            when (ch) {
                '\'' -> return FunctionToken(this)
                ':' -> return TableStartToken(this)
                '(' -> return VectorStartToken(this)
                '/' ->                   // regexp in #/.../ form
                    return readRegexp('/')
                'r' -> {                  // regexp in #r{...} form
                    var ch = nextChar()
                    if (ch != null) {
                        if (ch.isWhitespace || ch == commentChar) {
                            throw SyntaxError("regexp delimiter may not be"
                                              + " whitespace or comment sign",
                                              this)
                        }
                        return readRegexp(Reader.closingOf(ch))
                    } else {
                        throw ParseError("unexpected EOF after '#r'", this)
                    }
                }
                '<' -> throw SyntaxError("unreadable #<...> object", this)
                '!' -> {
                    if (line == 1) {
                        skipRestOfLine()
                        return null
                    }
                }
                'O', 'o' ->
                    return NumberToken(this, readRadixNumber(8))
                'B', 'b' ->
                    return NumberToken(this, readRadixNumber(2))
                'X', 'x' ->
                    return NumberToken(this, readRadixNumber(16))
                '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' -> {
                    unreadChar(ch)
                    return NumberToken(this, readFreeRadixNumber())
                }
            else -> break
            }
            throw SyntaxError("unexpected char `\(ch)` after '#'", this)
        }
        throw ParseError("EOF after '#'", this)
    }

    fun readFreeRadixNumber(): Double {
        // #25R-7H and the like, radix first, in decimal
        var digit_value = mapOf(
            '0' to 0, '1' to 1, '2' to 2, '3' to 3, '4' to 4,
            '5' to 5, '6' to 6, '7' to 7, '8' to 8, '9' to 9
        )
        var radix: Int = 0

        while (true) {
            val ch = nextChar()
            var value = digit_value[ch]
            if (value != null) {
                radix = radix * 10 + value
            } else if (ch in "rR") {
                break
            } else {
                throw SyntaxError("unexpected char reading integer radix: $ch",
                                  this)
            }
        }
        if (radix <= 36 && radix > 0) {
            return readRadixNumber(radix)
        }
        throw ValueError("invalid number radix \(radix)")
    }

    fun readRadixNumber(radix: Int): Double {
        var digits: MutableList<Char> = mutableListOf()
        var sign = 1
        var first = true
        var ch: Char? = null

        while (true) {
            ch = nextChar()
            if (ch == null) {
                break
            }
            if (first) {
                first = false
                when (ch) {
                    '-' ->{
                        sign = -1
                        continue
                    }
                    '+' -> continue
                    else -> break
                }
            }
            if (ch.isWhitespace || delimiter_chars.contains(ch)) {
                unreadChar(ch)
                break
            }
            digits.append(ch)
        }
        val digs = String(digits)
        try {
            return Double(digs.toInt() * sign)
        } catch (nfe: NumberFormatException) {
            throw SyntaxError("not a number of base $radix: $digs", this)
        }
    }
    

    fun readAtomToken(): ReaderToken {
        // Read an atom (symbol or number) and return it as a ReaderToken.
        // 
        // Barred can stop or begin anywhere! Also, backslash escapes. Holy
        // bovine. Sounds like it's time for a table-based approach again. Hah!
        // Finally!

        // Maybe pulling the constant setup out to the top level could make this
        // faster. See later if it is worth the price.
        
        enum class CC(val v: Int) {   // character class
            Bar(0),            // vertical bar
            Backsl(1),         // a backslash
            Delim(2),          // whitespace or some other delimiter
            Member(3)          // potential member of name or number
        }

        fun charclass(ch: Char?): CC {
            // Return the class of a character.
            if (ch != null) {
                if (ch == '|') { return CC.Bar }
                if (ch == '\\') { return CC.Backsl }
                if (ch.isWhitespace || delimiter_chars.contains(ch)) {
                    return CC.Delim
                }
                return CC.Member
            }
            return CC.Delim
        }

        enum class St(val v: Int) { // states
            initial(0),    // normal, nothing remarkable about this
            normesc(1),    // escaped from initial state
            barred(2),     // after a Bar (vs. *in* a bar)
            barresc(3),    // escaped from barred state
            done(4)        // done with everything (crash otherwise)
        }

        enum class Ac {                       // actions
            none,                   // do nothing except maybe change state
            collect,                // collect character
            membar,                 // remember being barred
            finish                 // unread character and return result
        }
        
        // 'Tis but a small table setup, luckily.

        // state transition table, by state (vertical) and cclass
        var newstate = [
            // Bar        Backsl      Delim       Member
            [ St.barred,  St.normesc, St.done,    St.initial ], // initial
            [ St.initial, St.initial, St.initial, St.initial ], // normesc
            [ St.initial, St.barresc, St.barred,  St.barred  ], // barred
            [ St.barred,  St.barred,  St.barred,  St.barred  ]  // barresc
        ]
        
        // action table, by state (vertical) and cclass
        var action = [
            // Bar        Backsl      Delim       Member
            [ Ac.membar,  Ac.none,    Ac.finish,  Ac.collect ], // initial
            [ Ac.collect, Ac.collect, Ac.collect, Ac.collect ], // normesc
            [ Ac.none,    Ac.none,    Ac.collect, Ac.collect ], // barred
            [ Ac.collect, Ac.collect, Ac.collect, Ac.collect ]  // barresc
        ]
        
        var was_barred = false          // was barred at some point => no number
        var collected: MutableList<Char> = []
        var the_state = St.initial

        while (the_state != St.final) {
            var ch = nextChar()
            if (ch == null) {
                break
            }
            var cclass = charclass(ch)
            var act = action[the_state.rawValue][cclass.rawValue]
            // print("next char is `\(ch)`, state \(the_state) cclass \(cclass)"
            //         + " action \(act)")

            when (act) {
                Ac.none    -> break
                Ac.collect -> collected.append(ch)
                Ac.membar  -> was_barred = true
                Ac.finish  -> unreadChar(ch)
            }
            the_state = newstate[the_state.rawValue][cclass.rawValue]
        }

        var result = String(collected)
        if (was_barred) {
            return SymbolToken(this, result)
        }

        var num = the_int(result)
        if (num != null) {
            return NumberToken(this, Double(num))
        }
        if (result.startsWith("0o")) {
            var result = result
            result.removeFirst(2)
            var num = the_int(result, 8)
            if (num != 0) {
                return NumberToken(this, Double(num))
            }
        }
        if (result.startsWith("0b")) {
            var result = result
            result.removeFirst(2)
            var num = the_int(result, 2)
            if (num != null) {
                return NumberToken(this, Double(num))
            }
        }
        var dnum = Double(result)
        if (dnum != null) {
            return NumberToken(this, dnum)
        }
        return SymbolToken(this, result)
    }

    fun read_stringlike(regexpp: Bool, endChar: Char): String {
        // Read the contents of a regexp or string.
        //
        // whatami is the name of the thing being read, for messages; endChar
        // is the char that stops the reading. Return the string contents.
        var whatami = ["string", "regexp"][if (regexpp) 1 else 0]
        var octaldigits = "01234567"
        var hexdigits = "0123456789abcdefABCDEF" // just to check membership!
        var hexdigit_keys = "xuU"
        var n_hexdigits = [ 'x' to 2, 'u' to 4, 'U' to 8 ]

        fun parse_octaldigits(first: Char): Char {
            // Read octal digits and return the number value
            // Other than with hexdigits, the octal digits string is not fixed
            // in length. There is at least one (the first, which we have read
            // already) and at most three, so we may now read up to two more.
            // Any character that is not an octal digit ends the sequence.
            var digits: MutableList<Char> = [first]
            for (_ in 1..3) {
                var digit = nextChar()
                if (digit == null) {
                    throw ParseError("unexpected EOF in string (octal)", this)
                }
                if (digit !in octaldigits) {
                    unreadChar(digit)
                    break
                }
                digits.append(digit)
            }
            var cvalue = the_int(String(digits), 8)
            if (cvalue != null) {
                return cvalue.toChar()
            }
            throw SyntaxError("cannot convert octal $digits to char",
                              this)
        }

        fun parse_hexdigits(ndigits: Int): Char {
            // Read a number of hex digits and return the number value.
            var digits: MutableList<Char> = []
            for (_ in 1..ndigits) {
                var digit = nextChar()
                if (digit == null) {
                    throw ParseError("unexpected EOF in string (hex)", this)
                }
                if (digit !in hexdigits) {
                    throw SyntaxError("invalid hex digit '$digit' in"
                                   + " $whatami literal", this)
                }
                digits.append(digit)
            }
            var cvalue = the_int(String(digits), 16)
            if (cvalue != null) {
                return cvalue.Char()
            }
            throw SyntaxError("cannot convert hex '${String(digits)}' to char",
                              this)
        }
                

        var result: MutableList<Char> = []
        var after_backslash = false
        while (true) {
            var ch = nextChar()
            if (ch == null) {
                throw ParseError("EOF in $whatami literal", this)
            }
            var to_append = ch
            if (after_backslash) {
                // Now this is a bit tricky, as backslashes are for some
                // part used to escape regpexp specials chars, as * or +,
                // and for another to escape those that are special chars in
                // strings. Holy bovine, she is dumping!
                if (ch in escape2specialChar.keys) {
                    to_append = escape2specialChar.getValue(ch)
                } else if (ch in hexdigits.keys) {
                    to_append = parse_hexdigits(n_hexdigits.getValue(ch))
                } else if (ch in octaldigits) {
                    to_append = parse_octaldigits(ch)
                } else if (ch == endChar) {
                    // insert this if it is backslash-escaped
                } else if (regexpp) {  // need literal backslash after all
                    result.append('\\')
                }
                after_backslash = false
            } else {
                if (ch == '\\') {
                    after_backslash = true
                    continue
                }
                if (ch == endChar) {
                    break
                }
            }
            result.append(to_append)
        }
        return String(result)
    }
    
    fun readRegexp(endChar: Char): ReaderToken {
        // Read a regexp from the input. '#/' has already been seen.
        return RegexpToken(this, read_stringlike(regexpp = true,
                                                 endChar = endChar))
    }
    

    fun readStringToken(): ReaderToken {
        // Read a string from the input, return a StringToken."""
        return StringToken(this, read_stringlike(regexpp = false,
                                                 endChar = '\"'))
    }


    fun skipRestOfLine() {
        var ch: Char = null
        do {
            ch = nextChar()
            if (ch == '\n') {
                break
            }
        } while (ch != null)
    }


    fun read(): Object? {
        // Read an expression from the input and return it.
        var token = nextToken()
        var macroSymbol: Symbol? = null
        
        when (token) {
            is SymbolToken ->
                return intern(token.value)
            is NumberToken ->
                return makeNumber(token.value)
            is StringToken ->
                return makeString(token.value)
            is OparenToken ->
                return readList()
            is TableStartToken ->
                return readTable()
            is VectorStartToken ->
                return readVector()
            is RegexpToken ->
                return Regexp(token.value)
            is QuoteToken ->
                macroSymbol = QuoteSymbol
            is FunctionToken ->
                macroSymbol = FunctionSymbol
            is UnquoteToken ->
                macroSymbol = UnquoteSymbol
            is QuasiquoteToken ->
                macroSymbol = QuasiquoteSymbol
            is UnquoteSplicingToken ->
                macroSymbol = UnquoteSplicingSymbol
            is EOFToken ->
                return null
            is CparenToken ->
                throw SyntaxError("unexpected closing parenthesis", this)
            is PeriodToken ->
                throw SyntaxError("unexpected dot", this)
            else ->
                throw SyntaxError("unexpected $token", this)
        }
        
        if (macroSymbol != null) {
            var macroArg = read() ?:
                throw ParseError("unexpected EOF after $macroSymbol",
                                 this)
            return ListCollector(macroFunction, macroArg).list
        }
        throw InternalReaderError("must not reach this: Reader.read()")
    }
            

    fun readTable(): Object {
        // Read the body of a table. '#:' has already been read.
        var token = nextToken()
        if (token !is OparenToken) {
            throw SyntaxError("invalid $token expecting '(' in a table", this)
        }
        var lc = ListCollector()
        while (true) {
            token = nextToken()
            when (token) {
                is OparenToken ->
                    lc.append(readList())
                is CparenToken ->
                    return Table(lc.list)
                else ->
                    throw SyntaxError("invalid \(token) expecting key/value"
                                      + " pair in a table", this)
            }
        }
    }


    fun readVector(): Object {
        // Read a vector from the input and return it.
        var lc = ListCollector()
        while (true) {
            var token = nextToken()
            when (token) {
                is CparenToken ->
                    return Vector(lc.list)
                is PeriodToken ->
                    throw SyntaxError ("unexpected period in vector", this)
                is EOFToken ->
                    throw ParseError("unexpected EOF in vector", this)
                else -> {
                    unreadToken(token)
                    obj = read() ?:
                        throw ParseError("EOF when expecting vector element",
                                         this)
                    lc.append(obj)
                }
            }
        }
    }


    fun readList(): Object {
        // Read a list from the input and return it.
        var lc = ListCollector()
        while (true) {
            var token = nextToken()
            when (token) {
                is PeriodToken -> {
                    if (lc.list() == Nil) {
                        throw SyntaxError("unexpected dot at beginning of list",
                                          token)
                    }
                    var elem = read() ?:
                        throw ParseError("EOF reading list element after `.`",
                                         this)
                    lc.lastcdr(elem)
                    var next = nextToken()
                    if (next is CparenToken) {
                        return lc.list
                    } else {
                        throw SyntaxError("unexpected \(next) in list"
                                          + " where ')' expected after `.`",
                                          this)
                    }
                }
                is CparenToken ->
                    return lc.list
                is EOFToken ->
                    throw ParseError("EOF in list", this)
                else -> {
                    unreadToken(token)
                    var elem = read() ?:
                        throw ParseError("EOF reading list element", this)
                    lc.append(elem)
                }
            }
        }
    }
}

// EOF

