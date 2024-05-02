// the Lisp Reader

package org.w21.lyk


val commentChar = ';'

// special characters to be escaped in string literals and similar items

// backward: the escaped char after a backslash => real char
val escape2specialChar = mapOf(
  'a' to '\u0007', 'b' to '\b', 'f' to '\u000c', 'n' to '\n',
  'r' to '\r', 't' to '\t', 'v' to '\u000b', '\"' to '\"', '\\' to '\\',
)
val specialChar2escaped = mapOf(
  '\u0007' to 'a', '\b' to 'b', '\u000c' to 'f', '\n' to 'n',
  '\r' to 'r', '\t' to 't', '\u000b' to 'v', '\"' to '\"', '\\' to '\\',
)

// characters denoting the end of (most) tokens, besides whitespace
val delimiter_chars = "(),'`\""

val QuoteSymbol = intern("quote", true)
val UnquoteSymbol = intern("unquote", true)
val QuasiquoteSymbol = intern("quasiquote", true)
val UnquoteSplicingSymbol = intern("unquote-splicing")
val FunctionSymbol = intern("function", true)

fun closingOf(opening: Char): Char {
    // Return the closing character for the opening character. For brackets
    // of any kind it is the matching opposite, for others it is the same
    // character.
    val matching_bracket = mapOf(
        '{' to '}', '[' to ']', '(' to ')', '<' to '>'
    )
    return matching_bracket[opening] ?: opening
}

fun the_int(s: String, radix: Int = 10): Long? {
    try {
        return s.toLong(radix)
    } catch (e: NumberFormatException) {
        return null
    }
}

fun the_double(s: String): Double? {
    try {
        return s.toDouble()
    } catch (e: NumberFormatException) {
        return null
    }
}


class Reader(val input: LStream, sourceName: String? = null): LocationHolder
{
    // This is a Lisp reader that on each call to read() returns an Objects as
    // found in the input stream, as long as it finds one. Then it returns null,
    // meaning the input is read to the end.

    var line = 1                        // current line
    var column = 0                      // current column read
    var pushbackToken: ReaderToken? = null
    val readerName = sourceName ?: input.name
    

    override fun toString(): String {
        return "#<Reader:$readerName>"
    }
    fun desc() = toString()

    override fun location(): String {
        return input.read_location()
    }

    fun unreadToken(token: ReaderToken) {
        if (pushbackToken != null) {
            // InternalReaderError
            throw InternalReaderError("pushbackToken $pushbackToken exists",
                                      this)
        }
        debug(debugReaderSym) {
            "push back token $token"
        }
        pushbackToken = token
    }
    
    fun nextChar(): Char? {
        try {
            val ch = input.readChar()
            if (ch != null) {
                if (ch == '\n') {
                    line += 1
                    column = 0
                } else {
                    column += 1
                }
                debug(debugReaderSym) {
                    "nextChar() from $input returns '$ch'"
                }
                return ch
            }
        } catch (e: Exception) {
            throw IOError(e, "reading character from $input")
        }
        debug(debugReaderSym) {
            "nextChar() from $input returns null"
        }
        return null
    }
    
    fun unreadChar(ch: Char) {
        if (ch == '\n') {
            line -= 1                   // this will never happen, right?
        } else {
            column -= 1
        }
        debug(debugReaderSym) {
            "unreadChar('$ch')"
        }
        input.unreadChar(ch)
    }
    
    fun nextNonCommentChar(): Char? {
        // Return the next char that is not in a comment. Actually, return a
        // space for a comment, so we have a delimiter.
        var in_comment = false
        while (true) {
            val ch = nextChar()
            if (ch == null) {
                debug(debugReaderSym) {
                     "nextNonCommentChar() returns null"
                }
                return null
            }
            if (in_comment) {
                if (ch == '\n') {
                    debug(debugReaderSym) {
                         "nextNonCommentChar() returns ' '"
                    }
                    return ' '
                }
            } else if (ch == commentChar) {
                in_comment = true
                continue
            } else {
                debug(debugReaderSym) {
                     "nextNonCommentChar() returns '$ch'"
                }
                return ch
            }
        }
    }
    
    fun nextNonSpaceChar(): Char? {
        // Return the next char that is not whitespace or in a comment.
        var ch: Char?
        while (true) {
            ch = nextNonCommentChar()
            if (ch == null) {
                debug(debugReaderSym) {
                     "nextNonSpaceChar() returns null"
                }
                return null
            }
            if (ch.isWhitespace()) {
                continue
            }
            debug(debugReaderSym) {
                 "nextNonSpaceChar() returns '$ch'"
            }
            input.setPrompt("")
            return ch
        }
    }
    
    fun nextToken(): ReaderToken {
        // Deliver the next token from the input stream. This may be a token
        // that has been unread before.

        if (pushbackToken != null) {
            val token = pushbackToken!!
            pushbackToken = null
            return token
        }

        var ch: Char?
        while (true) {
            ch = nextNonSpaceChar()
            if (ch == null) {
                return EOFToken(this)
            }
            when (ch) {
                '(' -> return OparenToken(this)
                ')' -> return CparenToken(this)
                '.' -> {
                    // This is only a period token if there is a
                    // separator behind it; floats and symbols may
                    // begin with a '.'
                    val next = nextChar() ?: return PeriodToken(this)
                    unreadChar(next)
                    when (charclass(next)) {
                        CC.Member, CC.Bar -> {
                            return readAtomToken(ch)
                        }
                        else -> return PeriodToken(this)
                    }
                }
                '\'' -> return QuoteToken(this)
                '`' -> return QuasiquoteToken(this)
                '\"' -> return readStringToken()
                ',' -> {
                    val ch2: Char? = nextChar()
                    if (ch2 != null) {
                        if (ch2 == '@') {
                            return UnquoteSplicingToken(this)
                        }
                        unreadChar(ch2)
                    }
                    return UnquoteToken(this)
                }
                '#' -> {
                    val t = octothorpeMacro()
                    if (t != null) {
                        return t
                    }
                    continue                // may also be #! in line 1
                }
                else -> {
                    unreadChar(ch)
                    return readAtomToken()  //  symbol or number
                }
            }
        }
    }
    
    fun octothorpeMacro(): ReaderToken? {
        // Return an octothorpe token after having alread read the '#'.
        //
        // Return null if reading a '#!' in the first line.
        //
        // The type of the octothorpe token depends on the next char (*not* the
        // next non-space char!).
        val ch = nextChar()
        if (ch != null) {
            when (ch) {
                '\'' -> return FunctionToken(this)
                ':' -> return TableStartToken(this)
                '(' -> return VectorStartToken(this)
                '/' ->                   // regexp in #/.../ form
                    return readRegexp('/')
                'r' -> {                  // regexp in #r{...} form
                    val c = nextChar()
                    if (c != null) {
                        if (c.isWhitespace() || c == commentChar) {
                            throw SyntaxError("regexp delimiter may not be"
                                              + " whitespace or comment sign",
                                              this)
                        }
                        return readRegexp(closingOf(c))
                    } else {
                        throw SyntaxError("unexpected EOF after '#r'", this)
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
                    return NumberToken(this, readRadixNumber(8, "#$ch"))
                'B', 'b' ->
                    return NumberToken(this, readRadixNumber(2, "#$ch"))
                'X', 'x' ->
                    return NumberToken(this, readRadixNumber(16, "#$ch"))
                '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' -> {
                    unreadChar(ch)
                    return NumberToken(this, readFreeRadixNumber())
                }
                '\\' -> return charLiteralToken()
                else -> {}
            }
            throw SyntaxError("unexpected char `$ch` after '#'", this)
        }
        throw SyntaxError("EOF after '#'", this)
    }

    fun charLiteralToken(): ReaderToken {
        // we have seen the #\ beginning, now read the rest of the token

        val cb = CharBuf()

        while (true) {
            val ch = nextChar()
            if (ch == null) {
                if (cb.size == 0) {
                    throw SyntaxError("unexpected EOF reading"
                                      + " character literal", this)
                }
                break
            }
            if (!(ch.isLetter() || ch.isDigit())) {
                if (cb.size > 0) {
                    unreadChar(ch)
                } else {
                    cb.add(ch)
                }
                break
            } else {
                cb.add(ch)
            }
        }

        val charName = cb.toString()
        val len = charName.length

	// Down from here we have a character with a symbol-like name or
	// hexadecimal code.

	// defined character literal names first
        val char = LChar.nameChar[charName.lowercase()]
        if (char != null) {
            return CharToken(this, char)
        }

        // if the name of the character has length one, it is
        // literally the character itself
        if (len == 1) {
            return CharToken(this, charName[0])
        }
        val radix = charName[0]
        val code: Int
        if ((len == 3 && radix in "xX")
                || (len == 5 && radix == 'u')
                || (len == 9 && radix == 'U')) {
            code = charName.substring(1).toInt(16)
        } else if (radix in "bB") {
            code = charName.substring(1).toInt(2)
        } else if (radix in "01234567") {
            code = charName.toInt(8)
        } else {
            debug (debugReadCharSym) {
                debug_out.print("token len $len: ")
                for (i in 0..<len) {
                    debug_out.print("%02x ".format(charName[i].code))
                }
                "($charName)"
            }
            throw SyntaxError("invalid character literal: #\\$charName",
                              this)
        }
        return CharToken(this, code.toChar())
    }

    fun readFreeRadixNumber(): Double {
        // #25R-7H and the like, radix first, in decimal
        val digit_value = mapOf(
            '0' to 0, '1' to 1, '2' to 2, '3' to 3, '4' to 4,
            '5' to 5, '6' to 6, '7' to 7, '8' to 8, '9' to 9
        )
        var radix: Int = 0
        var ch: Char?

        while (true) {
            ch = nextChar()
            if (ch == null) {
                throw SyntaxError("unexpected EOF reading integer radix", this)
            }
            val value = digit_value[ch]
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
            return readRadixNumber(radix, "#$radix$ch")
        }
        throw ValueError("invalid number radix $radix", this)
    }

    fun readRadixNumber(radix: Int, prefix: String): Double {
        val digits = CharBuf()
        var sign = 1
        var first = true
        var ch: Char?
        var signch: Char? = null

        while (true) {
            ch = nextChar()
            if (ch == null) {
                break
            }
            if (first) {
                first = false
                when (ch) {
                    '-' -> { signch = ch; sign = -1; continue }
                    '+' -> { signch = ch; continue }
                    else -> {}
                }
            }
            if (ch.isWhitespace() || ch in delimiter_chars) {
                unreadChar(ch)
                break
            }
            digits.add(ch)
        }
        val digs = digits.toString()
        try {
            return digs.toLong(radix).toDouble() * sign
        } catch (nfe: NumberFormatException) {
            throw SyntaxError("not a number of base $radix: "
                              + "`$prefix${signch ?: ""}$digs`", this)
        }
    }

    // A few enums for readAtomToken(); these cannot be declared in a function,
    // sadly.

    enum class CC {                     // character class
        Bar,                            // vertical bar
        Backsl,                         // a backslash
        Delim,                          // whitespace or some other delimiter
        Member                          // potential member of name or number
    }

    enum class St {                    // states
        initial,                       // normal, nothing remarkable about this
        normesc,                       // escaped from initial state
        barred,                        // after a Bar (vs. *in* a bar)
        barresc,                       // escaped from barred state
        done                           // done with everything (crash otherwise)
    }

    enum class Ac {                     // actions; don't need a value here
        none,                           // do nothing except maybe change state
        collect,                        // collect character
        membar,                         // remember being barred
        finish                          // unread character and return result
    }

    fun charclass(ch: Char?): CC {
        // Return the class of a character.
        if (ch != null) {
            if (ch == '|') { return CC.Bar }
            if (ch == '\\') { return CC.Backsl }
            if (ch.isWhitespace() || ch in delimiter_chars) {
                return CC.Delim
            }
            return CC.Member
        }
        return CC.Delim
    }

    fun readAtomToken(start: Char? = null, wantSymbol: Boolean = false
    ): ReaderToken {
        // Read an atom (symbol or number) and return it as a ReaderToken.
        // 
        // Barred can stop or begin anywhere! Also, backslash escapes. Holy
        // bovine. Sounds like it's time for a table-based approach again. Hah!
        // Finally!

        // Maybe pulling the constant setup out to the top level could make this
        // faster. See later if it is worth the price.
        

        // 'Tis but a small table setup, luckily.

        // state transition table, by state (vertical) and cclass
        val newstate = arrayOf(
            //      Bar         Backsl      Delim       Member
            arrayOf(St.barred,  St.normesc, St.done,    St.initial), // initial
            arrayOf(St.initial, St.initial, St.initial, St.initial), // normesc
            arrayOf(St.done,    St.barresc, St.barred,  St.barred ), // barred
            arrayOf(St.barred,  St.barred,  St.barred,  St.barred )  // barresc
        )
        
        // action table, by state (vertical) and cclass
        val action = arrayOf(
            //      Bar         Backsl      Delim       Member
            arrayOf(Ac.membar,  Ac.none,    Ac.finish,  Ac.collect), // initial
            arrayOf(Ac.collect, Ac.collect, Ac.collect, Ac.collect), // normesc
            arrayOf(Ac.none,    Ac.none,    Ac.collect, Ac.collect), // barred
            arrayOf(Ac.collect, Ac.collect, Ac.collect, Ac.collect)  // barresc
        )
        
        var was_barred = false          // was barred at some point => no number
        val collected = CharBuf()
        var the_state = St.initial
        var new_state: St

        if (start != null) {
            collected.add(start)
        }
        while (the_state != St.done) {
            val ch = nextChar()
            if (ch == null) {
                break
            }
            val cclass = charclass(ch)
            val act = action[the_state.ordinal][cclass.ordinal]

            new_state = newstate[the_state.ordinal][cclass.ordinal]
            debug(debugReadSymSym) {
                "state $the_state, `$ch`:$cclass act $act => $new_state"
            }
            when (act) {
                Ac.none    -> {}
                Ac.collect -> collected.add(ch)
                Ac.membar  -> was_barred = true
                Ac.finish  -> unreadChar(ch)
            }
            the_state = new_state
        }

        val result = collected.toString()
        if (was_barred || wantSymbol) {
            if (result == "" && was_barred) {
                unreadChar('|')
            }
            return SymbolToken(this, result)
        }

        val num = the_int(result)
        if (num != null) {
            return NumberToken(this, num.toDouble())
        }
        if (result.startsWith("0o")) {
            val i = the_int(result.substring(startIndex = 2), 8)
            if (i != null) {
                return NumberToken(this, i.toDouble())
            }
        }
        if (result.startsWith("0b")) {
            val i = the_int(result.substring(startIndex = 2), 2)
            if (i != null) {
                return NumberToken(this, i.toDouble())
            }
        }
        val dnum = the_double(result)
        if (dnum != null) {
            return NumberToken(this, dnum)
        }
        return SymbolToken(this, result)
    }

    fun read_stringlike(regexpp: Boolean, endChar: Char): String {
        // Read the contents of a regexp or string.
        //
        // whatami is the name of the thing being read, for messages; endChar
        // is the char that stops the reading. Return the string contents.
        val whatami = arrayOf("string", "regexp")[if (regexpp) 1 else 0]
        val octaldigits = "01234567"
        val hexdigits = "0123456789abcdefABCDEF" // just to check membership!
        // val hexdigit_keys = "xuU"
        val n_hexdigits = mapOf('x' to 2, 'u' to 4, 'U' to 8)

        fun parse_octaldigits(first: Char): Char {
            // Read octal digits and return the number value
            // Other than with hexdigits, the octal digits string is not fixed
            // in length. There is at least one (the first, which we have read
            // already) and at most three, so we may now read up to two more.
            // Any character that is not an octal digit ends the sequence.
            val digits = CharBuf(first)
            for (n in 1..3) {
                val digit = nextChar()
                if (digit == null) {
                    throw SyntaxError("unexpected EOF in string (octal)", this)
                }
                if (digit !in octaldigits) {
                    unreadChar(digit)
                    break
                }
                digits.add(digit)
            }
            val cvalue = the_int(digits.toString(), 8)
            if (cvalue != null) {
                return cvalue.toInt().toChar()
            }
            throw SyntaxError("cannot convert octal $digits to char",
                              this)
        }

        fun parse_hexdigits(ndigits: Int): Char {
            // Read a number of hex digits and return the number value.
            val digits = CharBuf()
            for (n in 1..ndigits) {
                val digit = nextChar()
                if (digit == null) {
                    throw SyntaxError("unexpected EOF in string (hex)", this)
                }
                if (digit !in hexdigits) {
                    throw SyntaxError("invalid hex digit '$digit' in"
                                   + " $whatami literal", this)
                }
                digits.add(digit)
            }
            val cvalue = the_int(digits.toString(), 16)
            if (cvalue != null) {
                return cvalue.toInt().toChar()
            }
            throw SyntaxError("cannot convert hex '${digits}' to char",
                              this)
        }
                

        val result = CharBuf()
        var after_backslash = false
        while (true) {
            val ch = nextChar()
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
                    if (regexpp && ch == '\\') {
                        result.add('\\') // need one extra for a literal one
                    }
                    to_append = escape2specialChar.get(ch) ?:
                        throw SyntaxError("invalid char in special char escape"
                                          +": $ch", this)
                } else if (ch in n_hexdigits.keys) {
                    to_append = parse_hexdigits(
                        n_hexdigits.get(ch) ?:
                            throw SyntaxError("invalid char in hexdigits escape"
                                              +": $ch", this))
                } else if (ch in octaldigits) {
                    to_append = parse_octaldigits(ch)
                } else if (ch == endChar) {
                    // insert this if it is backslash-escaped
                } else if (regexpp) {   // need literal backslash after all
                    result.add('\\')
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
            result.add(to_append)
        }
        return result.toString()
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
        var ch: Char?
        do {
            ch = nextChar()
            if (ch == '\n') {
                break
            }
        } while (ch != null)
    }


    fun read(): Pair<LObject?, String> {
        // Read an expression from the input and return it together with the
        // location of its first token.
        val token = nextToken()
        val location = token.location()
        var macroSymbol: LSymbol?
        
        when (token) {
            is SymbolToken ->
                return Pair(intern(token.value), location)
            is NumberToken ->
                return Pair(makeNumber(token.value), location)
            is StringToken ->
                return Pair(makeString(token.value), location)
            is OparenToken ->
                return Pair(readList(), location)
            is TableStartToken ->
                return Pair(readTable(), location)
            is VectorStartToken ->
                return Pair(readVector(), location)
            is RegexpToken ->
                return Pair(LRegexp(token.value), location)
            is CharToken ->
                return Pair(makeChar(token.value), location)
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
                return Pair(null, location)
            is CparenToken ->
                throw SyntaxError("unexpected closing parenthesis", this)
            is PeriodToken ->
                throw SyntaxError("unexpected dot", this)
            else ->
                throw SyntaxError("unexpected $token", this)
        }

        // arrives here only if we actually had a macroSymbol
        val macroArg = read().first ?:
            throw ParseError("unexpected EOF after $macroSymbol", this)
        return Pair(LCons(macroSymbol, LCons(macroArg, Nil)), location)
    }
            

    fun readTable(): LObject {
        // Read the body of a table. '#:' has already been read.
        var token = nextToken()
        if (token !is OparenToken) {
            throw SyntaxError("invalid $token expecting '(' in a table", this)
        }
        val lc = ListCollector()
        while (true) {
            token = nextToken()
            when (token) {
                is OparenToken ->
                    lc.add(readList())
                is CparenToken ->
                    return LTable(lc.list)
                else ->
                    throw SyntaxError("invalid $token expecting key/value"
                                      + " pair in a table", this)
            }
        }
    }


    fun readVector(): LObject {
        // Read a vector from the input and return it.
        val lc = ListCollector()
        while (true) {
            val token = nextToken()
            when (token) {
                is CparenToken ->
                    return LVector(lc.list)
                is PeriodToken ->
                    throw SyntaxError ("unexpected period in vector", this)
                is EOFToken ->
                    throw ParseError("unexpected EOF in vector", this)
                else -> {
                    unreadToken(token)
                    val obj = read().first ?:
                        throw ParseError("EOF when expecting vector element",
                                         this)
                    lc.add(obj)
                }
            }
        }
    }


    fun readList(): LObject {
        // Read a list from the input and return it.
        val lc = ListCollector()
        while (true) {
            val token = nextToken()
            when (token) {
                is PeriodToken -> {
                    if (lc.list == Nil) {
                        throw SyntaxError("unexpected dot at beginning of list",
                                          token)
                    }
                    val elem = read().first ?:
                        throw ParseError("EOF reading list element after `.`",
                                         this)
                    lc.lastcdr(elem)
                    val next = nextToken()
                    if (next is CparenToken) {
                        return lc.list
                    } else {
                        throw SyntaxError("unexpected $next in list"
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
                    val elem = read().first ?:
                        throw ParseError("EOF reading list element", this)
                    lc.add(elem)
                }
            }
        }
    }
}

// EOF

