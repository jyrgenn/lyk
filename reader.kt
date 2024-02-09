// Reader and Readertoken (which comes first)


// Token types for the Reader. Of the most, the function is fully described by
// their type; tokens for strings, symbols, numbers, and regexps also have some
// value.

class ReaderToken(reader: Reader): LocationHolder, CustomStringConvertible {
    val sourceName: reader.sourceName
    val line: reader.line
    val column: reader.column

    fun location() = "$source:$line:$column"
    
    fun description() = "${type(this)}"
}

class OparenToken: ReaderToken {}
class CparenToken: ReaderToken {}
class PeriodToken: ReaderToken {}
class QuoteToken: ReaderToken {}
class FunctionToken: ReaderToken {}
class UnquoteToken: ReaderToken {}
class QuasiquoteToken: ReaderToken {}
class UnquoteSplicingToken: ReaderToken {}
class StringToken(reader: Reader, value: String): ReaderToken {
    override fun description() = super.description + "(\(value))"
}
class SymbolToken: StringToken {}
class NumberToken(reader: Reader, value: Double): ReaderToken {
    override fun description() = super.description + "(\(value))"
}
class TableStartToken: ReaderToken {}
class VectorStartToken: ReaderToken {}
class RegexpToken: StringToken {}
class EOFToken: ReaderToken {}


val commentChar = ';'

// special characters to be escaped in string literals and similar items

// backward: the escaped char after a backslash => real char
val escape2specialChar = mapOf(
  "a" to "\u0007", "b" to "\b", "f" to "\u000c", "n" to "\n",
  "r" to "\r", "t" to "\t", "v" to "\u000b", "\"" to "\"", "\\" to "\\",
)
var specialChar2escaped = [
  "\u0007" to "a", "\b" to "b", "\u000c" to "f", "\n" to "n",
  "\r" to "r", "\t" to "t", "\u000b" to "v", "\"" to "\"", "\\" to "\\",
]

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


class Reader(input: Stream, sourceName: String): LocationHolder {
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
            throw Exception("pushbackToken $existing exists",
                            self, // #function, #file, #line
            )
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
        var digit_value: [Char: Int] = [
            '0' to 0, '1' to 1, '2' to 2, '3' to 3, '4' to 4,
            '5' to 5, '6' to 6, '7' to 7, '8' to 8, '9' to 9
        ]
        var radix: Int = 0
        
        while var ch = try nextChar() {
            if var value = digit_value[ch] {
                radix = radix * 10 + value
            } else if "rR".contains(ch) {
                break
            } else {
                throw SyntaxError("unexpected char reading integer radix:"
                                    + " `\(ch)`", this)
            }
        }
        if radix <= 36 && radix > 0 {
            return try readRadixNumber(radix)
        }
        throw ValueError("invalid number radix \(radix)")
    }

    fun readRadixNumber(radix: Int): Double {
        var digits: [Char] = []
        var sign = 1
        var first = true

        while var ch = try nextChar() {
            if first {
                first = false
                switch ch {
                case '-': sign = -1; continue
                case '+': continue
                default: break
                }
            }
            if ch.isWhitespace || delimiter_chars.contains(ch) {
                unreadChar(ch)
                break
            }
            digits.append(ch)
        }
        var digs = String(digits)
        if var value = Int(digs, radix: radix) {
            return Double(value * sign)
        }
        throw SyntaxError("not a number of base \(radix): \(digs)", this)
    }
    

    fun readAtomToken(): ReaderToken {
        // Read an atom (symbol or number) and return it as a ReaderToken.
        // 
        // Barred can stop or begin anywhere! Also, backslash escapes. Holy
        // bovine. Sounds like it's time for a table-based approach again. Hah!
        // Finally!

        // Maybe pulling the constant setup out to the top level could make this
        // faster. See later if it is worth the price.
        
        enum CC: Int {                  // character class
            case Bar = 0                // vertical bar
            case Backsl                 // a backslash
            case Delim                  // whitespace or some other delimiter
            case Member                 // potential member of name or number
        }

        fun charclass(ch: Char?): CC {
            // Return the class of a character.
            if var ch = ch {
                if ch == '|' { return CC.Bar }
                if ch == '\\' { return CC.Backsl }
                if ch.isWhitespace || delimiter_chars.contains(ch) {
                    return CC.Delim
                }
                return CC.Member
            }
            return CC.Delim
        }

        enum St: Int {                 // states
            case initial = 0           // normal, nothing remarkable about this
            case normesc               // escaped from initial state
            case barred                // after a Bar (vs. *in* a bar)
            case barresc               // escaped from barred state
            case final                 // done with everything (crash otherwise)
        }

        enum Ac {                       // actions
            case none                   // do nothing except maybe change state
            case collect                // collect character
            case membar                 // remember being barred
            case finish                 // unread character and return result
        }
        
        // 'Tis but a small table setup, luckily.

        // state transition table, by state (vertical) and cclass
        var newstate = [
            // Bar        Backsl      Delim       Member
            [ St.barred,  St.normesc, St.final,   St.initial ], // initial
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
        var collected: [Char] = []
        var the_state = St.initial

        while the_state != St.final, var ch = try nextChar() {
            var cclass = charclass(ch)
            var act = action[the_state.rawValue][cclass.rawValue]
            // print("next char is `\(ch)`, state \(the_state) cclass \(cclass)"
            //         + " action \(act)")

            switch act {
            case Ac.none:
                break
            case Ac.collect:
                collected.append(ch)
            case Ac.membar:
                was_barred = true
            case Ac.finish:
                unreadChar(ch)
            }
            the_state = newstate[the_state.rawValue][cclass.rawValue]
        }

        var result = String(collected)
        if was_barred {
            return SymbolToken(this, value: result)
        }

        if var num = Int(result) {
            return NumberToken(this, value: Double(num))
        }
        if result.hasPrefix("0o") {
            var result = result
            result.removeFirst(2)
            if var num = Int(result, radix: 8) {
                return NumberToken(this, value: Double(num))
            }
        }
        if result.hasPrefix("0b") {
            var result = result
            result.removeFirst(2)
            if var num = Int(result, radix: 2) {
                return NumberToken(this, value: Double(num))
            }
        }
        if var num = Double(result) {
            return NumberToken(this, value: num)
        }
        return SymbolToken(this, value: result)
    }

    fun read_stringlike(regexpp: Bool, endChar: Char): String {
        // Read the contents of a regexp or string.
        //
        // whatami is the name of the thing being read, for messages; endChar
        // is the char that stops the reading. Return the string contents.
        var whatami = ["string", "regexp"][regexpp ? 1 : 0]
        var octaldigits = "01234567"
        var hexdigits = "0123456789abcdefABCDEF" // just to check membership!
        var n_hexdigits: [Char: Int] = [ 'x' to 2, 'u' to 4, 'U' to 8 ]

        fun parse_octaldigits(first: Char): Char {
            // Read octal digits and return the number value
            // Other than with hexdigits, the octal digits string is not fixed
            // in length. There is at least one (the first, which we have read
            // already) and at most three, so we may now read up to two more.
            // Any character that is not an octal digit ends the sequence.
            var digits = [first]
            for _ in 1...3 {
                guard var digit = try nextChar() else {
                    throw ParseError("unexpected EOF in string (octal)", this)
                }
                if !octaldigits.contains(digit) {
                    unreadChar(digit)
                    break
                }
                digits.append(digit)
            }
            if var cvalue = Int(String(digits), radix: 8) {
                if var scalar = Unicode.Scalar(cvalue) {
                    return Char(scalar)
                }
            }
            throw SyntaxError("cannot convert octal \(String(digits)) to char",
                              this)
        }

        fun parse_hexdigits(ndigits: Int): Char {
            // Read a number of hex digits and return the number value.
            var digits: [Char] = []
            for _ in 1...ndigits {
                guard var digit = try nextChar() else {
                    throw ParseError("unexpected EOF in string (hex)", this)
                }
                if !hexdigits.contains(digit) {
                    throw SyntaxError("invalid hex digit \(digit) in"
                                   + " \(whatami) literal", this)
                    // raise PyleSyntaxError(
                    //     this,
                    // "invalid hex digit in {} literal".format(whatami),
                    //     digit)
                }
                digits.append(digit)
            }
            if var cvalue = (Int(String(digits), radix: 16)) {
                if var scalar = Unicode.Scalar(cvalue) {
                    return Char(scalar)
                }
            }
            throw SyntaxError("cannot convert hex \(String(digits)) to char",
                              this)
        }
                

        var result: [Char] = []
        var after_backslash = false
        while true {
            if var ch = try nextChar() {
                var to_append = ch
                if after_backslash {
                    // Now this is a bit tricky, as backslashes are for some
                    // part used to escape regpexp specials chars, as * or +,
                    // and for another to escape those that are special chars in
                    // strings. Holy bovine, she is dumping!
                    if var c = escape2specialChar[ch] {
                        to_append = c
                    } else if var n = n_hexdigits[ch] {
                        to_append = try parse_hexdigits(ndigits: n)
                    } else if octaldigits.contains(ch) {
                        to_append = try parse_octaldigits(first: ch)
                    } else if ch == endChar {
                        // insert this if it is backslash-escaped
                    } else if regexpp {  // need literal backslash after all
                        result.append('\\')
                    }
                    after_backslash = false
                } else {
                    if ch == '\\' {
                        after_backslash = true
                        continue
                    }
                    if ch == endChar {
                        break
                    }
                }
                result.append(to_append)
            } else {
                throw ParseError("EOF in \(whatami) literal", this)
            }
        }
        return String(result)
    }
    
    fun readRegexp(endChar: Char): ReaderToken {
        // Read a regexp from the input. '#/' has already been seen.
        return RegexpToken(this,
                           value: try read_stringlike(regexpp: true,
                                                  endChar: endChar))
    }
    

    fun readStringToken(): ReaderToken {
        // Read a string from the input, return a StringToken."""
        return StringToken(this,
                           value: try read_stringlike(regexpp: false,
                                                  endChar: '\"'))
    }


    fun skipRestOfLine() {
        while var ch = try nextChar() {
            if ch == '\n' {
                break
            }
        }
    }


    fun read(): Object? {
        // Read an expression from the input and return it.
        do {
            var token = try nextToken()
            var macroSymbol: Symbol?
            
            switch token {
            case is SymbolToken:
                return intern((token as! SymbolToken).value)
            case is NumberToken:
                return makeNumber((token as! NumberToken).value)
            case is StringToken:
                return makeString((token as! StringToken).value)
            case is OparenToken:
                return try readList()
            case is TableStartToken:
                return try readTable()
            case is VectorStartToken:
                return try readVector()
            case is RegexpToken:
                return try Regexp((token as! RegexpToken).value)
            case is QuoteToken:
                macroSymbol = QuoteSymbol
            case is FunctionToken:
                macroSymbol = FunctionSymbol
            case is UnquoteToken:
                macroSymbol = UnquoteSymbol
            case is QuasiquoteToken:
                macroSymbol = QuasiquoteSymbol
            case is UnquoteSplicingToken:
                macroSymbol = UnquoteSplicingSymbol
            case is EOFToken:
                return null
            case is CparenToken:
                throw SyntaxError("unexpected closing parenthesis", this)
            case is PeriodToken:
                throw SyntaxError("unexpected dot", this)
            default:
                throw SyntaxError("unexpected \(token)", this)
            }
            
            if var macroFunction = macroSymbol {
                guard var macroArg = try read() else {
                    throw ParseError("unexpected EOF after `\(macroFunction)",
                                     this)
                }
                return ListCollector(macroFunction, macroArg).list
            }
            throw InternalReaderError("must not reach this", this, #function,
                                      #file, #line)
                                      
        } catch {
            throw error
        }
    }
            

    fun readTable(): Object {
        // Read the body of a table. '#:' has already been read.
        var token = try nextToken()
        if !(token is OparenToken) {
            throw SyntaxError("invalid \(token) expecting '(' in a table", this)
        }
        var lc = ListCollector()
        while true {
            token = try nextToken()
            switch token {
            case is OparenToken:
                lc.append(try readList())
            case is CparenToken:
                return try Table(lc.list)
            default:
                throw SyntaxError("invalid \(token) expecting key/value"
                               + " pair in a table", this)
            }
        }
    }


    fun readVector(): Object {
        // Read a vector from the input and return it.
        var lc = ListCollector()
        while true {
            var token = try nextToken()
            switch token {
            case is CparenToken:
                return Vector(lc.list)
            case is PeriodToken:
                throw SyntaxError ("unexpected period in vector", this)
            case is EOFToken:
                throw ParseError("unexpected EOF in vector", this)
            default:
                try unreadToken(token)
                guard var obj = try read() else {
                    throw ParseError("EOF when expecting vector element", this)
                }
                lc.append(obj)
            }
        }
    }


    fun readList(): Object {
        // Read a list from the input and return it.
        var lc = ListCollector()
        while true {
            var token = try nextToken()
            switch token {
            case is PeriodToken:
                if lc.list === Null {
                    throw SyntaxError("unexpected dot at beginning of list",
                                      token)
                }
                guard var elem = try read() else {
                    throw ParseError("EOF reading list element after `.`", this)
                }
                lc.lastcdr(elem)
                var next = try nextToken()
                if next is CparenToken {
                    return lc.list
                } else {
                    throw SyntaxError("unexpected \(next) in list"
                                        + " where ')' expected after `.`", this)
                }
            case is CparenToken:
                return lc.list
            case is EOFToken:
                throw ParseError("EOF in list", this)
            default:
                try unreadToken(token)
                guard var elem = try read() else {
                    throw ParseError("EOF reading list element", this)
                }
                lc.append(elem)
            }
        }
    }
}

// EOF

