// character builtin functions

package org.w21.lyk


/// builtin code-char
/// fun     bi_code_char
/// std     code
/// key     
/// opt     
/// rest    
/// ret     char
/// special no
/// doc {
/// Return a character with the code attribute given by `code`. If no such
/// character exists and one cannot be created, return nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_code_char(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val code = intArg(arg1(args), "code-char")
    if (code.toChar().category == kotlin.text.CharCategory.UNASSIGNED) {
        throw ArgumentError("code $code is not an assigned Unicode character")
    }
    return makeChar(code)
}

/// builtin char-int
/// fun     bi_char_int
/// std     char
/// key     
/// opt     
/// rest    
/// ret     code
/// special no
/// doc {
/// Return a non-negative integer encoding the `char` object.
/// `char` may also be a string of length 1.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_char_int(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val char = charOrStringArg(arg1(args), "char-int")
    return makeNumber(char.code)
}

/// builtin char-equal
/// fun     bi_char_equal
/// std     
/// key     
/// opt     
/// rest    characters
/// ret     t/nil
/// special no
/// doc {
/// Return true iff all characters are the same except for case.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_char_equal(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val code1 = charOrStringArg(arg1(args),
                                "char-equal first").lowercase()[0].code
    for (ch in args.cdr) {
        if (charOrStringArg(ch, "char-equal").lowercase()[0].code
                != code1) {
            return Nil
        }
    }
    return T
}

/// builtin char-not-equal
/// fun     bi_char_not_equal
/// std     
/// key     
/// opt     
/// rest    characters
/// ret     t/nil
/// special no
/// doc {
/// Return true iff all characters are unequal, without regarding case.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_char_not_equal(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val seen = mutableSetOf<Int>()

    for (ch in args) {
        val code = charOrStringArg(
            ch, "char-not-equal first").lowercase()[0].code
        if (code in seen) {
            return Nil
        }
        seen.add(code)
    }
    return T
}

/// builtin char/=
/// fun     bi_char_not_equal2
/// std     
/// key     
/// opt     
/// rest    characters
/// ret     t/nil
/// special no
/// doc {
/// Return true iff all given characters are unequal to each other.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_char_not_equal2(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    var things = mutableSetOf<Int>()
    
    for (arg in args) {
        val code = charArg(arg, "char/=").code
        if (things.contains(code)) {
            return Nil
        } else {
            things.add(code)
        }
    }
    return T
}

/// builtin char<
/// fun     bi_char_lt
/// std     
/// key     
/// opt     
/// rest    characters
/// ret     t/nil
/// special no
/// doc {
/// Return true iff the characters are monotonically increasing.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_char_lt(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (ch1, rest) = args
    if (ch1 === Nil) {
        return T
    }
    var previous: Int = charArg(ch1, "char< first").code
    for (ch in rest) {
        val code = charArg(ch, "char<").code
        if (previous < code) {
            previous = code
        } else {
            return Nil
        }
    }
    return T
}

/// builtin char<=
/// fun     bi_char_le
/// std     
/// key     
/// opt     
/// rest    characters
/// ret     t/nil
/// special no
/// doc {
/// Return true if the characters are monotonically non-decreasing.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_char_le(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (ch1, rest) = args
    if (ch1 === Nil) {
        return T
    }
    var previous: Int = charArg(ch1, "char< first").code
    for (ch in rest) {
        val code = charArg(ch, "char<").code
        if (previous <= code) {
            previous = code
        } else {
            return Nil
        }
    }
    return T
}

/// builtin char=
/// fun     bi_char_eql
/// std     
/// key     
/// opt     
/// rest    characters
/// ret     t/nil
/// special no
/// doc {
/// Return true if the characters are all equal.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_char_eql(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (ch1, rest) = args
    if (ch1 === Nil) {
        return T
    }
    var previous: Int = charArg(ch1, "char< first").code
    for (ch in rest) {
        val code = charArg(ch, "char<").code
        if (previous != code) {
            return Nil
        }
    }
    return T
}

/// builtin char>
/// fun     bi_char_gt
/// std     
/// key     
/// opt     
/// rest    characters
/// ret     t/nil
/// special no
/// doc {
/// Return true iff the characters are monotonically decreasing.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_char_gt(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (ch1, rest) = args
    if (ch1 === Nil) {
        return T
    }
    var previous: Int = charArg(ch1, "char> first").code
    for (ch in rest) {
        val code = charArg(ch, "char>").code
        if (previous > code) {
            previous = code
        } else {
            return Nil
        }
    }
    return T
}

/// builtin char>=
/// fun     bi_char_ge
/// std     
/// key     
/// opt     
/// rest    characters
/// ret     t/nil
/// special no
/// doc {
/// Return true iff the characters are monotonically not increasing.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_char_ge(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (ch1, rest) = args
    if (ch1 === Nil) {
        return T
    }
    var previous: Int = charArg(ch1, "char>= first").code
    for (ch in rest) {
        val code = charArg(ch, "char>=").code
        if (previous >= code) {
            previous = code
        } else {
            return Nil
        }
    }
    return T
}

