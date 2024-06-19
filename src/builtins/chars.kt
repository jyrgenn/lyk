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
fun bi_code_char(args: LObject, kwArgs: Map<LSymbol, LObject>,
                 suppp: Map<LSymbol, Boolean>): LObject {
    val code = intArg(arg1(args))
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
fun bi_char_int(args: LObject, kwArgs: Map<LSymbol, LObject>,
                suppp: Map<LSymbol, Boolean>): LObject {
    val char = charOrStringArg(arg1(args))
    return makeNumber(char.code)
}


/////////////////////////////
// We have three types of character comparison functions here:
// (1) check for equality: char=, char-equal
// (2) check for uniqueness: char/=, char-not-equal
// (3) check for order: char<, char<=, char>, char>=, char-greaterp,
//     char-lessp, char-not-greaterp, char-not-lessp
// We can breakfast them off with three generic function bodies that
// take the differences between the respective functions as parameters,
// which avoids nearly all code duplication between those functions.
//
//
// Here are core of the comparison functions, in which
//   args:    the original argument list
//   what:    name of the function, to be used in error messages
//   case_i:  case-insensitive iff true
//   compare: the comparison function, working on Char codes, for order

// compare for order
fun char_compare_ordered(args: LObject, case_i: Boolean,
                         compare: (code1: Int, code2: Int) -> Boolean
): LObject {
    if (args === Nil) {
        return T
    }
    val (ch1, rest) = args
    val char1 = charArg(ch1, " first").the_char
    var previous: Int = (if (case_i) char1.lowercase()[0] else char1).code

    for (arg in rest) {
        val char = charArg(arg).the_char
        val code = (if (case_i) char.lowercase()[0] else char).code
        if (compare(previous, code)) {
            previous = code
        } else {
            return Nil
        }
    }
    return T
}

// core of the comparison functions that check for uniqueness
fun char_compare_unique(args: LObject, case_i: Boolean): LObject
{
    var things = mutableSetOf<Int>()
    
    for (arg in args) {
        val char = charArg(arg).the_char
        val code = (if (case_i) char.lowercase()[0] else char).code
        
        if (code in things) {
            return Nil
        } else {
            things.add(code)
        }
    }
    return T
}
                        
// core of the comparison functions that check for equality
fun char_compare_equal(args: LObject, case_i: Boolean): LObject
{
    if (args === Nil) {
        return T
    }
    val (ch1, rest) = args
    val char1 = charArg(ch1, " first").the_char
    val code1 = (if (case_i) char1.lowercase()[0] else char1).code

    for (arg in rest) {
        val char = charArg(arg).the_char
        val code = (if (case_i) char.lowercase()[0] else char).code
        if (code != code1) {
            return Nil
        }
    }
    return T
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
fun bi_char_equal(args: LObject, kwArgs: Map<LSymbol, LObject>,
                  suppp: Map<LSymbol, Boolean>): LObject {
    return char_compare_equal(args, case_i = true)
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
fun bi_char_not_equal(args: LObject, kwArgs: Map<LSymbol, LObject>,
                      suppp: Map<LSymbol, Boolean>): LObject {
    return char_compare_unique(args, case_i = true)
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
fun bi_char_not_equal2(args: LObject, kwArgs: Map<LSymbol, LObject>,
                       suppp: Map<LSymbol, Boolean>): LObject {
    return char_compare_unique(args, case_i = false)
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
fun bi_char_lt(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
    return char_compare_ordered(args, case_i = false) {
        code1, code2 -> code1 < code2
    }
}

/// builtin char-lessp
/// fun     bi_char_lessp
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
fun bi_char_lessp(args: LObject, kwArgs: Map<LSymbol, LObject>,
                  suppp: Map<LSymbol, Boolean>): LObject {
    return char_compare_ordered(args, case_i = true) {
        code1, code2 -> code1 < code2
    }
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
fun bi_char_le(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
    return char_compare_ordered(args, case_i = false) {
        code1, code2 -> code1 <= code2
    }
}

/// builtin char-not-greaterp
/// fun     bi_char_not_greaterp
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
fun bi_char_not_greaterp(args: LObject, kwArgs: Map<LSymbol, LObject>,
                         suppp: Map<LSymbol, Boolean>): LObject {
    return char_compare_ordered(args, case_i = true) {
        code1, code2 -> code1 <= code2
    }
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
fun bi_char_eql(args: LObject, kwArgs: Map<LSymbol, LObject>,
                suppp: Map<LSymbol, Boolean>): LObject {
    return char_compare_equal(args, case_i = false)
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
fun bi_char_gt(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
    return char_compare_ordered(args, case_i = false) {
        code1, code2 -> code1 > code2
    }
}

/// builtin char-greaterp
/// fun     bi_char_greaterp
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
fun bi_char_greaterp(args: LObject, kwArgs: Map<LSymbol, LObject>,
                     suppp: Map<LSymbol, Boolean>): LObject {
    return char_compare_ordered(args, case_i = true) {
        code1, code2 -> code1 > code2
    }
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
fun bi_char_ge(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
    return char_compare_ordered(args, case_i = false) {
        code1, code2 -> code1 >= code2
    }
}

/// builtin char-not-lessp
/// fun     bi_char_not_lessp
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
fun bi_char_not_lessp(args: LObject, kwArgs: Map<LSymbol, LObject>,
                      suppp: Map<LSymbol, Boolean>): LObject {
    return char_compare_ordered(args, case_i = true) {
        code1, code2 -> code1 >= code2
    }
}

