// calculating numbers

package org.w21.lyk

import kotlin.math.*

/// builtin +
/// fun     bi_plus
/// std     
/// key     
/// opt     
/// rest    numbers
/// ret     sum
/// special no
/// doc {
/// Return the sum of the NUMBERS.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_plus(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    var acc: Double = 0.0

    for (arg in args) {
        acc += numberArg(arg, "+")
    }
    return makeNumber(acc)
}


/// builtin -
/// fun     bi_minus
/// std     num1
/// key     
/// opt     
/// rest    numbers
/// ret     number
/// special no
/// doc {
/// Return NUM1 minus all NUMBERS, or the negation of sole arg NUM1.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_minus(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (arg, rest) = args
    var acc = numberArg(arg, "-")

    if (rest !== Nil) {
        for (elem in rest) {
            acc -= numberArg(elem, "-")
        }
        return makeNumber(acc)
    }
    return makeNumber(- acc)
}


/// builtin *
/// fun     bi_mult
/// std     
/// key     
/// opt     
/// rest    numbers
/// ret     product
/// special no
/// doc {
/// Return the product of all NUMBERS.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_mult(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    var acc = 1.0

    for (arg in args) {
        acc *= numberArg(arg, "*")
    }
    return makeNumber(acc)
}

/// builtin /
/// fun     bi_div
/// std     num1
/// key     
/// opt     
/// rest    numbers
/// ret     number
/// special no
/// doc {
/// Return NUM1 divided by all NUMBERS, or the inverse of sole arg NUM1.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_div(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (arg, rest) = args
    var acc = numberArg(arg, "/")

    if (rest !== Nil) {
        for (elem in rest) {
            acc /= numberArg(elem, "/")
        }
        return makeNumber(acc)
    }
    return makeNumber(1 / acc)
}

/// builtin zerop
/// fun     bi_zerop
/// std     number
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if number is zero, nil otherwise
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_zerop(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return bool2ob(numberArg(arg1(args), "zerop") == 0.0)
}

val numberZero = makeNumber(0)

/// builtin abs
/// fun     bi_abs
/// std     number
/// key     
/// opt     
/// rest    
/// ret     absolute-value
/// special no
/// doc {
/// Return the absolute value of `number`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_abs(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(abs(numberArg(arg1(args), "abs")))
}

/// builtin acos
/// fun     bi_acos
/// std     number
/// key     
/// opt     
/// rest    
/// ret     radians
/// special no
/// doc {
/// Return the arc cosine of `number`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_acos(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(acos(numberArg(arg1(args), "acos")))
}

/// builtin acosh
/// fun     bi_acosh
/// std     number
/// key     
/// opt     
/// rest    
/// ret     radians
/// special no
/// doc {
/// Return the arc hyperbolic cosine of `number`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_acosh(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(acosh(numberArg(arg1(args), "acosh")))
}

// return true iff self is less than other
/// builtin <
/// fun     bi_cmp_lt
/// std     
/// key     
/// opt     
/// rest    items
/// ret     t/nil
/// special no
/// doc {
/// Return t if all `items` are in strictly ascending order, otherwise nil.
/// Comparable are symbols, numbers, and strings.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cmp_lt(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (arg1, rest) = args
    var first = arg1

    for (arg in rest) {
        if (first.compareTo(arg) < 0) {
            first = arg
        } else {
            return Nil
        }
    }
    return T
}

// return true iff self is greater than other
/// builtin >
/// fun     bi_cmp_gt
/// std     
/// key     
/// opt     
/// rest    items
/// ret     t/nil
/// special no
/// doc {
/// Return t if all `items` are in strictly descending order, otherwise nil.
/// Comparable are symbols, numbers, and strings.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cmp_gt(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (arg1, rest) = args
    var first = arg1

    for (arg in rest) {
        if (first.compareTo(arg) > 0) {
            first = arg
        } else {
            return Nil
        }
    }
    return T
}

// return true iff self is equal to other
/// builtin =
/// fun     bi_cmp_eqv
/// std     
/// key     
/// opt     
/// rest    items
/// ret     t/nil
/// special no
/// doc {
/// 
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cmp_eqv(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (arg1, rest) = args
    var first = arg1

    for (arg in rest) {
        if (first.compareTo(arg) == 0) {
            first = arg
        } else {
            return Nil
        }
    }
    return T
}

// return true iff self is not equal to other
/// builtin /=
/// fun     bi_cmp_ne
/// std     
/// key     
/// opt     
/// rest    items
/// ret     t/nil
/// special no
/// doc {
/// Return t if all `items` are different, otherwise nil.
/// Comparable are symbols, numbers, and strings.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cmp_ne(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    var things = mutableSetOf<LObject>()
    
    for (arg in args) {
        if (things.contains(arg)) {
            return Nil
        } else {
            things.add(arg)
        }
    }
    return T
}

// return true iff self is greater than or equal to other
/// builtin >=
/// fun     bi_cmp_ge
/// std     
/// key     
/// opt     
/// rest    items
/// ret     t/nil
/// special no
/// doc {
/// Return t if all `items` are in descending order, otherwise nil.
/// Comparable are symbols, numbers, and strings.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cmp_ge(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (arg1, rest) = args
    var first = arg1

    for (arg in rest) {
        if (first.compareTo(arg) >= 0) {
            first = arg
        } else {
            return Nil
        }
    }
    return T
}

// return true iff self is less than or equal to other
/// builtin <=
/// fun     bi_cmp_le
/// std     
/// key     
/// opt     
/// rest    items
/// ret     t/nil
/// special no
/// doc {
/// Return t if all `items` are in ascending order, otherwise nil.
/// Comparable are symbols, numbers, and strings.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cmp_le(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (arg1, rest) = args
    var first = arg1

    for (arg in rest) {
        if (first.compareTo(arg) <= 0) {
            first = arg
        } else {
            return Nil
        }
    }
    return T
}

// return -1 if self is less than other, 0 if equal to, 1 if greater than
/// builtin <=>
/// fun     bi_cmp
/// std     item1 item2
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return -1, 0, or 1 if `item1` is less than, equal to, or greater than `item2`.
/// Comparable are symbols, numbers, and strings.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cmp(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (v1, v2) = args2(args)
    return makeNumber(v1.compareTo(v2))
}

// Return modulo of NUM1 divided by NUM2 (which must be integers).
/// builtin %
/// fun     bi_modulo
/// std     num1 num2
/// key     
/// opt     
/// rest    
/// ret     modulo
/// special no
/// doc {
/// Return modulo of `num1` divided by `num2` (which must be integers).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_modulo(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (value1, value2) = args2(args)
    val long1: Long = longArg(value1, "%")
    val long2: Long = longArg(value2, "%")
    val result = makeNumber(long1 % long2)
    return result
}

/// builtin **
/// fun     bi_power
/// std     base power
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return `base` raised to the power `power`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_power(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (value1, value2) = args2(args)
    return makeNumber(
        numberArg(value1, "**").pow(numberArg(value2, "**")))
}

/// builtin 1+
/// fun     bi_1plus
/// std     number
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return a value of `number` plus 1.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_1plus(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val value = numberArg(arg1(args), "1+")
    return makeNumber(value + 1)
}

/// builtin 1-
/// fun     bi_1minus
/// std     number
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return a value of `number` plus 1.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_1minus(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val value = numberArg(arg1(args), "1-")
    return makeNumber(value - 1)
}

/// builtin isqrt
/// fun     bi_isqrt
/// std     number
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return the integer square root of `number`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_isqrt(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val n = numberArg(arg1(args), "isqrt")
    return makeNumber(isqrt(n.toLong()))
}

/// builtin ash
/// fun     bi_ash
/// std     integer count
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return `integer` arithmetically shifted by `count` bit positions.
/// Ash shifts left if `count` is positive, right if `count` is negative.
/// The shifted value has the same sign as `integer`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_ash(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (int, n) = args2(args)
    val value = longArg(int, "ash integer")
    val count = longArg(n, "ash count")

    if (count < 0) {
        return makeNumber(value shr -count.toInt())
    }
    return makeNumber(value shl count.toInt())
}

/// builtin asin
/// fun     bi_asin
/// std     number
/// key     
/// opt     
/// rest    
/// ret     radians
/// special no
/// doc {
/// Return the arc sine of `number`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_asin(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val value = numberArg(arg1(args), "asin")
    return makeNumber(asin(value))
}

/// builtin asinh
/// fun     bi_asinh
/// std     number
/// key     
/// opt     
/// rest    
/// ret     arc-hsine
/// special no
/// doc {
/// Return the arc hyperbolic sine of `number`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_asinh(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val value = numberArg(arg1(args), "asinh")
    return makeNumber(asinh(value))
}

/// builtin atan
/// fun     bi_atan
/// std     number
/// key     
/// opt     
/// rest    
/// ret     radians
/// special no
/// doc {
/// Return the arc tangent of `number`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_atan(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val value = numberArg(arg1(args), "atan")
    return makeNumber(atan(value))
}

/// builtin atanh
/// fun     bi_atanh
/// std     number
/// key     
/// opt     
/// rest    
/// ret     arc-htangent
/// special no
/// doc {
/// Return the arc hyperbolic tangent of `number`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_atanh(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val value = numberArg(arg1(args), "atanh")
    return makeNumber(atanh(value))
}

// https://de.wikipedia.org/wiki/Gau%C3%9Fsche_Osterformel#Eine_erg.C3.A4nzte_Osterformel
/// builtin easter
/// fun     bi_easter
/// std     year
/// key     
/// opt     
/// rest    
/// ret     year-month-day
/// special no
/// doc {
/// Calculate the easter date for `year`; return as a list (year month mday).
/// The formula to calculate the date is according to Lichtenberg as cited by
/// Wikipedia in
/// https://de.wikipedia.org/wiki/Gau%C3%9Fsche_Osterformel#Eine_erg.C3.A4nzte_Osterformel
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_easter(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val X = longArg(arg1(args), "easter year")
    val K = X / 100
    val M = 15 + (3*K+3)/4 - (8*K+13)/25
    val S = 2 - (3*K+3)/4
    val A = X % 19
    val D = (19*A + M) % 30
    val R = (D + A/11) / 29
    val OG = 21 + D - R
    val SZ = 7 - (X+X/4+S)%7
    val OE = 7 - (OG-SZ)%7
    val OS = OG + OE
    var month = 3
    var mday = OS
    if (OS > 31) {
	month = 4
	mday -= 31
    }
    return LCons(makeNumber(X),
                LCons(makeNumber(month),
                     LCons(makeNumber(mday), Nil)))
}
    
/// builtin round
/// fun     bi_round
/// std     number
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return `number` rounded to the nearest integer.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_round(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(round(numberArg(arg1(args), "round")))
}

/// builtin ceiling
/// fun     bi_ceiling
/// std     number
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return `number` truncated to an integer towards positive infinity.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_ceiling(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return makeNumber(ceil(numberArg(arg1(args), "round")))
}

/// builtin floor
/// fun     bi_floor
/// std     number
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return `number` truncated to an integer towards negative infinity.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_floor(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(floor(numberArg(arg1(args), "round")))
}

/// builtin sqrt
/// fun     bi_sqrt
/// std     number
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return the sqare root of `number`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_sqrt(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(sqrt(numberArg(arg1(args), "sqrt")))
}

/// builtin integerp
/// fun     bi_integerp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return true iff object is an integer number
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_integerp(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val arg = arg1(args)
    return bool2ob((arg is LNumber) && arg.isInt())
}

/// builtin signum
/// fun     bi_signum
/// std     number
/// key     
/// opt     
/// rest    
/// ret     sign
/// special no
/// doc {
/// Return -1 / 0 / 1 if NUMBER is negative / zero / positive, respectively.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_signum(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val number = numberArg(arg1(args), "signum")
    return makeNumber(if (number < 0) {
                          -1
                      } else if (number > 0) {
                          1
                      } else {
                          0
                      })
}


