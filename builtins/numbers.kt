// calculating numbers

package org.w21.lyk

import kotlin.math.*
import kotlin.random.Random

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
/// Ties (<integer> + 0.5) are rounded towards an even integer.
/// }
/// end builtin
/// builtin fround
/// fun     bi_round
/// std     number
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return `number` rounded to the nearest integer.
/// Ties (<integer> + 0.5) are rounded towards an even integer.
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
/// builtin ffloor
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

/// builtin evenp
/// fun     bi_evenp
/// std     n
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return a true value iff the number is an even integer.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_evenp(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val arg = arg1(args)

    if (arg is LNumber) {
        if (arg.isLong()) {
            return bool2ob(arg.the_number.toLong() % 2 == 0L)
        }
        return Nil
    }
    throw ArgumentError("evenp argument is not a number: $arg (${typeOf(arg)})")
}

/// builtin oddp
/// fun     bi_oddp
/// std     n
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return a true value iff the number is an odd integer.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_oddp(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val arg = arg1(args)
    
    if (arg is LNumber) {
        if (arg.isLong()) {
            return bool2ob(arg.the_number.toLong() % 2 != 0L)
        }
        return Nil
    }
    throw ArgumentError("oddp argument is not a number: $arg (${typeOf(arg)})")
}

/// builtin conjugate
/// fun     bi_conjugate
/// std     
/// key     
/// opt     
/// rest    &rest nothing
/// ret     nothing
/// special no
/// doc {
/// Function is not implemented; will throw an error.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_conjugate(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    throw NotImplementedError("conjugate")
}

/// builtin cis
/// fun     bi_cis
/// std     
/// key     
/// opt     
/// rest    &rest nothing
/// ret     nothing
/// special no
/// doc {
/// Function is not implemented; will throw an error.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cis(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    throw NotImplementedError("cis")
}

/// builtin rational
/// fun     bi_rational
/// std     
/// key     
/// opt     
/// rest    &rest nothing
/// ret     nothing
/// special no
/// doc {
/// Function is not implemented; will throw an error.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_rational(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    throw NotImplementedError("rational")
}

/// builtin rationalize
/// fun     bi_rationalize
/// std     
/// key     
/// opt     
/// rest    &rest nothing
/// ret     nothing
/// special no
/// doc {
/// Function is not implemented; will throw an error.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_rationalize(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    throw NotImplementedError("rationalize")
}

/// builtin minusp
/// fun     bi_minusp
/// std     number
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return true iff `number` is less than zero.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_minusp(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return bool2ob(numberArg(arg1(args), "minusp") < 0)
}

/// builtin plusp
/// fun     bi_plusp
/// std     number
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return true iff `number` is greater than zero.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_plusp(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return bool2ob(numberArg(arg1(args), "plusp") > 0)
}

/// builtin integer-length
/// fun     bi_integer_length
/// std     integer
/// key     
/// opt     
/// rest    
/// ret     length
/// special no
/// doc {
/// Returns the number of bits needed to represent `integer`.
/// If the argument is not an integer, it will be rounded to the nearest one.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_integer_length(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val num = numberArg(arg1(args), "integer-length")
    var integer = round(num).toLong()
    if (integer < 0) {
        integer = -integer - 1
    }
    var mask = 0L
    var count = 0
    while (integer != mask and integer) {
        count++
        mask = (mask shl 1) or 1
    }
    return makeNumber(count)
}

/// builtin expt
/// fun     bi_expt
/// std     base power
/// key     
/// opt     
/// rest    
/// ret     result
/// special no
/// doc {
/// Return `base` raise to the power `power`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_expt(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (base, power) = args2(args)
    return makeNumber(numberArg(base, "expt base")
                          .pow(numberArg(power, "expt power")))
}

/// builtin numerator
/// fun     bi_numerator
/// std     number
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return the numerator of `number`.
/// As only real numbers are supported, the numerator of a number is
/// always the number itself.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_numerator(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val num = arg1(args)
    numberArg(num, "numerator")
    return num
}

/// builtin denominator
/// fun     bi_denominator
/// std     number
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return the denominator of `number`.
/// As only real numbers are supported, the denominator of a number is
/// always 1.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_denominator(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val num = arg1(args)
    numberArg(num, "denominator")
    return makeNumber(1)
}

/// builtin realpart
/// fun     bi_realpart
/// std     number
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return the realpart of `number`.
/// As only real numbers are supported, the real part of a number is
/// always the number itself.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_realpart(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val num = arg1(args)
    numberArg(num, "realpart")
    return num
}

/// builtin imagpart
/// fun     bi_imagpart
/// std     number
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return the imagpart of `number`.
/// As only real numbers are supported, the imaginary part of a number is
/// always zero.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_imagpart(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val num = arg1(args)
    numberArg(num, "imagpart")
    return makeNumber(0)
}

/// builtin phase
/// fun     bi_phase
/// std     number
/// key     
/// opt     
/// rest    
/// ret     phase
/// special no
/// doc {
/// Return the angle part of `number`'s polar representation.
/// As only real numbers are supported, the return value is only -Pi for
/// negative numbers, Pi for positive numbers, and zero for zero.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_phase(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val num = numberArg(arg1(args), "phase")

    return makeNumber(if (num < 0) {
                          -PI
                      } else if (num > 0) {
                          PI
                      } else {
                          0.0
                      })
}

/// builtin pi
/// fun     bi_pi
/// std     
/// key     
/// opt     
/// rest    
/// ret     pi
/// special no
/// doc {
/// Return the value of the constant Pi.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_pi(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(PI)
}

/// builtin e
/// fun     bi_e
/// std     
/// key     
/// opt     
/// rest    
/// ret     e
/// special no
/// doc {
/// Return the value of the constant E.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_e(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(E)
}

/// builtin fceiling
/// fun     bi_fceiling
/// std     number
/// key     
/// opt     
/// rest    
/// ret     result
/// special no
/// doc {
/// Return `number` truncated to an integer towards positive infinity.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_fceiling(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(ceil(numberArg(arg1(args), "fceiling")))
}

/// builtin log
/// fun     bi_log
/// std     number
/// key     
/// opt     base makeNumber(E)
/// rest    
/// ret     result
/// special no
/// doc {
/// Return the logarithm of `number` in base `base` (defaults to e).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_log(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (number, base) = args2(args)
    return makeNumber(log(numberArg(number, "log number"),
                          numberArg(base, "log base")))
}

/// builtin exp
/// fun     bi_exp
/// std     power
/// key     
/// opt     
/// rest    
/// ret     result
/// special no
/// doc {
/// Return e raised to the power of `power`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_exp(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(exp(numberArg(arg1(args), "exp")))
}

/// builtin tanh
/// fun     bi_tanh
/// std     radians
/// key     
/// opt     
/// rest    
/// ret     result
/// special no
/// doc {
/// Return the hyperbolic tangent of `radians`
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_tanh(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(tanh(numberArg(arg1(args), "tanh")))
}

/// builtin cosh
/// fun     bi_cosh
/// std     radians
/// key     
/// opt     
/// rest    
/// ret     result
/// special no
/// doc {
/// Return the hyperbolic cosine of `radians`
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cosh(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(cosh(numberArg(arg1(args), "cosh")))
}

/// builtin sinh
/// fun     bi_sinh
/// std     radians
/// key     
/// opt     
/// rest    
/// ret     result
/// special no
/// doc {
/// Return the hyperbolic sine of `radians`
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_sinh(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(sinh(numberArg(arg1(args), "sinh")))
}

/// builtin tan
/// fun     bi_tan
/// std     radians
/// key     
/// opt     
/// rest    
/// ret     result
/// special no
/// doc {
/// Return the tangent of `radians`
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_tan(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(tan(numberArg(arg1(args), "tan")))
}

/// builtin cos
/// fun     bi_cos
/// std     radians
/// key     
/// opt     
/// rest    
/// ret     result
/// special no
/// doc {
/// Return the cosine of `radians`
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cos(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(cos(numberArg(arg1(args), "cos")))
}

/// builtin sin
/// fun     bi_sin
/// std     radians
/// key     
/// opt     
/// rest    
/// ret     result
/// special no
/// doc {
/// Return the sine of `radians`
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_sin(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(sin(numberArg(arg1(args), "sin")))
}

/// builtin truncate
/// fun     bi_truncate
/// std     number
/// key     
/// opt     
/// rest    
/// ret     result
/// special no
/// doc {
/// Return `number` truncated towards zero.
/// }
/// end builtin
/// builtin ftruncate
/// fun     bi_truncate
/// std     number
/// key     
/// opt     
/// rest    
/// ret     result
/// special no
/// doc {
/// Return `number` truncated towards zero.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_truncate(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(truncate(numberArg(arg1(args), "truncate")))
}

/// builtin random
/// fun     bi_random
/// std     
/// key     
/// opt     limit, int
/// rest    
/// ret     random-number
/// special no
/// doc {
/// Return a non-negative pseudo-random number less than 1 (or `limit`).
/// If optional `int` is non-nil, the returned number is an integer.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_random(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (limit, int) = args2(args)
    val rnd = Random.Default

    if (limit === Nil) {
        if (ob2bool(int)) {
            return makeNumber(rnd.nextInt())
        } else {
            return makeNumber(rnd.nextDouble())
        }
    } else {
        val until = numberArg(limit, "random limit")
        if (ob2bool(int)) {
            return makeNumber(rnd.nextInt(until.toInt()))
        } else {
            return makeNumber(rnd.nextDouble(until))
        }
    }
}

/// builtin max
/// fun     bi_max
/// std     number
/// key     
/// opt     
/// rest    numbers
/// ret     maximum
/// special no
/// doc {
/// Return the largest number of all arguments
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_max(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (number, rest) = args
    var the_max = numberArg(number, "max first")

    for (next in rest) {
        val num = numberArg(next, "max")
        if (num > the_max) {
            the_max = num
        }
    }
    return makeNumber(the_max)
}

/// builtin min
/// fun     bi_min
/// std     number
/// key     
/// opt     
/// rest    numbers
/// ret     minimum
/// special no
/// doc {
/// Return the smallest number of all arguments
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_min(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (number, rest) = args
    var the_min = numberArg(number, "min first")

    for (next in rest) {
        val num = numberArg(next, "min")
        if (num < the_min) {
            the_min = num
        }
    }
    return makeNumber(the_min)
}

