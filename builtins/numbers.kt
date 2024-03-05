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
fun bi_plus(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    var acc: Double = 0.0

    for (arg in args) {
        acc += numberArg(arg, "+")
    }
    return Number.makeNumber(acc)
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
fun bi_minus(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val (arg, rest) = args
    var acc = numberArg(arg, "-")

    if (rest !== Nil) {
        for (elem in rest) {
            acc -= numberArg(elem, "-")
        }
        return Number.makeNumber(acc)
    }
    return Number.makeNumber(- acc)
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
fun bi_mult(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    var acc = 1.0

    for (arg in args) {
        acc *= numberArg(arg, "*")
    }
    return Number.makeNumber(acc)
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
fun bi_div(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val (arg, rest) = args
    var acc = numberArg(arg, "/")

    if (rest !== Nil) {
        for (elem in rest) {
            acc /= numberArg(elem, "/")
        }
        return Number.makeNumber(acc)
    }
    return Number.makeNumber(1 / acc)
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
fun bi_zerop(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return bool2ob(numberArg(arg1(args), "zerop") == 0.0)
}

val numberZero = Number.makeNumber(0)

// /// builtin 
// /// fun     
// /// std     
// /// key     
// /// opt     
// /// rest    
// /// ret     
// /// special 
// /// doc {
// /// 
// /// }
// /// end builtin


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
fun bi_abs(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return Number.makeNumber(abs(numberArg(arg1(args), "abs")))
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
fun bi_acos(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return Number.makeNumber(acos(numberArg(arg1(args), "acos")))
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
fun bi_acosh(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return Number.makeNumber(acosh(numberArg(arg1(args), "acosh")))
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
fun bi_cmp_lt(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
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
fun bi_cmp_gt(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
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
fun bi_cmp_eqv(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
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
fun bi_cmp_ne(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    var things = mutableSetOf<LispObject>()
    
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
fun bi_cmp_ge(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
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
fun bi_cmp_le(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
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
fun bi_cmp(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    // var args = args
    // val value1 = comparableArg(pop(&args), "<=>")
    // val value2 = comparableArg(pop(&args), "<=>")
    val (v1, v2) = args2(args)
    return Number.makeNumber(v1.compareTo(v2))
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
fun bi_modulo(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val (value1, value2) = args2(args)
    return Number.makeNumber(intArg(value1, "%") % intArg(value2, "%"))
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
fun bi_power(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val (value1, value2) = args2(args)
    return Number.makeNumber(
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
fun bi_1plus(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val value = numberArg(arg1(args), "1+")
    return Number.makeNumber(value + 1)
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
fun bi_1minus(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val value = numberArg(arg1(args), "1-")
    return Number.makeNumber(value - 1)
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
fun bi_isqrt(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val n = numberArg(arg1(args), "isqrt")
    var op = n.toInt()
    var res = 0
    var one = 1 shl 62

    while (one > op) {
        one = one shr 2
    }
    while (one != 0) {
        if (op >= res+one) {
            op -= res + one
            res += one shl 1 // <-- faster than 2 * one
        }
        res = res shr 1
        one = one shr 2
    }
    return Number.makeNumber(res)
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
fun bi_ash(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val (int, n) = args2(args)
    val value = intArg(int, "ash integer")
    val count = intArg(n, "ash count")

    if (count < 0) {
        return Number.makeNumber(value shr -count)
    }
    return Number.makeNumber(value shl count)
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
fun bi_asin(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val value = numberArg(arg1(args), "asin")
    return Number.makeNumber(asin(value))
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
fun bi_asinh(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val value = numberArg(arg1(args), "asinh")
    return Number.makeNumber(asinh(value))
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
fun bi_atan(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val value = numberArg(arg1(args), "atan")
    return Number.makeNumber(atan(value))
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
fun bi_atanh(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val value = numberArg(arg1(args), "atanh")
    return Number.makeNumber(atanh(value))
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
fun bi_easter(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val X = intArg(arg1(args), "easter year")
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
    return Cons(Number.makeNumber(X),
                Cons(Number.makeNumber(month),
                     Cons(Number.makeNumber(mday), Nil)))
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
fun bi_round(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return Number.makeNumber(round(numberArg(arg1(args), "round")))
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
fun bi_ceiling(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    return Number.makeNumber(ceil(numberArg(arg1(args), "round")))
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
fun bi_floor(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return Number.makeNumber(floor(numberArg(arg1(args), "round")))
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
fun bi_sqrt(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return Number.makeNumber(sqrt(numberArg(arg1(args), "sqrt")))
}

