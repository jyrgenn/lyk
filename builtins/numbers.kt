// calculating numbers

package org.w21.lyk


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

