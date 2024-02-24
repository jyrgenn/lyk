// calculating numbers

package org.w21.lyk


@Suppress("UNUSED_PARAMETER")
fun bi_plus(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    var acc: Double = 0.0

    for (arg in args) {
        acc += numberArg(arg, "+")
    }
    return makeNumber(acc)
}

@Suppress("UNUSED_PARAMETER")
fun bi_minus(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val (arg, rest) = args
    var acc = numberArg(arg, "-")

    if (rest !== Nil) {
        for (arg in rest) {
            acc -= numberArg(arg, "-")
        }
        return makeNumber(acc)
    }
    return makeNumber(- acc)
}

@Suppress("UNUSED_PARAMETER")
fun bi_mult(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    var acc = 1.0

    for (arg in args) {
        acc *= numberArg(arg, "*")
    }
    return makeNumber(acc)
}

@Suppress("UNUSED_PARAMETER")
fun bi_div(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val (arg, rest) = args
    var acc = numberArg(arg, "/")

    if (rest !== Nil) {
        for (arg in rest) {
            acc /= numberArg(arg, "/")
        }
        return makeNumber(acc)
    }
    return makeNumber(1 / acc)
}

