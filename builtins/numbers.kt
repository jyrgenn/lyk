// calculating numbers

package org.w21.lyk


@Suppress("UNUSED_PARAMETER")
fun bi_plus(arglist: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    var acc: Double = 0.0

    for (arg in arglist) {
        acc += numberArg(arg, "+")
    }
    return makeNumber(acc)
}

@Suppress("UNUSED_PARAMETER")
fun bi_minus(arglist: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    var acc: Double = numberArg((arglist as Cons).car(), "-")
    val rest = arglist.cdr()

    if (rest !== Nil) {
        for (arg in rest) {
            acc -= numberArg(arg, "-")
        }
        return makeNumber(acc)
    }
    return makeNumber(- acc)
}

@Suppress("UNUSED_PARAMETER")
fun bi_mult(arglist: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    var acc: Double = 1.0

    for (arg in arglist) {
        acc *= numberArg(arg, "*")
    }
    return makeNumber(acc)
}

@Suppress("UNUSED_PARAMETER")
fun bi_div(arglist: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    var acc: Double = numberArg((arglist as Cons).car(), "/")
    val rest = arglist.cdr()

    if (rest !== Nil) {
        for (arg in rest) {
            acc /= numberArg(arg, "/")
        }
        return makeNumber(acc)
    }
    return makeNumber(1 / acc)
}

