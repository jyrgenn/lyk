// calculating numbers

package org.w21.lyk

fun bi_plus(arglist: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    var acc: Double = 0.0

    if (kwArgs.count() > 0) {
        throw ArgumentError("won't take keyword arguments")
    }
    for (arg in arglist) {
        acc += numberArg(arg, "add")
    }
    return makeNumber(acc)
}

