// helpers for functions, mostly Builtins

package org.w21.lyk


fun envArg(arg: LispObject, what: String): Environment {
    if (arg is Environment) {
        return arg
    }
    throw ArgumentError("$what argument is not an environment: $arg")
}

fun numberArg(arg: LispObject, what: String): Double {
    if (arg is Number) {
        return arg.value
    }
    throw ArgumentError("$what argument not a number: $arg")
}

// fun intArg(arg: LispObject, what: String): Int {
//     if let num = arg as? Number {
//         let intval = Int(num.value)
//         if num.value == Double(intval) {
//             return intval
//         }
//     }
//     throw ArgumentError("$what argument not an integer: $arg")
// }

fun intArg(arg: LispObject, what: String): Int {
    if (arg is Number && arg.isInt()) {
        return arg.value.toInt()
    }
    throw ArgumentError("$what argument not an integer: $arg")
}

fun functionArg(arg: LispObject, what: String): Function {
    if (arg is Function) {
        return arg
    }
    throw ArgumentError("$what argument not a function: $arg")
}

// fun comparableArg(arg: LispObject, what: String): CompObject {
//     if (arg is CompObject) {
//         return arg
//     }
//     throw ArgumentError("$what argument $arg type ${typeOf(arg)}"
//                           + " not comparable")
// }

fun pairArg(arg: LispObject, what: String): Cons {
    if (arg is Cons) {
        return arg
    }
    throw ArgumentError("$what argument not a pair: $arg")
}

fun listArg(arg: LispObject, what: String): LispList {
    if (arg is Cons) {
        return arg
    }
    if (arg is Symbol && arg === Nil) {
        return arg
    }
    throw ArgumentError("$what argument not a list: $arg")
}

// fun sequenceArg(arg: LispObject, what: String): ObjectSequence {
//     if (arg is any) ObjectSequence {
//         return arg
//     }
//     throw ArgumentError("$what argument not a sequence: $arg")
// }

fun stringArg(arg: LispObject, what: String): String {
    if (arg is LispString) {
        return arg.value
    }
    throw ArgumentError("$what argument not a string: $arg")
}

fun stringlikeArg(arg: LispObject, what: String): String {
    if (arg is LispString) {
        return arg.value
    }
    if (arg is Symbol) {
        return arg.name
    }
    throw ArgumentError("$what argument not a string or symbol: $arg")
}

fun symbolArg(arg: LispObject, what: String): Symbol {
    if (arg is Symbol) {
        return arg
    }
    throw ArgumentError("$what argument not a symbol: $arg")
}

fun tableArg(arg: LispObject, what: String): Table {
    if (arg is Table) {
        return arg
    }
    throw ArgumentError("$what argument not a table: $arg")
}

fun regexpArg(arg: LispObject, what: String): Regexp {
    if (arg is Regexp) {
        return arg
    }
    throw ArgumentError("$what argument not a regexp: $arg")
}

fun vectorArg(arg: LispObject, what: String): Vector {
    if (arg is Vector) {
        return arg
    }
    throw ArgumentError("$what argument not a vector: $arg")
}
