// helpers for functions, mostly Builtins

package org.w21.lyk


fun outputStreamArg(arg: LObject, what: String): Stream {
    if (arg === Nil) {
        return stdout
    }
    return streamArg(arg, what)
}

fun streamArg(arg: LObject, what: String): Stream { 
    return (arg as? Stream) ?:
        throw ArgumentError("$what argument is not a stream: $arg")
}

fun envArg(arg: LObject, what: String): LEnv {
    return (arg as? LEnv) ?:
        throw ArgumentError("$what argument is not an environment: $arg")
}

fun numberArg(arg: LObject, what: String): Double {
    return (arg as? LNumber)?.value ?:
        throw ArgumentError("$what argument not a number: $arg (${typeOf(arg)})")
}

// fun longArg(arg: LObject, what: String): Int {
//     if let num = arg as? LNumber {
//         let intval = Int(num.value)
//         if num.value == Double(intval) {
//             return intval
//         }
//     }
//     throw ArgumentError("$what argument not an integer: $arg")
// }

fun longArg(arg: LObject, what: String): Long {
    if (arg is LNumber && arg.isLong()) {
        return arg.value.toLong()
    }
    throw ArgumentError("$what argument not an integer: $arg")
}

fun functionArg(arg: LObject, what: String): LFunction {
    return arg as? LFunction ?:
        throw ArgumentError("$what argument not a function: $arg")
}

// fun comparableArg(arg: LObject, what: String): CompObject {
//     if (arg is CompObject) {
//         return arg
//     }
//     throw ArgumentError("$what argument $arg type ${typeOf(arg)}"
//                           + " not comparable")
// }

fun consArg(arg: LObject, what: String): LCons {
    return arg as? LCons ?:
        throw ArgumentError("$what argument not a cons: $arg")
}

fun listArg(arg: LObject, what: String): LObject {
    if (arg.isList()) {
        return arg
    }
    throw ArgumentError("$what argument not a list: $arg")
}

// fun sequenceArg(arg: LObject, what: String): ObjectSequence {
//     if (arg is any) ObjectSequence {
//         return arg
//     }
//     throw ArgumentError("$what argument not a sequence: $arg")
// }

fun stringArg(arg: LObject, what: String): String {
    return (arg as? LString)?.value ?:
        throw ArgumentError("$what argument not a string: $arg")
}

fun stringlikeArg(arg: LObject, what: String): String {
    if (arg is LString) {
        return arg.value
    }
    if (arg is LSymbol) {
        return arg.name
    }
    throw ArgumentError("$what argument not a string or symbol: $arg")
}

fun symbolArg(arg: LObject, what: String): LSymbol {
    return arg as? LSymbol ?:
        throw ArgumentError("$what argument not a symbol: $arg")
}

fun tableArg(arg: LObject, what: String): LTable {
    if (arg is LTable) {
        return arg
    }
    throw ArgumentError("$what argument not a table: $arg")
}

fun regexpArg(arg: LObject, what: String): LRegexp {
    if (arg is LRegexp) {
        return arg
    }
    throw ArgumentError("$what argument not a regexp: $arg")
}

fun vectorArg(arg: LObject, what: String): LVector {
    if (arg is LVector) {
        return arg
    }
    throw ArgumentError("$what argument not a vector: $arg")
}


fun spreadArglist(args: LObject): LObject {
    // spreadable argument list designator n. a designator for a list of
    // objects; that is, an object that denotes a list and that is a non-null
    // list L1 of length n, whose last element is a list L2 of length m
    // (denoting a list L3 of length m+n-1 whose elements are L1i for i < n-1
    // followed by L2j for j < m). ``The list (1 2 (3 4 5)) is a spreadable
    // argument list designator for the list (1 2 3 4 5).'' [CLHS Glossary]

    if (args === Nil) {
        return Nil
    }
    var arglist = args
    var last: LObject = Nil
    var lastBut2nd: LObject = Nil
    var next = arglist

    while (next is LCons) {
        lastBut2nd = last
        last = next
        next = next.cdr()
    }
    val tail = last.car()
    if (lastBut2nd === Nil) {
        arglist = tail
    } else {
        (lastBut2nd as LCons).rplacd(tail)
    }
    return arglist
}

fun environmentArg(arg: LObject, what: String): LEnv {
    return arg as? LEnv ?:
        throw ArgumentError("$what argument not a environment: $arg")
}

fun bool2ob(value: Boolean): LObject {
    return if (value) T else Nil
}

fun ob2bool(value: LObject): Boolean {
    return value !== Nil
}

fun lastCons(list: LCons): LCons {
    var last2b = list
    while (true) {
        val next = last2b.cdr()
        if (next is LCons) {
            last2b = next
        } else {
            return last2b
        }
    }
}

fun lastCons2(list: LCons): LCons {
    var last = list

    while (last.cdr() is LCons) {
        last = last.cdr() as LCons
    }
    return last
}

// Return the first element of list. This is not checked -- if the list is
// not a cons, return Nil for efficiency reasons. The caller must know or not
// care.
fun arg1(list: LObject): LObject {
    return (list as? LCons)?.car() ?: Nil
}

// Return the second element of list. This is not checked -- if the list is not
// a cons or not long enough, return Nil for efficiency reasons. The caller must
// know or not care.
fun arg2(list: LObject): LObject {
    return ((list as? LCons)?.cdr() as? LCons)?.car() ?: Nil
}

// Return the third element of list. This is not checked -- if the list is not a
// cons or not long enough, return Nil for efficiency reasons. The caller must
// know or not care.
fun arg3(list: LObject): LObject {
    return (((list as? LCons)?.cdr() as? LCons)?.cdr() as? LCons)?.car() ?: Nil
}

// Return the first two elements of list. This is not checked -- if the list is
// not a cons or not long enough, return Nil for efficiency reasons. The caller
// must know or not care.
fun args2(list: LObject): Pair<LObject, LObject> {
    val (a1, rest) = list
    return Pair(a1, arg1(rest))
}

// Return the first three elements of list. This is not checked -- if the list
// is not a cons or not long enough, return Nil for efficiency reasons. The
// caller must know or not care.
fun args3(list: LObject): Triple<LObject, LObject, LObject> {
    val (a1, rest1) = list
    val (a2, rest2) = rest1
    return Triple(a1, a2, arg1(rest2))
}


fun key2var(sym: LSymbol): LSymbol {
    return intern(sym.name.substring(1))
}

fun isKeysym(sym: LSymbol): LSymbol? {
    if (sym.name.startsWith(":")) {
        return key2var(sym)
    }
    return null
}

fun var2key(sym: LSymbol): LSymbol {
    return intern(":" + sym.name)
}

fun itemList(args: LObject): LObject {
    return collectedList() { lc ->
        for (arg in args) {
            lc.add(arg)
        }
    }
}

fun withVariableAs(variable: LSymbol, value: LObject, closure: () -> Unit) {
    val previousValue = variable.getValueOptional()
    
    variable.setValue(value)
    try {
        closure()
    } finally {
        if (previousValue == null) {
            currentEnv.unbind(variable)
        } else {
            variable.setValue(previousValue)
        }
    }
}
