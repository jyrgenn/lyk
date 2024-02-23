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
    throw ArgumentError("$what argument not a number: $arg (${typeOf(arg)}")
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


fun spreadArglist(args: LispObject): LispObject {
    // spreadable argument list designator n. a designator for a list of
    // objects; that is, an object that denotes a list and that is a non-null
    // list L1 of length n, whose last element is a list L2 of length m
    // (denoting a list L3 of length m+n-1 whose elements are L1i for i < n-1
    // followed by L2j for j < m). ``The list (1 2 (3 4 5)) is a spreadable
    // argument list designator for the list (1 2 3 4 5).''

    let lc = ListCollector()
    var list = args
    while let p = list as? Pair {
        if p.cdr() is Pair {
            lc.append(p.car())
        } else {
            break
        }
        list = p.cdr()
    }
    lc.lastcdr(list)
    return lc.list
}

fun environmentArg(arg: LispObject, what: String): Environment {
    if let environment = arg as? Environment {
        return environment
    }
    throw ArgumentError("\(what) argument not a environment: \(arg)")
}

fun streamArg(arg: LispObject, what: String): Stream {
    if let stream = arg as? Stream {
        return stream
    }
    throw ArgumentError("\(what) argument not a stream: \(arg)")
}

fun bool2ob(value: Boolean): LispObject {
    return value ? T : Nil
}

fun ob2bool(value: LispObject): Boolean {
    return value !== Nil
}

fun evalProgn(forms: LispObject): LispObject {
    var value: LispObject = Nil
    for form in forms {
        value = try eval(form)
    }
    return value
}

fun lastPair(list: Pair): Pair {
    var last = list
    while let next = last.cdr() as? Pair {
        last = next
    }
    return last
}

fun push(list: inout LispObject, elem: LispObject) {
    list = Pair(elem, list)
}

// Return the first element off list and store back the cdr of list.
// This is not checked -- if the list is not a pair, return Nil for efficiency
// reasons. The caller must know or not care.
fun pop(list: inout LispObject): LispObject {
    if list !== Nil {
        if let p = list as? Pair {
            let value = p.car()
            list = p.cdr()
            return value
        }
    }
    return Nil
}

// Return the first element of list. This is not checked -- if the list is
// not a pair, return Nil for efficiency reasons. The caller must know or not
// care.
fun arg1(list: LispObject): LispObject {
    if list !== Nil {
        if let p = list as? Pair {
            return p.car()
        }
    }
    return Nil
}

// Return the second element of list. This is not checked -- if the list is not
// a pair or not long enough, return Nil for efficiency reasons. The caller must
// know or not care.
fun arg2(list: LispObject): LispObject {
    if list !== Nil {
        if let p1 = list as? Pair {
            if let p2 = p1.cdr() as? Pair {
                return p2.car()
            }
        }
    }
    return Nil
}

// Return the third element of list. This is not checked -- if the list is not a
// pair or not long enough, return Nil for efficiency reasons. The caller must
// know or not care.
fun arg3(list: LispObject): LispObject {
    if list !== Nil {
        if let p1 = list as? Pair {
            if let p2 = p1.cdr() as? Pair {
                if let p3 = p2.cdr() as? Pair {
                    return p3.car()
                }
            }
        }
    }
    return Nil
}

// Return the first two elements of list. This is not checked -- if the list is
// not a pair or not long enough, return Nil for efficiency reasons. The caller
// must know or not care.
fun args2(list: LispObject): (LispObject, LispObject) {
    if list !== Nil {
        if let p1 = list as? Pair {
            if let p2 = p1.cdr() as? Pair {
                return (p1.car(), p2.car())
            }
        }
    }
    return (Nil, Nil)
}

// Return the first three elements of list. This is not checked -- if the list
// is not a pair or not long enough, return Nil for efficiency reasons. The
// caller must know or not care.
fun args3(list: LispObject): (LispObject, LispObject, LispObject) {
    if list !== Nil {
        if let p1 = list as? Pair {
            if let p2 = p1.cdr() as? Pair {
                if let p3 = p2.cdr() as? Pair {
                    return (p1.car(), p2.car(), p3.car())
                }
            }
        }
    }
    return (Nil, Nil, Nil)
}


fun iterateList(list: LispObject, what: String = "",
                 closure: (p: Pair, elem: LispObject): Void) {
    var curP = list
    while let p = curP as? Pair {
        try closure(p, p.car())
        curP = p.cdr()
    }
    if curP !== Nil {
        let this = what == "" ? "" : what + " is "
        throw ArgumentError("\(this)not a proper list: \(list)")
    }
}

fun list2varargs(args: LispObject): [any CVarArg] {
    var args = args
    var result: [any CVarArg] = []

    while args is Pair {
        let arg = pop(&args)
        switch arg {
        case let num as Number:
            result.append(num.isInt ? Int(num.value) : num.value)
        default:
            result.append(arg.valueString)
        }
    }
    return result
}

fun key2var(sym: Symbol): Symbol {
    return intern(String(sym.name.dropFirst(1)))
}

fun isKeysym(sym: Symbol): Symbol? {
    if sym.name.hasPrefix(":") {
        return key2var(sym)
    }
    return nil
}


fun var2key(sym: Symbol): Symbol {
    return intern(":" + sym.name)
}

fun itemList(args: LispObject...): LispObject {
    return try collectedList() { lc in
        for arg in args {
            lc.append(arg)
        }
    }
}
