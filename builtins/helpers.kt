// helpers for functions, mostly Builtins

package org.w21.lyk

import kotlin.io.path.*

// all these *Arg(arg: LObject, desc: String? = null) functions check the passed
// argument for the correct type(s) and return the corresponding typed value.
// 
// The `desc` argument if meant for an error message if the type check fails;
// the name of the function is taken from global calledFunctionName, and the
// `desc` argument, if passed, may serve to specify the argument further.
// Whereas with #'list, all argument have the same function and need not be
// distiguished further, it will be helpful to distiguish "elt index argument"
// and "elt sequence argument", for instance.
//
// If passed at all, it SHOULD begin with a blank, like in " index" so the error
// message can be templated as "$calledFunctionName$desc is not an [...]"

fun outputStreamArg(arg: LObject, desc: String = ""): LStream {
    if (arg === Nil) {
        return stdout
    }
    return streamArg(arg, desc)
}

fun streamArg(arg: LObject, desc: String = ""): LStream { 
    return (arg as? LStream) ?:
        throw ArgumentError("$calledFunctionName$desc argument is not a stream: $arg")
}

fun charArg(arg: LObject, desc: String = ""): LChar { 
    return (arg as? LChar) ?:
        throw ArgumentError("$calledFunctionName$desc argument is not a char: $arg")
}

fun charOrStringArg(arg: LObject, desc: String = ""): Char {
    if (arg is LChar) {
        return arg.the_char
    } else if (arg is LString) {
        if (arg.the_string.length == 1) {
            return arg.the_string[0]
        }
    } else {
        val s = arg.toString()
        if (s.length == 1) {
            return s[0]
        }
    }
    throw ArgumentError("$calledFunctionName$desc argument is not a char or string of"
                        +" length 1: $arg")
}

fun envArg(arg: LObject, desc: String = ""): LEnv {
    return (arg as? LEnv) ?:
        throw ArgumentError("$calledFunctionName$desc argument is not an environment: $arg")
}

fun numberArg(arg: LObject, desc: String = ""): Double {
    return (arg as? LNumber)?.the_number ?:
        throw ArgumentError("$calledFunctionName$desc argument is not a number: $arg (${arg.obtype})")
}

fun intArg(arg: LObject, desc: String = "") = numberArg(arg, desc).toInt()

fun indexArg(arg: LObject, desc: String = ""): Int {
    val index = intArg(arg, desc)
    if (index >= 0) {
        return index
    }
    throw ArgumentError("$calledFunctionName$desc argument is negative: $arg")
}

fun longArg(arg: LObject, desc: String = ""): Long {
    if (arg is LNumber && arg.isLong()) {
        return arg.the_number.toLong()
    }
    throw ArgumentError("$calledFunctionName$desc argument is not an integer: $arg")
}

fun functionArg(arg: LObject, desc: String = ""): LFunction {
    return arg as? LFunction ?:
        throw ArgumentError("$calledFunctionName$desc argument is not a function: $arg")
}

fun consArg(arg: LObject, desc: String = ""): LCons {
    return arg as? LCons ?:
        throw ArgumentError("$calledFunctionName$desc argument is not a cons: $arg")
}

fun listArg(arg: LObject, desc: String = ""): LObject {
    if (arg.isList()) {
        return arg
    }
    throw ArgumentError("$calledFunctionName$desc argument is not a list: $arg")
}

fun seqArg(arg: LObject, desc: String = ""): LSeq {
    if (arg is LSeq) {
        return arg
    }
    throw ArgumentError("$calledFunctionName$desc argument is not a sequence: $arg")
}

// fun sequenceArg(arg: LObject, desc: String = ""): ObjectSequence {
//     if (arg is any) ObjectSequence {
//         return arg
//     }
//     throw ArgumentError("$calledFunctionName$desc argument is not a sequence: $arg")
// }

fun stringArg(arg: LObject, desc: String = ""): String {
    return (arg as? LString)?.the_string ?:
        throw ArgumentError("$calledFunctionName$desc argument is not a string: $arg")
}

fun stringlikeArg(arg: LObject, desc: String = ""): String {
    if (arg is LString) {
        return arg.the_string
    }
    if (arg is LSymbol) {
        return arg.name
    }
    throw ArgumentError("$calledFunctionName$desc argument is not a string or symbol: $arg")
}

fun symbolArg(arg: LObject, desc: String = ""): LSymbol {
    return arg as? LSymbol ?:
        throw ArgumentError("$calledFunctionName$desc argument is not a symbol: $arg")
}

fun tableArg(arg: LObject, desc: String = ""): LTable {
    if (arg is LTable) {
        return arg
    }
    throw ArgumentError("$calledFunctionName$desc argument is not a table: $arg")
}

fun regexpArg(arg: LObject, desc: String = ""): LRegexp {
    if (arg is LRegexp) {
        return arg
    }
    try {
        return LRegexp(arg.toString())
    } catch (e: Exception) {
        throw ArgumentError("$calledFunctionName$desc argument is not a regexp: $arg ($e)")
    }
}

fun vectorArg(arg: LObject, desc: String = ""): LVector {
    if (arg is LVector) {
        return arg
    }
    throw ArgumentError("$calledFunctionName$desc argument is not a vector: $arg")
}

fun intOrDefault(arg: LObject, default: Int, desc: String = ""): Int {
    if (arg === Nil) {
        return default
    }
    return intArg(arg, desc)
}

fun stringlistArg(list: LObject, desc: String = ""): List<String> {
    val mlos = mutableListOf<String>()
    for (elem in list) {
        mlos.add(stringArg(elem, desc))
    }
    return mlos
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
        next = next.cdr
    }
    val tail = last.car
    if (lastBut2nd === Nil) {
        arglist = tail
    } else {
        (lastBut2nd as LCons).cdr = tail
    }
    return arglist
}

fun environmentArg(arg: LObject, desc: String = ""): LEnv {
    return arg as? LEnv ?:
        throw ArgumentError("$calledFunctionName$desc argument is not a environment: $arg")
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
        val next = last2b.cdr
        if (next is LCons) {
            last2b = next
        } else {
            return last2b
        }
    }
}

fun lastCons2(list: LCons): LCons {
    var last = list

    while (last.cdr is LCons) {
        last = last.cdr as LCons
    }
    return last
}

// Return the first element of list. This is not checked -- if the list is
// not a cons, return Nil for efficiency reasons. The caller must know or not
// care.
fun arg1(list: LObject): LObject {
    return (list as? LCons)?.car ?: Nil
}

// Return the second element of list. This is not checked -- if the list is not
// a cons or not long enough, return Nil for efficiency reasons. The caller must
// know or not care.
fun arg2(list: LObject): LObject {
    return ((list as? LCons)?.cdr as? LCons)?.car ?: Nil
}

// Return the third element of list. This is not checked -- if the list is not a
// cons or not long enough, return Nil for efficiency reasons. The caller must
// know or not care.
fun arg3(list: LObject): LObject {
    return (((list as? LCons)?.cdr as? LCons)?.cdr as? LCons)?.car ?: Nil
}

// Return the first two elements of list. This is not checked -- if the list is
// not a cons or not long enough, return Nil for efficiency reasons. The caller
// must know or not care.
fun args2(list: LObject): Pair<LObject, LObject> {
    var rest = list
    val a1 = rest.car
    rest = rest.cdr
    val a2 = rest.car
    return Pair(a1, a2)
}

// Return the first three elements of list. This is not checked -- if the list
// is not a cons or not long enough, return Nil for efficiency reasons. The
// caller must know or not care.
fun args3(list: LObject): Triple<LObject, LObject, LObject> {
    var rest = list
    val a1 = rest.car
    rest = rest.cdr
    val a2 = rest.car
    rest = rest.cdr
    val a3 = rest.car
    return Triple(a1, a2, a3)
}

// Return the first four elements of list. This is not checked -- if the list
// is not a cons or not long enough, return Nil for efficiency reasons. The
// caller must know or not care.
fun args4(list: LObject): Array<LObject> {
    var rest = list
    val a1 = rest.car
    rest = rest.cdr
    val a2 = rest.car
    rest = rest.cdr
    val a3 = rest.car
    rest = rest.cdr
    val a4 = rest.car
    return arrayOf(a1, a2, a3, a4)
}


// Return the first five elements of list. This is not checked -- if the list
// is not a cons or not long enough, return Nil for efficiency reasons. The
// caller must know or not care.
fun args5(list: LObject): Array<LObject> {
    var rest = list
    val a1 = rest.car
    rest = rest.cdr
    val a2 = rest.car
    rest = rest.cdr
    val a3 = rest.car
    rest = rest.cdr
    val a4 = rest.car
    rest = rest.cdr
    val a5 = rest.car
    return arrayOf(a1, a2, a3, a4, a5)
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
    return collectedList { lc ->
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


fun dirlist(pathspec: String): LObject {
    val dirpath = Path(dirname(pathspec))
    val glob = basename(pathspec)
    return collectedList {
        for (path in dirpath.listDirectoryEntries(glob).sorted()) {
            it.add(makeString(path.toString()))
        }
    }
}


fun basename(pathname: String): String {
    if (pathname == "/" || pathname == "") {
        return pathname
    }
    var s = pathname
    while (s.endsWith("/")) {
        s = s.substring(0, s.length -1)
    }
    val parts = s.split("/")
    return parts[parts.size - 1]
}

fun dirname(pathname: String): String {
    if (pathname == "/" || pathname == "") {
        return pathname
    }
    var s = pathname
    while (s.endsWith("/")) {
        s = s.substring(0, s.length -1)
    }
    var absolute = s.startsWith("/")
    val parts = s.split("/")
    val result = parts.subList(0, parts.size - 1).joinToString("/")
    if (absolute && !result.startsWith("/")) {
        return "/"+ result
    }
    return result
}

fun isqrt(n: Long): Long {
    var op = n
    var res = 0L
    var one = 1L shl 62

    while (one > op) {
        one = one shr 2
    }
    while (one != 0L) {
        if (op >= res+one) {
            op -= res + one
            res += one shl 1 // <-- faster than 2 * one
        }
        res = res shr 1
        one = one shr 2
    }
    return res
}

// Return the checked int value of form if it evals to non-nil, or default
fun intValueOr(form: LObject, default: Int?, desc: String = ""): Int? {
    val value = eval(form)
    if (value === Nil) {
        return default
    } else {
        return intArg(value, desc)
    }
}
