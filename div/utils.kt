// dig. utilities

package org.w21.lyk


fun interface LocationHolder {
    fun location(): String
}

fun interface Callable {
    fun call(arglist: LispObject): LispObject
}

fun list2lisp(elems: List<LispObject>): LispObject {
    val lc = ListCollector()
    for (elem in elems) {
        lc.add(elem)
    }
    return lc.list()
}

class ListIterator(var l: LispObject): Iterator<LispObject> {
    val original = l                    // keep the original list for an error

    override fun hasNext() =
        when (l) {
            is Cons -> true
            else -> false
        }

    override fun next(): LispObject {
        val obj = l.car()
        l = l.cdr()
        return obj
    }
}


class ListCollector(vararg objects: LispObject) {
    var head: LispObject = Nil
    var last: LispObject? = null

    init {
        for (obj in objects) {
            add(obj)
        }
    }

    fun add(arg: LispObject) {
        val newpair = Cons(arg, Nil)
        if (last is Cons) {
            (last as Cons).rplacd(newpair)
        } else {
            head = newpair
        }
        last = newpair
    }

    fun lastcdr(arg: LispObject) {
        if (last is Cons) {
            (last as Cons).rplacd(arg)
        } else {
            last = arg
            head = arg
        }
    }

    fun list() = head
}

fun typeOf(obj: Any): String {
    if (obj is LispString) {
        return "string"
    }
    return "${obj::class.simpleName}"//.lowercase()
}

class CharBuf {
    val chars = mutableListOf<Char>()

    constructor() {
        
    }
    constructor(ch: Char) {
        chars.add(ch)
    }

    fun add(ch: Char) {
        chars.add(ch)
    }
    
    override fun toString(): String {
        return chars.joinToString(separator = "")
    }
}

class StrBuf() {
    val buf = mutableListOf<String>()

    constructor(s: String) : this() {
        buf.add(s)
    }
    fun add(ch: Char) {
        buf.add(ch.toString())
    }
    fun add(s: String) {
        buf.add(s)
    }
    
    override fun toString(): String {
        return buf.joinToString(separator = "")
    }

    fun join(separator: String): String {
        return buf.joinToString(separator = separator)
    }
}

fun arrayIntern(array: Array<String>): List<Symbol> {
    val symbols = mutableListOf<Symbol>()
    for (elem in array) {
        symbols.add(Symbol.intern(elem))
    }
    return symbols
}

fun mapInternKeys(map: Map<String, LispObject>): Map<Symbol, LispObject> {
    val result = mutableMapOf<Symbol, LispObject>()
    for ((key, value) in map) {
        result[Symbol.intern(key)] = value
    }
    return result
}

fun pairsInternFirst(pairs: Array<Pair<String, LispObject>>
): List<Pair<Symbol, LispObject>> {
    var result = mutableListOf<Pair<Symbol, LispObject>>()
    for ((key, value) in pairs) {
        result.add(Pair<Symbol, LispObject>(Symbol.intern(key), value))
    }
    return result
}

