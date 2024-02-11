package org.w21.lyk


fun interface LocationHolder {
    fun location(): String
}

fun interface Callable {
    fun call(arglist: LispObject): LispObject
}

interface List: Iterable<LispObject> {
    fun car(): LispObject
    fun cdr(): LispObject
}

class ListIterator(var l: List): Iterator<LispObject> {
    val original = l
    override fun hasNext(): Boolean {
        return l != Nil
    }
    override fun next(): LispObject {
        val obj = l.car()
        val next_l = l.cdr()
        if (next_l is List) {
            l = next_l
            return obj
        }
        throw ValueError("iterated over improper list $original")
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
    return "${obj::class.simpleName}".lowercase()
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

class StrBuf {
    val buf = mutableListOf<String>()

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


