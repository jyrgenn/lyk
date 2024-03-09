
package org.w21.lyk

fun collectedList(closure: (lc: ListCollector) -> Unit): LispObject {
    val lc = ListCollector()
    closure(lc)
    return lc.list()
}

fun list2lisp(elems: Collection<LispObject>): LispObject {
    val lc = ListCollector()
    for (elem in elems) {
        lc.add(elem)
    }
    return lc.list()
}

fun valueList(elems: LispObject): List<Any> {
    val l = mutableListOf<Any>()

    for (elem in elems) {
        l.add(when (elem) {
                  is Number -> elem.value
                  else -> elem.toString()
              })
    }
    return l
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

