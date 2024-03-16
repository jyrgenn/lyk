
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
                  is LNumber -> elem.value
                  else -> elem.toString()
              })
    }
    return l
}

class ListIterator(var l: LispObject): Iterator<LispObject> {
    val original = l                    // keep the original list for an error

    override fun hasNext() =
        when (l) {
            is LCons -> true
            else -> false
        }

    override fun next(): LispObject {
        val obj = l.car()
        l = l.cdr()
        return obj
    }
}


class ListCollector() {
    var head: LispObject = Nil
    var last: LispObject? = null

    fun add(arg: LispObject) {
        val newpair = LCons(arg, Nil)
        if (last is LCons) {
            (last as LCons).rplacd(newpair)
        } else {
            head = newpair
        }
        last = newpair
    }

    fun lastcdr(arg: LispObject) {
        if (last is LCons) {
            (last as LCons).rplacd(arg)
        } else {
            last = arg
            head = arg
        }
    }

    fun list() = head
}

