
package org.w21.lyk

fun collectedList(closure: (lc: ListCollector) -> Unit): LObject {
    val lc = ListCollector()
    closure(lc)
    return lc.list()
}

fun list2lisp(elems: Collection<LObject>): LObject {
    val lc = ListCollector()
    for (elem in elems) {
        lc.add(elem)
    }
    return lc.list()
}

fun valueList(elems: LObject): List<Any> {
    val l = mutableListOf<Any>()

    for (elem in elems) {
        l.add(when (elem) {
                  is LNumber -> elem.value
                  else -> elem.toString()
              })
    }
    return l
}

class ListIterator(var l: LObject): Iterator<LObject> {
    val original = l                    // keep the original list for an error

    override fun hasNext() =
        when (l) {
            is LCons -> true
            else -> false
        }

    override fun next(): LObject {
        val obj = l.car
        l = l.cdr
        return obj
    }
}


class ListCollector() {
    var head: LObject = Nil
    var last: LObject? = null

    fun add(arg: LObject) {
        val newpair = LCons(arg, Nil)
        if (last is LCons) {
            (last as LCons).cdr = newpair
        } else {
            head = newpair
        }
        last = newpair
    }

    fun lastcdr(arg: LObject) {
        if (last is LCons) {
            (last as LCons).cdr = arg
        } else {
            last = arg
            head = arg
        }
    }

    fun list() = head
}

