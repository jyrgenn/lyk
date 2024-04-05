
package org.w21.lyk


// return a list with the first n elements of list; all for 0
fun firstN(list: LObject, n: Int): LObject {
    val lc = ListCollector()
    var l = list

    for (i in 0..<n) {
        if (l === Nil) {
            return lc.list
        }
        lc.add(l.car)
        l = l.cdr
    }
    return lc.list
}

fun collectedList(closure: (lc: ListCollector) -> Unit): LObject {
    val lc = ListCollector()
    closure(lc)
    return lc.list
}

fun list2lisp(elems: Collection<LObject>): LObject {
    val lc = ListCollector()
    for (elem in elems) {
        lc.add(elem)
    }
    return lc.list
}

fun valueArray(elems: LObject): Array<Any> {
    val a = Array<Any>(elems.length()) { 0 }

    var index = 0
    for (elem in elems) {
        a[index++] = when (elem) {
            is LNumber -> if (elem.isLong())
                              elem.value.toLong()
                          else
                              elem.value
            else -> elem.toString()
        }
    }
    return a
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


class ListCollector(): Iterable<LObject> {
    var list: LObject = Nil
    var last: LObject? = null

    fun add(arg: LObject) {
        val newpair = LCons(arg, Nil)
        if (last is LCons) {
            (last as LCons).cdr = newpair
        } else {
            list = newpair
        }
        last = newpair
    }

    fun lastcdr(arg: LObject) {
        if (last is LCons) {
            (last as LCons).cdr = arg
        } else {
            last = arg
            list = arg
        }
    }

    val size get() = list.length()

    override fun iterator() = ListIterator(this.list)
}

