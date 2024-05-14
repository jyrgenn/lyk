// The cons cell, a.k.a. pair

package org.w21.lyk


class LCons(override var car: LObject,
            override var cdr: LObject): LObject(), LSeq {

    init {
        debug(debugConsSym) {
            "(cons $car $cdr)"
        }
        consCounter++
    }

    override val type = "cons"

    override fun toString() = desc(null)

    override fun isList() = true

    override fun desc(seen: Set<Int>?): String {
        if (seen != null && this.id in seen) {
            return "..."
        }
        val seen1 = (seen ?: setOf<Int>()) + this.id

        val result = StrBuf("(")
        val car_s = car.desc(seen1)
        result.add(car_s)
        when (cdr) {
            Nil -> result.add(")")
            is LCons -> {
                if (cdr.id in seen1) {
                    result.add(" . ...)")
                } else {
                    result.add(" ")
                    result.add(cdr.desc(seen1).substring(1))
                }
            }
            else -> {
                result.add(" . ")
                result.add(cdr.desc(seen1))
                result.add(")")
            }
        }
        return result.toString()
    }

    override val length: Int
        get () {
            var len = 0
            var cell: LObject = this
            while (cell is LCons) {
                len++
                cell = cell.cdr
            }
            return len
        }
    
    override fun delete(item: LObject): LObject {
        val lc = ListCollector()
        
        for (elem in this) {
            if (!elem.equal(item)) {
                lc.add(elem)
            }
        }
        return lc.list
    }

    override fun getAt(index: Int): LObject {
        var l: LObject = this
        var i = 0
        while (l is LCons) {
            if (i++ == index) {
                return l.car
            }
            l = l.cdr
        }
        throw IndexError(this, index)
    }
    override fun setAt(index: Int, value: LObject) {
        var l: LObject = this
        var i = 0
        while (l is LCons) {
            if (i++ == index) {
                l.car = value
                return
            }
            l = l.cdr
        }
        throw IndexError(this, index)
    }

    override fun elements(): LObject {
        var l: LObject = this
        return collectedList {
            while (l is LCons) {
                it.add(l.car)
                l = l.cdr
            }
            if (l !== Nil) {
                throw TypeError("not a proper list: $this")
            }
        }
    }

    override fun copy() = elements()

    override fun subseq(start: Int, end: Int?): LObject {
        if (start < 0) {
            IndexError(this, start)
        }
        if (end != null && end < 0) {
            IndexError(this, end)
        }
        var l: LObject = this
        var lc = ListCollector()
        var i = 0
        while (l is LCons) {
            if (end != null && i >= end) {
                break
            }
            if (i >= start) {
                lc.add(l.car)
            }
            i++
            l = l.cdr
        }
        if (i <= start) {
            IndexError(this, start)
        }
        if (end != null && i < end) {
            IndexError(this, end)
        }
        return lc.list
    }

    override fun find(start: Int, end: Int?, last: Boolean,
                      predicate: (LObject) -> Boolean): LObject {
        var result: LObject = Nil
        var index = -1
        for (elem in this) {
            index++
            if (index < start) {
                continue
            }
            if (end != null && index >= end) {
                break
            }
            if (predicate(elem)) {
                if (last) {
                    result = elem
                } else {
                    return elem
                }
            }
        }
        return result
    }

    override fun position(start: Int, end: Int?, last: Boolean,
                          predicate: (LObject) -> Boolean): Int {
        var result = -1
        var index = -1
        for (elem in this) {
            index++
            if (index < start) {
                continue
            }
            if (end != null && index >= end) {
                break
            }
            if (predicate(elem)) {
                if (last) {
                    result = index
                } else {
                    return index
                }
            }
        }
        return result
    }

    override fun reversed(): LObject {
        var l: LObject = Nil
        for (elem in this) {
            l = LCons(elem, l)
        }
        return l
    }

    override fun equal(other: LObject): Boolean {
        return other is LCons
            && car.equal(other.car)
            && cdr.equal(other.cdr)
    }

    fun toArray(): Array<LObject> {
        val valueArray = Array<LObject>(length) { Nil }

        var index = 0
        for (elem in this) {
            valueArray[index++] = elem
        }
        return valueArray
    }

    class ConsIterator(var l: LObject): Iterator<LObject> {
        val original = l                // keep the original list for an error

        override fun hasNext() =
            when (l) {
                is LCons -> true
                Nil -> false
                else -> throw TypeError("not a proper list: $original")

            }
        override fun next(): LObject {
            val obj = l.car
            l = l.cdr
            return obj
        }
    }

    override fun iterator(): Iterator<LObject> = ConsIterator(this)
}
