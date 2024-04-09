// The cons cell, a.k.a. pair

package org.w21.lyk


class LCons(override var car: LObject,
            override var cdr: LObject = Nil): LObject(), LSeq {

    init {
        debug(debugConsSym) {
            "(cons $car $cdr)"
        }
        consCounter++
    }

    override fun toString(): String {
        val result = StrBuf("(")

        var elem: LObject = this
        while (elem is LCons) {
            result.add(elem.car.desc())
            if (elem.cdr !== Nil) {
                result.add(" ")
            }
            elem = elem.cdr
        }
        if (elem !== Nil) {
            result.add(". ")
            result.add(elem.desc())
        }
        result.add(")")
        return result.toString()
    }

    override fun isList() = true

    override fun desc() = toString()

    override fun length(): Int {
        var len = 0
        var cell: LObject = this
        while (cell is LCons) {
            len++
            cell = cell.cdr
        }
        return len
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

    override fun equal(other: LObject): Boolean {
        return other is LCons
            && car.equal(other.car)
            && cdr.equal(other.cdr)
    }

    fun toArray(): Array<LObject> {
        val valueArray = Array<LObject>(length()) { Nil }

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
                else -> false
            }

        override fun next(): LObject {
            val obj = l.car
            l = l.cdr
            return obj
        }
    }

    override fun iterator(): Iterator<LObject> = ConsIterator(this)
}
