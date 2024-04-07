// The cons cell, a.k.a. pair

package org.w21.lyk


class LCons(override var car: LObject,
            override var cdr: LObject = Nil): LObject() {

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
        throw IndexError("index $index for ${typeOf(this)} too large: $this")
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
}
