// The cons cell, a.k.a. pair

package org.w21.lyk


class LCons(var car: LObject, var cdr: LObject = Nil): LObject() {

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
            result.add(elem.car().desc())
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

    override fun car() = car

    override fun cdr() = cdr

    override fun length(): Int {
        var len = 0
        var cell: LObject = this
        while (cell is LCons) {
            len++
            cell = cell.cdr()
        }
        return len
    }
}
