// The cons cell, a.k.a. pair

package org.w21.lyk


data class Cons(var car: LispObject,
                var cdr: LispObject = Nil): LispObject(), LispList {

    override fun toString(): String {
        val result = StrBuf("(")

        var elem: LispObject = this
        while (elem is Cons) {
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

    override fun desc() = toString()

    fun rplaca(newcar: LispObject) {
        car = newcar
    }

    fun rplacd(newcdr: LispObject) {
        cdr = newcdr
    }

    override fun car(): LispObject {
        return car
    }

    override fun cdr(): LispObject {
        return cdr
    }

    override fun iterator(): ListIterator {
        return ListIterator(this)
    }
}
