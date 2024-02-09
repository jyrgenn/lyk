// The cons cell, a.k.a. pair

package org.w21.lyk

data class Cons(var car: LispObject, var cdr: LispObject = Nil): LispObject() {

    override fun toString(): String {
        return "(${car.toString()} . ${cdr.toString()})"
    }
}
