// dig. utilities

package org.w21.lyk


fun interface LocationHolder {
    fun location(): String
}

fun interface Callable {
    fun call(arglist: LispObject): LispObject
}

