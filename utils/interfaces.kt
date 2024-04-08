// dig. utilities

package org.w21.lyk


fun interface LocationHolder {
    fun location(): String
}

interface LSeq {
    fun getAt(index: Int): LObject
    fun setAt(index: Int, value: LObject)
}
