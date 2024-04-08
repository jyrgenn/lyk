// dig. utilities

package org.w21.lyk


fun interface LocationHolder {
    fun location(): String
}

interface LSeq {
    fun getAt(index: Int): LObject
    fun setAt(index: Int, value: LObject)
    operator fun iterator(): Iterator<LObject>
    fun copy(): LObject
    fun elements(): LObject
}
