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
    fun subseq(start: Int, end: Int?): LObject
    fun reversed(): LObject
    fun delete(item: LObject): LObject
    fun find(start: Int, end: Int?, last: Boolean,
             predicate: (LObject) -> Boolean): LObject
    val length: Int
}
