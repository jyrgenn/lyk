package org.w21.lyk

class Vector(): LObject() {
    // key: int, backed by an array [Object]
    val the_vector = mutableListOf<LObject>()

    constructor(elems: LObject) : this() {
        if (!elems.isList()) {
            throw TypeError("Vector constructor called with "
                            + "non-list arg $elems")
        }
        for (elem in elems) {
            the_vector.add(elem)
        }
    }

    constructor(vararg elems: LObject) : this() {
        for (elem in elems) {
            the_vector.add(elem)
        }
    }

    constructor(length: Int, elem: LObject) : this() {
        for (n in 1..length) {
            the_vector.add(elem)
        }
    }

    override fun desc(): String {
        var elems = StrBuf()
        for (elem in the_vector) {
            elems.add(elem.desc())
        }
        return "#(" + elems.join(" ") + ")"
    }
    override fun toString() = desc()
    
    override fun length() = the_vector.size

    fun get(index: Int): LObject {
            if (index >= 0 && index < the_vector.size) {
                return the_vector[index]
            }
            throw ValueError("invalid index $index for vector $this")
        }

    fun atIndex(index: Int): LObject? {
        if (index >= 0 && index < the_vector.size) {
            return the_vector[index]
        }
        return null
    }

    fun set(index: Int, value: LObject): LObject {
            if (index >= 0 && index < the_vector.size) {
                the_vector[index] = value
                return value
            }
            throw ValueError("invalid index $index for vector $this")
        }

    override fun equal(other: LObject): Boolean {
        if (this === other) {
            return true
        }
        if (other !is Vector) {
            return false
        }
        if (the_vector.size != other.the_vector.size) {
            return false
        }
        for (i in 0..<the_vector.size) {
            if (!the_vector[i].equal(other.the_vector[i])) {
                return false
            }
        }
        return true
    }

}
