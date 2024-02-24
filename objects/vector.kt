package org.w21.lyk

class Vector(): LispObject() {
    // key: int, backed by an array [Object]
    val the_vector = mutableListOf<LispObject>()

    constructor(elems: LispObject) : this() {
        if (!elems.isList()) {
            throw TypeError("Vector constructor called with "
                            + "non-list arg $elems")
        }
        for (elem in elems) {
            the_vector.add(elem)
        }
    }

    constructor(vararg elems: LispObject) : this() {
        for (elem in elems) {
            the_vector.add(elem)
        }
    }

    constructor(length: Int, elem: LispObject) : this() {
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
    
    fun get(index: Int): LispObject {
             if (index >= 0 && index < the_vector.size) {
            return the_vector[index]
        }
        throw ValueError("invalid index $index for vector $this")
    }

    fun atIndex(index: Int): LispObject? {
        if (index >= 0 && index < the_vector.size) {
            return the_vector[index]
        }
        return null
    }

    fun set(index: Int, value: LispObject) {
            if (index >= 0 && index < the_vector.size) {
                the_vector[index] = value
            }
            throw ValueError("invalid index $index for vector $this")
        }

    override fun equal(other: LispObject): Boolean {
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
