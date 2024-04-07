package org.w21.lyk

class LVector(elems: LObject): LObject() {
    // key: int, backed by an array [Object]
    val the_vector = mutableListOf<LObject>()

    init {
        if (!elems.isList()) {
            throw TypeError("Vector constructor called with "
                            + "non-list arg $elems")
        }
        for (elem in elems) {
            the_vector.add(elem)
        }
    }

    constructor(vararg elems: LObject) : this(Nil) {
        for (elem in elems) {
            the_vector.add(elem)
        }
    }

    constructor(length: Int, elem: LObject) : this(Nil) {
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

    override fun setAt(index: Int, value: LObject) {
            if (index >= 0 && index < the_vector.size) {
                the_vector[index] = value
                return
            }
            throw ValueError("invalid index $index for vector $this")
        }

    override fun equal(other: LObject): Boolean {
        if (this === other) {
            return true
        }
        if (other !is LVector) {
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
    override operator fun component1() = the_vector.get(0)
    override operator fun component2() = the_vector.get(1)
    operator fun component3() = the_vector.get(2)
    operator fun component4() = the_vector.get(3)
    operator fun component5() = the_vector.get(4)
    operator fun component6() = the_vector.get(5)
    operator fun component7() = the_vector.get(6)
}
