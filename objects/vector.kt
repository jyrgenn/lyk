package org.w21.lyk

class LVector(elems: LObject): LObject(), LSeq {
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

    override val type = "vector"

    constructor(vararg elems: LObject) : this(Nil) {
        for (elem in elems) {
            the_vector.add(elem)
        }
    }

    constructor(elems: Iterable<LObject>) : this(Nil) {
        for (elem in elems) {
            the_vector.add(elem)
        }
    }

    constructor(length: Int, elem: LObject) : this(Nil) {
        for (n in 1..length) {
            the_vector.add(elem)
        }
    }

    override fun desc(seen: Set<Int>?): String {
        if (seen != null && this.id in seen) {
            return "..."
        }
        val seen1 = seen ?: setOf<Int>()

        var elems = StrBuf()
        for (elem in the_vector) {
            elems.add(elem.desc(seen1 + this.id))
        }
        return "#(" + elems.join(" ") + ")"
    }
    override fun toString() = desc(null)
    
    override val length get() = the_vector.size

    override fun delete(item: LObject): LObject {
        val lc = ListCollector()
        
        for (elem in this) {
            if (!elem.equal(item)) {
                lc.add(elem)
            }
        }
        return LVector(lc.list)
    }


    override fun getAt(index: Int): LObject {
        if (index >= 0 && index < the_vector.size) {
            return the_vector[index]
        }
        throw IndexError(this, index)
    }

    fun getAtOptional(index: Int): LObject? {
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
            throw IndexError(this, index)
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

    override fun elements(): LObject {
        return list2lisp(the_vector)
    }

    override fun copy(): LObject {
        return LVector(elements())
    }

    override fun subseq(start: Int, end: Int?): LObject {
        val real_end = (if (end == null) {
                            the_vector.size
                        } else {
                            end
                        })
        if (start < 0 || start > the_vector.size) {
            throw IndexError(this, start)
        }
        if (real_end < 0 || real_end > the_vector.size) {
            throw IndexError(this, real_end)
        }
        return LVector(the_vector.subList(start, real_end))
    }

    override fun find(start: Int, end: Int?, last: Boolean,
                      predicate: (LObject) -> Boolean): LObject {
        var result: LObject = Nil
        var index = -1
        for (elem in the_vector) {
            index++
            if (index < start) {
                continue
            }
            if (end != null && index >= end) {
                break
            }
            if (predicate(elem)) {
                if (last) {
                    result = elem
                } else {
                    return elem
                }
            }
        }
        return result
    }

    override fun reversed(): LObject {
        return LVector(the_vector.reversed())
    }

    class VectorIterator(val vector: LVector): Iterator<LObject> {
        var nextIndex = 0
        
        override fun hasNext(): Boolean {
            return nextIndex < vector.the_vector.size
        }

        override fun next(): LObject {
            return vector.getAt(nextIndex++)
        }
    }

    override fun iterator(): Iterator<LObject> = VectorIterator(this)
}
