// Parent class of all objects

package org.w21.lyk

private var objectCounter = 0


abstract class LispObject: Iterable<LispObject>, Comparable<LispObject> {
    val id: Int
    
    init {
        objectCounter += 1
        id = objectCounter
    }
    

    open fun bool() = true

    open fun isAtom() = false

    open fun isList() = false

    fun type() = typeOf(this)

    open fun length(): Int {
        throw ValueError("$this has no length")
    }

    open fun isKeyword() = false

    // This will be used by the system, e.g. for expansion in "bla
    // $value" String templates. It will not necessarily be the form
    // that can be read in again by the reader.
    override fun toString(): String {
        return dump()
    }

    // Print as much information about the object as can be helpful debugging.
    open fun dump() = "${type()}[$id]"

    // The output of this shall, if at all possible, be sufficent to
    // be read by the reader to re-create the object.
    open fun desc() = toString()

    open fun equal(other: LispObject) = false

    override fun iterator() = ObjectIterator(this)

    operator fun component1(): LispObject {
        return (this as? Cons)?.car() ?:
            throw TypeError("$this is not a pair")
    }

    operator fun component2(): LispObject {
        return (this as? Cons)?.cdr() ?:
            throw TypeError("$this is not a pair")
    }

    fun toBoolean() = this != Nil

    open fun car(): LispObject {
        throw LispError("called car on non-list $this")
    }

    open fun cdr(): LispObject {
        throw LispError("called cdr on non-list $this")
    }

    open override fun compareTo(other: LispObject): Int {
        throw TypeError("cannot compare ${typeOf(this)} to ${typeOf(other)}")
    }
}


class ObjectIterator(var theObject: LispObject): Iterator<LispObject> {
    val original = theObject
    var nextIndex = 0

    override fun hasNext(): Boolean {
        val ob = theObject
        when (ob) {
            Nil -> return false
            is Cons -> return true
            is Vector -> return nextIndex < ob.the_vector.size
            else ->
                throw ValueError("iterating over improper list: $original")
        }
    }

    override fun next(): LispObject {
        val ob = theObject
        when (ob) {
            Nil ->
                throw ValueError("called next() after end of list: $original")
            is Cons -> {
                val retVal = ob.car()
                theObject = ob.cdr()
                return retVal
            }
            is Vector -> {
                return ob.get(nextIndex++)
            }
            else -> ValueError("iterating over non-sequence: $original")
        }
        return Nil
    }
}
