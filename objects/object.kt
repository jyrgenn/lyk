// Parent class of all objects

package org.w21.lyk

private var objectCounter = 0


abstract class LispObject: Iterable<LispObject> {
    val id: Int
    
    init {
        objectCounter += 1
        id = objectCounter
    }
    

    open fun bool() = true

    open fun isAtom() = false

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
}


class ObjectIterator(var theObject: LispObject): Iterator<LispObject> {
    val original = theObject

    override fun hasNext(): Boolean {
        if (theObject == Nil) {
            return false
        }
        if (theObject is Cons) {
            return true
        }
        throw ValueError("iterating over not a proper list: $original")
    }

    override fun next(): LispObject {
        if (theObject is Cons) {
            val ob = theObject as Cons
            val retVal = ob.car()
            theObject = ob.cdr()
            return retVal
        }
        if (theObject === Nil) {
            throw ValueError("called next() after end of list: $original")
        }
        throw ValueError("iterating over improper list: $original")
    }
}
