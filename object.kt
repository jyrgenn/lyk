// Parent class of all objects

package org.w21.lyk

private var objectCounter = 0


abstract class LispObject {
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

    // This will be used by the system, e.g. for expansion in "bla
    // $value" String templates. It will not necessarily be the form
    // that can be read in again by the reader.
    override fun toString(): String {
        return "Object[$id]{${type()}}"
    }

    // The output of this shall, if at all possible, be sufficent to
    // be read by the reader to re-create the object.
    open fun desc() = toString()

    open fun equal(other: LispObject) = false

}
