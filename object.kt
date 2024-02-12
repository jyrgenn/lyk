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

    open fun desc() = toString()

    fun type() = typeOf(this)

    open fun length(): Int {
        throw ValueError("$this has no length")
    }

    override fun toString(): String {
        return "Object[$id]{${type()}}"
    }

    open fun equal(other: LispObject) = false

}
