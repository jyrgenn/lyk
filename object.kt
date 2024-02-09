// Parent class of all objects

package org.w21.lyk

private var objectCounter = 0


abstract class LispObject {
    val id: Int
    
    init {
        objectCounter += 1
        id = objectCounter
    }
    

    fun bool(): Boolean = true

    open fun description() = toString()

    open fun length(): Int {
        // should throw a more specific Exception some time
        throw Exception("$this has no length")
    }

    override fun toString(): String {
        return "Object[$id]{type}"
    }
}
