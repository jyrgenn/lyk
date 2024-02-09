// Parent class of all objects

package org.w21.lyk

private var objectCounter = 0

fun nextObjectID(): Int {
    objectCounter += 1
    return objectCounter
}

abstract class LispObject {
    val id: Int
    
    constructor() {
        id = nextObjectID()
    }
    

    fun bool(): Boolean = true

    override fun toString(): String {
        return "Object[$id]{type}"
    }
}
