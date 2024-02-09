
// The symbol

package org.w21.lyk

class Symbol(val name: String, val immutable: Boolean = false): LispObject()
{
    val props: MutableMap<LispObject, LispObject> = mutableMapOf()

    override fun toString() = name
}
