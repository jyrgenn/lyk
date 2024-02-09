
// The symbol

package org.w21.lyk

val symbolTable: MutableMap<String, Symbol> = mutableMapOf()

fun intern(name: String, immutable: Boolean = false): Symbol {
    if (name in symbolTable.keys) {
        return symbolTable[name] ?:
            throw Exception(
                "symbol $name in symbolTable is not in symbolTable"
            )
    }
    val isKeyword = name.startsWith(":")
    val sym = Symbol(name, immutable or isKeyword)
    symbolTable[name] = sym
    return sym
}


class Symbol(val name: String, val immutable: Boolean): LispObject()
{
    val props: MutableMap<LispObject, LispObject> = mutableMapOf()

    fun setValue(newvalue: LispObject) {
        if (immutable) {
            throw Exception("symbol $this is immutable")
        } else {
            currentEnv.setValue(this, newvalue)
        }
    }

    fun getValue() = currentEnv.getValue(this)

    fun setProp(name: Symbol, value: LispObject) {
        props[name] = value
    }

    fun getProp(name: Symbol): LispObject {
        return props[name] ?: Nil
    }

    override fun bool() = this != Nil

    override fun toString() = name
}
