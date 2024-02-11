
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

fun uninternedSymbol(name: String, immutable: Boolean = false): Symbol {
    val isKeyword = name.startsWith(":")
    val sym = Symbol(name, immutable or isKeyword)
    return sym
}

val symchars = "!#$%&*+-./0123456789:<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_abcdefghijklmnopqrstuvwxyz{}~".toSet()

fun isNumberString(s: String): Boolean {
    try {
        s.toDouble()
        return true
    } catch (e: Exception) {
        return false
    }
}

class Symbol(val name: String, val immutable: Boolean): LispObject(), List
{
    val props: MutableMap<Symbol, LispObject> = mutableMapOf()
    val descName = makeDescName()

    fun makeDescName(): String {
        var needQuoting = false

        if (name == "") {
            return "||"
        }
        if (name == ".") {
            return "|.|"
        }
        if (name[0] in "#,") {
            needQuoting = true
        } else if (isNumberString(name)) {
            needQuoting = true
        } else {
            for (ch in name) {
                if (ch in symchars) {
                    continue
                }
                needQuoting = true
                break
            }
        }

        if (needQuoting) {
            var result: MutableList<Char> = mutableListOf('|')
            for (ch in name) {
                if (ch in "|\\") {
                    result.add('\\')
                }
                result.add(ch)
            }
            result.add('|')
            return result.joinToString(separator="")
        } else {
            return name
        }
    }

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

    override fun car(): LispObject {
        if (this === Nil) {
            return Nil
        }
        throw ValueError("called car() of non-nil symbol $descName")
    }

    override fun cdr(): LispObject {
        if (this === Nil) {
            return Nil
        }
        throw ValueError("called cdr() of non-nil symbol $descName")
    }

    override fun iterator(): ListIterator {
        if (this === Nil) {
            return ListIterator(this)
        }
        throw ValueError("called iterator() of non-nil symbol $descName")
    }

    override fun bool() = this != Nil

    override fun toString() = name

    override fun description() = descName
}
