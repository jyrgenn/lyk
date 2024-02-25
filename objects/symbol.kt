
// The symbol

package org.w21.lyk

val symbolTable: MutableMap<String, Symbol> = mutableMapOf()

fun makeGlobal(name: String, value: LispObject = Nil) {
    val symbol = Symbol.intern(name)
    symbol.setValue(value, silent = true)
}

val symchars = "!#$%&*+-./0123456789:<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^" +
    "_abcdefghijklmnopqrstuvwxyz{}~".toSet()

fun isNumberString(s: String): Boolean {
    try {
        s.toDouble()
        return true
    } catch (e: Exception) {
        return false
    }
}

class Symbol(val name: String, val immutable: Boolean): LispObject()
{
    val props = mutableMapOf<Symbol, LispObject>()
    val descName = makeDescName()
    var function: Function? = null

    companion object {
        fun intern(name: String, immutable_and_selfvalued: Boolean = false
        ): Symbol {
            if (name in symbolTable.keys) {
                return symbolTable[name] ?:
                    throw Exception(
                        "symbol $name in symbolTable is not in symbolTable"
                    )
            }
            val sym = Symbol(name,
                             immutable_and_selfvalued or name.startsWith(":"))
            symbolTable[name] = sym
            if (immutable_and_selfvalued) {
                rootEnv.setValue(sym, sym)
            }
            return sym
        }

        fun uninterned(name: String): Symbol {
            val sym = Symbol(name, name.startsWith(":"))
            return sym
        }
    }                           // end companion object

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

    override fun isList() = this === Nil

    override fun isKeyword(): Boolean {
        return name.startsWith(':')
    }

    // Get the value of a property of Symbol. If it is not defined, return Nil.
    fun getprop(name: Symbol): LispObject {
        return props[name] ?: Nil
    }
    
    // Set the value of a property of Symbol.
    fun putprop(name: Symbol, value: LispObject) {
        props[name] = value
    }


    fun setFunction(func: Function?, silent: Boolean = false) {
        if (immutable) {
            throw ImmutableError(this, true)
        }
        if (function is Builtin && !silent) {
            val what = if (func == null) "unbinding" else "redefining"
            warn("$what builtin function $name")
        }
        function = func
    }

    fun setValue(newvalue: LispObject, silent: Boolean = false) {
        if (immutable) {
            throw ImmutableError(this, false)
        } else {
            if (!(currentEnv.setValue(this, newvalue) || silent)) {
                warn("setting unbound variable $this to $newvalue")
            }
        }
    }

    fun bind(newValue: LispObject) {
        if (this.immutable) {
            throw Exception("symbol $this is immutable")
        }
        currentEnv.bind(this, newValue)
    }

    fun getValue() = currentEnv.getValue(this)

    fun getValueOptional() = currentEnv.getValueMaybe(this)

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

    override fun isAtom() = true

    override fun bool() = this != Nil

    override fun toString() = name

    override fun desc() = descName

    override fun dump(): String {
        return super.dump() + "($descName:$function:$props)"
    }
}
