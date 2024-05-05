// The symbol

package org.w21.lyk

val symbolTable: MutableMap<String, LSymbol> = mutableMapOf()

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

class LSymbol(val name: String, val immutable: Boolean): LObject(), LSeq
{
    val props = mutableMapOf<LSymbol, LObject>()
    val descName = makeDescName()
    var function: LFunction? = null

    override val type = "symbol"

    companion object {
        fun interned(name: String, immutable_and_selfvalued: Boolean = false
        ): LSymbol {
            if (name in symbolTable.keys) {
                return symbolTable[name] ?:
                    throw Exception(
                        "symbol $name in symbolTable is not in symbolTable"
                    )
            }
            val i_and_sv = immutable_and_selfvalued || name.startsWith(":")
            val sym = LSymbol(name, i_and_sv)
            symbolTable[name] = sym
            if (i_and_sv) {
                rootEnv.setValue(sym, sym)
            }
            return sym
        }

        fun uninterned(name: String): LSymbol {
            val sym = LSymbol(name, name.startsWith(":"))
            return sym
        }

        fun makeGlobal(name: String, value: LObject = Nil): LSymbol {
            val symbol = intern(name)
            rootEnv.the_env[symbol] = value
            return symbol
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

    override val length: Int
        get() {
            if (this === Nil) {
                return 0
            }
            return super.length
        }
    
    override fun delete(item: LObject): LObject {
        if (this === Nil) {
            return Nil
        }
        throw TypeError("not a sequence: $this")
    }

    override fun isKeyword(): Boolean {
        return name.startsWith(':')
    }

    // Get the value of a property of LSymbol. If it is not defined, return Nil.
    fun getprop(name: LSymbol): LObject {
        return props[name] ?: Nil
    }
    
    // Set the value of a property of LSymbol.
    fun putprop(name: LSymbol, value: LObject) {
        props[name] = value
    }


    fun setFunction(func: LFunction?, silent: Boolean = false) {
        if (function is LBuiltin && !silent) {
            val what = if (func == null) "unbinding" else "redefining"
            warn("$what builtin function $name")
        }
        function = func
    }

    fun setValue(newvalue: LObject, silent: Boolean = false) {
        if (immutable) {
            throw ImmutableError(this, false)
        } else {
            if (!silent && getValueOptional() == null) {
                warn("setting unbound variable $this to $newvalue")
            }
            currentEnv.setValue(this, newvalue)
        }
    }

    fun bind(newValue: LObject) {
        if (this.immutable) {
            throw Exception("symbol $this is immutable")
        }
        currentEnv.bind(this, newValue)
    }

    fun getValue() = currentEnv.getValue(this)

    fun getValueOptional() = currentEnv.getValueMaybe(this) 

    fun setProp(name: LSymbol, value: LObject) {
        props[name] = value
    }

    fun getProp(name: LSymbol, default: LObject): LObject {
        return props[name] ?: default
    }

    fun remProp(name: LSymbol): LObject {
        val result = props[name] ?: Nil
        props.remove(name)
        return result
    }

    override fun getAt(index: Int): LObject {
        if (this === Nil) {
            throw IndexError(this, index)
        }
        throw TypeError("not a sequence: $this")
    }

    override fun setAt(index: Int, value: LObject) {
        if (this === Nil) {
            throw IndexError(this, index)
        }
        throw TypeError("not a sequence: $this")
    }

    override fun elements(): LObject {
        if (this === Nil) {
            return Nil
        }
        throw TypeError("not a sequence: $this")
    }

    override fun copy() = elements()

    override fun subseq(start: Int, end: Int?): LObject {
        if (this === Nil) {
            throw IndexError(
                "invalid indexes [$start, ${end ?: "nil"}) for empty list")
        }
        throw TypeError("not a sequence: $this")
    }

    override fun find(start: Int, end: Int?, last: Boolean,
                      predicate: (LObject) -> Boolean): LObject {
        if (this === Nil) {
            return Nil
        }
        throw TypeError("not a sequence: $this")
    }

    override fun reversed(): LObject {
        if (this === Nil) {
            return Nil
        }
        throw TypeError("not a sequence: $this")
    }

    override var car: LObject
        get() {
            if (this === Nil) {
                return Nil
            }
            throw ValueError("called car() of non-nil symbol $descName")
        }
        set(_) { throw ValueError("called car() of non- $descName") }

    override var cdr: LObject
        get() {
            if (this === Nil) {
                return Nil
            }
            throw ValueError("called cdr() of non-nil symbol $descName")
        }
        set(_) { throw ValueError("called cdr() of non- $descName") }

    override fun isAtom() = true

    override fun bool() = this != Nil

    override fun toString() = name

    override fun desc(seen: Set<Int>?) = descName

    override fun dump(): String {
        return super.dump() + "($descName:$function:$props)"
    }

    override fun compareTo(other: LObject): Int {
	if (other is LSymbol) {
	    if (name < other.name) {
		return -1
	    } else if (name > other.name) {
		return 1
	    } else {
		return 0
	    }
	} else {
	    throw compareError(other)
	}
    }

    class SymbolIterator(val sym: LObject): Iterator<LObject> {
        override fun hasNext(): Boolean {
            if (sym === Nil) {
                return false
            }
            throw TypeError(sym, "sequence", "iterator")
        }

        override fun next(): LObject {
            if (sym === Nil) {
                throw IndexError("no next on empty list")
            }
            throw TypeError(sym, "sequence", "iterator")
        }
    }

    override fun iterator(): Iterator<LObject> = SymbolIterator(this)
}

fun intern(name: String, immutable_and_selfvalued: Boolean = false) =
    LSymbol.interned(name, immutable_and_selfvalued)
