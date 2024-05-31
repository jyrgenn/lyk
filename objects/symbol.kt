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

class LSymbol(val name: String, val immutable: Boolean,
              val read_only: Boolean): LObject(), LSeq
{
    val props = mutableMapOf<LSymbol, LObject>()
    val descName = makeDescName()
    var function: LFunction? = null

    override val obtype = "symbol"

    companion object {
        fun interned(name: String,
                     immutable: Boolean = false,
                     selfvalued: Boolean = false,
                     read_only: Boolean = false): LSymbol {
            if (name in symbolTable.keys) {
                return symbolTable[name] ?:
                    throw Exception(
                        "symbol $name in symbolTable is not in symbolTable"
                    )
            }
            val is_keyword = name.startsWith(":")
            val sym = LSymbol(name,
                              immutable || is_keyword || read_only,
                              read_only)
            symbolTable[name] = sym
            if (selfvalued || is_keyword) {
                rootEnv.setValue(sym, sym)
            }
            return sym
        }

        fun uninterned(name: String): LSymbol {
            val sym = LSymbol(name, name.startsWith(":"), false)
            return sym
        }

        fun makeGlobal(name: String, value: LObject = Nil,
                       read_only: Boolean = false): LSymbol {
            val symbol = interned(name, false, false, read_only)
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
            if (read_only) {
                throw ReadOnlyError(this)
            } else {
                throw ImmutableError(this)
            }
        } else {
            if (!silent && getValueOptional() == null) {
                warn("setting unbound variable $this to $newvalue")
            }
            currentEnv.setValue(this, newvalue)
        }
    }

    // This is to set the value of a read-only system symbol.
    fun setROValue(newvalue: LObject) {
        rootEnv.setValue(this, newvalue)
    }

    fun bind(newValue: LObject) {
        if (immutable) {
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

    override fun toBoolean() = this !== Nil

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

    /////// Implementation of the LSeq interface ///////

    // Get the element of the sequence at the specified index;
    // return the default value if index isn't in the sequence,
    // or if the default value is null, raise an error.
    override fun getAt(index: Int, default: LObject?): LObject {
        if (this === Nil) {
            if (default == null) {
                throw IndexError(this, index)
            }
            return default
        }
        throw TypeError("not a sequence: $this")
    }
    
    // Set the element of the sequence at the specified index to the
    // specified value; raise an error if the index isn't in the
    // sequence or if the sequence is immutable (meaning Strings, at
    // least for now).
    @Suppress("UNUSED_PARAMETER")
    override fun setAt(index: Int, value: LObject) {
        if (this === Nil) {
            throw IndexError(this, index)
        }
        throw TypeError("not a sequence: $this")
    }

    // Return an iterator for the sequence.
    override operator fun iterator(): Iterator<LObject> = SymbolIterator(this)

    // Return a (shallow) copy of the sequence.
    // this is a null operation for a string, because identical content means
    // identical string object anyway
    override fun copy() = elements()

    // Return a list with the elements of the sequence.
    override fun elements(): LObject {
        if (this === Nil) {
            return Nil
        }
        throw TypeError("not a sequence: $this")
    }

    // Return a subsequence of the sequence. If start is null, start
    // at index 0; if end is null, end at the end of the sequence.
    // If both are null, return the sequence itself; otherwise, the
    // subsequence is a copy of the original sequence structure.
    override fun subseq(start: Int?, end: Int?): LObject {
        if (this === Nil) {
            if (start == 0 && (end == 0 || end == null)) {
                return Nil
            }
            throw IndexError(
                "invalid indexes [$start, ${end ?: "nil"}) for empty list")
        }
        throw TypeError("not a sequence: $this")
    }

    // Return a reversed copy of the sequence. The sequence is not
    // altered.
    override fun reversed(): LObject {
        if (this === Nil) {
            return Nil
        }
        throw TypeError("not a sequence: $this")
    }

    // Return a copy of the sequence withe the specified item
    // deleted. The item is identified using equal().
    override fun delete(item: LObject): LObject {
        if (this === Nil) {
            return Nil
        }
        throw TypeError("not a sequence: $this")
    }

    // Find the item in the sequence for which the predicate is
    // true. Return a Pair of the item and its index in the
    // sequence. If no such item is found, return a Pair of Nil and
    // -1.
    override fun find(start: Int, end: Int?, last: Boolean,
             predicate: (LObject) -> Boolean): Pair<LObject, Int> {
        if (this === Nil) {
            return Pair(Nil, -1)
        }
        throw TypeError("not a sequence: $this")
    }

    // The length of the sequence, read only.
    override val length: Int
        get () {
            if (this === Nil) {
                return 0
            }
            return super.length
        }
}

fun intern(name: String, immutable_and_selfvalued: Boolean = false) =
    LSymbol.interned(name, immutable_and_selfvalued, immutable_and_selfvalued)

// A system r/o symbol is a variable that cannot be changed from Lisp.
fun systemROSymbol(name: String, value: LObject = Nil) =
    LSymbol.makeGlobal(name, value, true)
