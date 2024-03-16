
// Environment, to bind values to symbols

package org.w21.lyk


class LEnv(val parent: LEnv? = null): LispObject() {
    val level: Int = if (parent == null) 0 else parent.level + 1
    val levelstring = if (level > 0) "$level" else "root"
    val map: MutableMap<LSymbol, LispObject> = mutableMapOf()

    // must only be called by symbol.bind
    fun bind(symbol: LSymbol, value: LispObject) {
	debug(debugBindSymSym) {
             "$symbol <= $value in $this"
        }
        map[symbol] = value
    }

    fun unbind(symbol: LSymbol) {
        if (symbol.immutable) {
            throw Exception("symbol $symbol is immutable")
        }
        var env: LEnv? = this
        while (env != null) {
	    debug(debugBindSymSym) {
                 "unbind $symbol in $this"
            }
            env.map.remove(symbol)
            env = env.parent
        }
    }
    
    fun getValueMaybe(symbol: LSymbol): LispObject? {
        var env: LEnv? = this
        while (env != null) {
            val maybe = env.map[symbol]
            if (maybe != null) {
                return maybe
            }
            env = env.parent
        }
        return null
    }

    fun getValue(symbol: LSymbol): LispObject {
        return getValueMaybe(symbol) ?:
            throw ValueError("value of symbol `$symbol` is undefined")
    }

    fun setValue(symbol: LSymbol, value: LispObject): Boolean {
        var env: LEnv? = this
        while (env != null) {
            if (symbol in env.map.keys) {
                env.map[symbol] = value
                return true
            }
            env = env.parent
        }
        rootEnv.map[symbol] = value
        return false
    }

    override fun toString() = "#<${typeOf(this)}$id[$level]>"

    override fun desc() = toString() + map.toString()
}

fun withNewEnvironment(parent: LEnv = currentEnv,
                       closure: () -> LispObject): LispObject {
    return withEnvironment(LEnv(parent), closure)
}

fun withEnvironment(env: LEnv, closure: () -> LispObject): LispObject {
    val savedEnv = currentEnv
    currentEnv = env
    try {
	debug(debugBindSymSym) {
             "enter $env from $savedEnv"
        }
        return closure()
    } finally {
	debug(debugBindSymSym) {
             "leave $env, back to $savedEnv"
        }
        currentEnv = savedEnv
    }
}
