
// Environment, to bind values to symbols

package org.w21.lyk

val bindSymSym = Symbol.intern("bind")

class Environment(val parent: Environment? = null): LispObject() {
    val level: Int = if (parent == null) 0 else parent.level + 1
    val levelstring = if (level > 0) "$level" else "root"
    val map: MutableMap<Symbol, LispObject> = mutableMapOf()

    // must only be called by symbol.bind
    fun bind(symbol: Symbol, value: LispObject) {
	debug(bindSymSym) {
             "bind $symbol = $value in $this"
        }
        map[symbol] = value
    }

    fun unbind(symbol: Symbol) {
        if (symbol.immutable) {
            throw Exception("symbol $symbol is immutable")
        }
        var env: Environment? = this
        while (env != null) {
	    debug(bindSymSym) {
                 "unbind $symbol in $this"
            }
            env.map.remove(symbol)
            env = env.parent
        }
    }
    
    fun getValueMaybe(symbol: Symbol): LispObject? {
        var env: Environment? = this
        while (env != null) {
            val maybe = env.map[symbol]
            if (maybe != null) {
                return maybe
            }
            env = env.parent
        }
        return null
    }

    fun getValue(symbol: Symbol): LispObject {
        return getValueMaybe(symbol) ?:
            throw ValueError("value of symbol `$symbol` is undefined")
    }

    fun setValue(symbol: Symbol, value: LispObject): Boolean {
        var env: Environment? = this
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
}

fun with_new_environment(parent: Environment = currentEnv,
                         closure: () -> LispObject): LispObject {
    val savedEnv = currentEnv
    currentEnv = Environment(parent)
    try {
        return closure()
    } finally {
        currentEnv = savedEnv
    }
}

fun with_environment(env: Environment, closure: () -> LispObject): LispObject {
    val savedEnv = currentEnv
    currentEnv = env
    try {
	debug(bindSymSym) {
             "enter $env from $savedEnv"
        }
        return closure()
    } finally {
	debug(bindSymSym) {
             "leave $env, back to $savedEnv"
        }
        currentEnv = savedEnv
    }
}
