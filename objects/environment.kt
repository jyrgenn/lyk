
// Environment, to bind values to symbols

package org.w21.lyk


class LEnv(val parent: LEnv? = null): LObject() {
    val level: Int = if (parent == null) 0 else parent.level + 1
    val levelstring = if (level > 0) "$level" else "root"
    val map: MutableMap<LSymbol, LObject> = mutableMapOf()

    // must only be called by symbol.bind
    fun bind(symbol: LSymbol, value: LObject) {
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
    
    fun getValueMaybe(symbol: LSymbol): LObject? {
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

    fun getValue(symbol: LSymbol): LObject {
        return getValueMaybe(symbol) ?:
            throw ValueError("variable `$symbol` is undefined")
    }

    fun setValue(symbol: LSymbol, value: LObject): Boolean {
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

    val descFormat = "#<${typeOf(this)}$id[$levelstring:%d]%s>"

    override fun toString() = descFormat.format(map.size, "")

    override fun desc() =
        descFormat.format(map.size,
                          if (level == 0) {
                              "{...}"
                          } else {
                              map.toString()
                          }
        )
    
}

fun withNewEnvironment(parent: LEnv = currentEnv,
                       closure: () -> LObject): LObject {
    return withEnvironment(LEnv(parent), closure)
}

fun withEnvironment(env: LEnv, closure: () -> LObject): LObject {
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
