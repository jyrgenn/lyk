
// Environment, to bind values to symbols

package org.w21.lyk


class LEnv(val parent: LEnv? = null): LObject() {
    val level: Int = if (parent == null) 0 else parent.level + 1
    val levelstring = if (level > 0) "$level" else "root"
    val the_env: MutableMap<LSymbol, LObject> = mutableMapOf()

    override val type = "environment"

    // must only be called by symbol.bind
    fun bind(symbol: LSymbol, value: LObject) {
	debug(debugBindSymSym) {
             "$symbol <= $value in $this"
        }
        the_env[symbol] = value
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
            env.the_env.remove(symbol)
            env = env.parent
        }
    }
    
    fun getValueMaybe(symbol: LSymbol): LObject? {
        var env: LEnv? = this
        while (env != null) {
            val maybe = env.the_env[symbol]
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
            if (symbol in env.the_env.keys) {
                env.the_env[symbol] = value
                return true
            }
            env = env.parent
        }
        rootEnv.the_env[symbol] = value
        return false
    }

    val descFormat = "#<${this.type}$id[$levelstring:%d]%s>"

    override fun toString() = desc(null) //descFormat.format(the_env.size, "")

    override fun desc(seen: MutableSet<LObject>?): String {
        val seen_set =
            if (seen == null) {
                mutableSetOf<LObject>()
            } else {
                seen
            }
        seen_set.add(this)
        return descFormat.format(the_env.size,
                          if (level == 0) {
                              "{...}"
                          } else {
                              val l = mutableListOf<String>()
                              for ((key, value) in the_env) {
                                  l.add("$key=${value.desc(seen_set)}")
                              }
                              "{" + l.joinToString(", ") + "}"
                          }
        )
    }
    
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
