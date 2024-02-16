// what all functions have in common

package org.w21.lyk


val anonLambdaSym = intern("*anon-lambda*")

abstract class Function(
    functionName: Symbol?,                       // present if non anonymous
    val stdPars: List<Symbol>,                   // normal parameters
    val keyPars: Map<Symbol, LispObject>,        // &key name => default
    val optPars: List<Pair<Symbol, LispObject>>, // &optional name, default
    val restPars: Symbol?,                       // &rest parameters
    val retval: Symbol?,                         // return value description
    val docBody: LispString,                     // docstring sans signature
    val isSpecial: Boolean,                      // used by Builtins only
): LispObject(), Callable {
    val name: Symbol
    val has_name: Boolean
    val minargs: Int
    val maxargs: Int

    init {
        has_name = functionName != null
        name = functionName ?: anonLambdaSym
        minargs = stdPars.size
        maxargs = if (restPars == null)
            minargs + optPars.size + keyPars.size * 2 // ":key keyarg"
        else
            -1
    }

    fun parlist(): String {
        val sb = StrBuf(name.name)
        for (par in stdPars) {
            sb.add(par.name)
        }
        if (optPars.size > 0) {
            sb.add("&optional")
            for ((pname, defval) in optPars) {
                if (defval === Nil) {
                    sb.add(pname.name)
                } else {
                    sb.add("(${pname.name} ${defval.desc()})")
                }
            }
        }
        if (keyPars.size > 0) {
            sb.add("&key")
            for ((pname, defval) in keyPars) {
                if (defval === Nil) {
                    sb.add(pname.name)
                } else {
                    sb.add("(${pname.name} ${defval.desc()})")
                }
            }
        }
        if (restPars != null) {
            sb.add("&rest")
            sb.add(restPars.name)
        }
        return sb.join(" ")
    }

    fun typeDesc(): String {
        // for the documentation
        if (this is Builtin) {
            if (isSpecial) {
                return "Special form"
            }
            return "Builtin function"
        }
        return typeOf(this)
    }

    fun docHeader(): String {
        return "#<${typeDesc()} (${parlist()})>"
    }

    fun documentation(): String {
        if (docBody.value == "") {
            return docHeader() + "\n"
        }
        return docHeader() + "\n" + docBody.value + "\n"        
    }

    fun key2var(maybeSym: LispObject): Symbol? {
        // if sym is a keyword *and* in the keyPars, return the variable
        // symbol, used in both Builtings and Lambdas
        if (maybeSym is Symbol && maybeSym.isKeyword()) {
            val varsym = intern(maybeSym.name.substring(startIndex = 1))
            if (varsym in keyPars.keys) {
                return varsym
            }
        }
        return null
    }

    open override fun call(arglist: LispObject): LispObject {
        throw InternalError("calling $this, not Subclass")
    }
}
