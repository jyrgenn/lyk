// what all functions have in common

package org.w21.lyk


val anonLambdaSym = intern("*anon-lambda*")

abstract class Function(
    functionName: LSymbol?,                       // present if non anonymous
    val stdPars: List<LSymbol>,                   // normal parameters
    val keyPars: Map<LSymbol, LispObject>,        // &key name => default
    val optPars: List<Pair<LSymbol, LispObject>>, // &optional name, default
    val restPar: LSymbol?,                       // &rest parameters
    val retval: LSymbol?,                         // return value description
    val isSpecial: Boolean,                      // used by Builtins only
    val docBody: LString,                     // docstring sans signature
): LispObject(), Callable {
    val name: LSymbol
    val has_name: Boolean
    val minargs: Int
    val maxargs: Int

    init {
        has_name = functionName != null
        name = functionName ?: anonLambdaSym
        minargs = stdPars.size
        maxargs = if (restPar == null)
            minargs + optPars.size + keyPars.size * 2 // ":key keyarg"
        else
            -1
        if (has_name) {
            (functionName as LSymbol).function = this
        }
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
        if (restPar != null) {
            sb.add("&rest")
            sb.add(restPar.name)
        }
        return sb.join(" ")
    }

    override open fun toString() = "#<${typeDesc()}[$id]$name>"

    override open fun desc() = "#<${typeDesc()}[$id](${parlist()})=$retval>"

    open fun typeDesc(): String {
        return typeOf(this)
    }

    fun docHeader(): String {
        return "${typeDesc()} (${parlist()}) => $retval"
    }

    fun documentation(): String {
        if (docBody.value == "") {
            return docHeader() + "\n"
        }
        return docHeader() + docBody.value + "\n"        
    }

    fun myKeywordArg(maybeSym: LispObject): LSymbol? {
        // if sym is a keyword *and* in the keyPars, return the variable
        // symbol, used in both Builtins and Lambdas
        if (maybeSym is LSymbol && maybeSym.isKeyword() &&
                maybeSym in keyPars.keys) {
            return intern(maybeSym.name.substring(1))
        }
        return null
    }

    open override fun call(arglist: LispObject): LispObject {
        throw InternalError("calling $this, not Subclass")
    }

    open override fun dump() = desc()
}
