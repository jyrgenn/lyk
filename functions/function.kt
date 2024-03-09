// what all functions have in common

package org.w21.lyk


val anonLambdaSym = Symbol.intern("*anon-lambda*")

abstract class Function(
    functionName: Symbol?,                       // present if non anonymous
    val stdPars: List<Symbol>,                   // normal parameters
    val keyPars: Map<Symbol, LispObject>,        // &key name => default
    val optPars: List<Pair<Symbol, LispObject>>, // &optional name, default
    val restPar: Symbol?,                       // &rest parameters
    val retval: Symbol?,                         // return value description
    val isSpecial: Boolean,                      // used by Builtins only
    val docBody: LispString,                     // docstring sans signature
): LispObject(), Callable {
    val name: Symbol
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
            (functionName as Symbol).function = this
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

    fun myKeywordArg(maybeSym: LispObject): Symbol? {
        // if sym is a keyword *and* in the keyPars, return the variable
        // symbol, used in both Builtins and Lambdas
        if (maybeSym is Symbol && maybeSym.isKeyword() &&
                maybeSym in keyPars.keys) {
            return Symbol.intern(maybeSym.name.substring(1))
        }
        return null
    }

    open override fun call(arglist: LispObject): LispObject {
        throw InternalError("calling $this, not Subclass")
    }

    open override fun dump(): String {
        return "#<${typeDesc()}[$id](${parlist()})= $retval>"
    }
}
