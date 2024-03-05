// the Builtin function

package org.w21.lyk

fun maybeIntern(word: String?): Symbol? {
    if (word == null) {
        return null
    }
    return Symbol.intern(word)
}


class Builtin(
    name: String,
    val bfun: (LispObject, Map<Symbol, LispObject>) -> LispObject,
    stdPars: Array<String>,
    keyPars: Map<String, LispObject>,
    optPars: Array<Pair<String, LispObject>>,
    restPars: String?,
    retval: String?,
    isSpecial: Boolean = false,
    docBody: String,
): Function(
       Symbol.intern(name),
       arrayIntern(stdPars),
       mapInternKeys(keyPars),
       pairsInternFirst(optPars),
       maybeIntern(restPars),
       maybeIntern(retval),
       isSpecial,
       LispString.makeString(docBody),
   ) {

    override fun call(arglist: LispObject): LispObject {
       // first establish the key_args[] with the default values
        var key_args = keyPars.toMutableMap()

        var nargs = 0                   // # of normal args seen
        var larglist = arglist          // local arglist; we may need to assign
                                        // a new one
        var argptr = larglist            // iterate through larglist
        var lastargpair: Cons? = null   // keep track 

        if (argptr !is Cons && argptr !== Nil) {
            throw CallError("$this called with improper arglist: $larglist")
        }

        // Walk along the larglist to check for presence of all the stdPars
        for (p in stdPars) {
            if (argptr !is Cons) {
                val atleast = if (minargs == maxargs) "" else "at least "
                throw ArgumentError("too few args for function `$name`; have "
                                    + "$nargs, needs $atleast$minargs")
                
            }
            lastargpair = argptr
            argptr = argptr.cdr()
            nargs += 1
        }
        // Further, append defaults for &optional params if necessary
        for (op in optPars) {
            if (argptr is Cons) {
                lastargpair = argptr
                argptr = argptr.cdr()
            } else {
                val newpair = Cons(op.second, Nil)
                if (lastargpair != null) {
                    lastargpair.rplacd(newpair)
                } else {                // larglist was Nil to begin with
                    larglist = newpair
                }
                lastargpair = newpair
            }
            nargs += 1
        }

        // any more present? must be keywords and rest; so first detach them
        // from the actual arg list
        if (argptr is Cons && lastargpair != null) {
            lastargpair.rplacd(Nil)
        }

        var wantKeywordParam: Symbol? = null  // i.e. have seen this keyword
        while (argptr is Cons) {
            val arg = argptr.car()
            if (wantKeywordParam != null) {
                key_args[wantKeywordParam] = arg
                wantKeywordParam = null
                argptr = argptr.cdr()
            } else if (arg is Symbol
                           && arg.isKeyword()
                           && key2var(arg) in keyPars.keys) {
                wantKeywordParam = key2var(arg)
                argptr = argptr.cdr()
            } else if (restPar != null) {
                if (lastargpair != null) {
                    lastargpair.rplacd(argptr)
                } else {
                    // had no lastargpair to append to, so the whole
                    // larglist must have been nil
                    larglist = argptr
                }
                lastargpair = argptr
                argptr = argptr.cdr()
                lastargpair.rplacd(Nil)
            } else {
                val atmost = if (minargs == maxargs) "" else "at most "
                throw ArgumentError("too many args for function `$name`;"
                                      + " have $nargs, takes $atmost"
                                      + "$maxargs")
            }
        }
        if (argptr !== Nil) {
            throw CallError("$this called with improper arglist: $arglist")
        }
        if (wantKeywordParam != null) {
            throw ArgumentError("&key `:$wantKeywordParam` argument missing "
                                + "calling builtin `$name`")
        }
        // finally, call the actual function
        return bfun(larglist, key_args)
    }

    override fun typeDesc() = if (isSpecial) "Special form" else "Builtin"
}
