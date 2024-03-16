// the Builtin function

package org.w21.lyk

fun maybeIntern(word: String?): LSymbol? {
    if (word == null) {
        return null
    }
    return LSymbol.intern(word)
}


class Builtin(
    // marked with S as kotlin String, not LispString, for clarity
    nameS: String,
    val bfun: (LispObject, Map<LSymbol, LispObject>) -> LispObject,
    stdParsS: Array<String>,
    keyParsS: Map<String, LispObject>,
    optParsS: Array<Pair<String, LispObject>>,
    restParsS: String?,
    retvalS: String?,
    isSpecial: Boolean = false,
    docBodyS: String,
): Function(
       LSymbol.intern(nameS),
       arrayIntern(stdParsS),
       mapInternKeys(keyParsS),
       pairsInternFirst(optParsS),
       maybeIntern(restParsS),
       maybeIntern(retvalS),
       isSpecial,
       LispString.makeString(docBodyS),
   ) {

    override fun call(arglist: LispObject): LispObject {
       // first establish the kwArgs[] with the default values
        var kwArgs = keyPars.toMutableMap()
        var wantStdArgs = stdPars.size
        var hadStdArgs = 0              // stdPars seen
        var wantOptArgs = optPars.size
        var hadOptArgs = 0              // optPars seen
        var hadArgs = 0                 // args seen at all

        var newArglist = ListCollector()

        if (arglist !is Cons && arglist !== Nil) {
            throw CallError("$this called with improper arglist: $arglist")
        }

        var wantKeywordParam: LSymbol? = null  // i.e. have seen this keyword
        for (arg in arglist) {
            hadArgs++
            if (wantKeywordParam != null) {
                kwArgs[wantKeywordParam] = arg
                wantKeywordParam = null
                continue
            }
            if (arg.isKeyword()) {
                if (arg !in kwArgs.keys) {
                    throw ArgumentError("keyword $arg invalid"
                                        + " for function `${this.name}'")
                }
                wantKeywordParam = arg as LSymbol
                continue
            }
            if (hadStdArgs < wantStdArgs) {
                newArglist.add(arg)
                hadStdArgs++
                continue
            }
            if (hadOptArgs < wantOptArgs) {
                newArglist.add(arg)
                hadOptArgs++
                continue
            }
            if (restPar != null) {
                newArglist.add(arg)
            }
        }
        // was it enough?
        if (hadStdArgs < wantStdArgs) {
            val atleast = if (minargs == maxargs) "" else "at least "
            throw ArgumentError("too few args for function `$name`; have "
                                + "$hadArgs, needs $atleast$minargs")
        }
        // and not too much?
        if (maxargs >= 0 && hadArgs > maxargs) {
            val atmost = if (minargs == maxargs) "" else "at most "
            throw ArgumentError("too many args for function `$name`;"
                                + " have $hadArgs, takes $atmost"
                                + "$maxargs")
        }
        // a :keyword left dangling?
        if (wantKeywordParam != null) {
            throw ArgumentError("&key `:$wantKeywordParam` argument missing "
                                + "calling builtin `$name`")
        }

        // fill in optArgs with default values if necessary
        while (hadOptArgs < wantOptArgs) {
            newArglist.add(optPars[hadOptArgs++].second)
        }
        // finally, call the actual function
        return bfun(newArglist.list(), kwArgs)
    }

    override fun typeDesc() = if (isSpecial) "Special form" else "Builtin"
}
