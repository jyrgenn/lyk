// the Builtin function

package org.w21.lyk

fun maybeIntern(word: String?): LSymbol? {
    if (word == null) {
        return null
    }
    return intern(word)
}


class LBuiltin(
    // marked with S as kotlin String, not LispString, for clarity
    nameS: String,
    val bfun: (LObject, Map<LSymbol, LObject>, Map<LSymbol, Boolean>)
        -> LObject,
    stdParsS: Array<String>,
    keyParsS: Map<String, LObject>,
    optParsS: Array<Pair<String, LObject>>,
    restParsS: String?,
    retvalS: String?,
    isSpecial: Boolean = false,
    docBodyS: String,
    location: String,
): LFunction(
       intern(nameS),
       arrayIntern(stdParsS),
       mapInternKeys(keyParsS),
       pairsInternFirst(optParsS),
       maybeIntern(restParsS),
       maybeIntern(retvalS),
       isSpecial,
       makeString(docBodyS.trim()),
       makeString(location),
   ) {
    override val type = "builtin"

    override val typeDesc =
        if (isSpecial) "special form" else "builtin function"

    override fun call(arglist: LObject): LObject {
       // first establish the kwArgs[] with the default values
        var kwArgs = keyPars.toMutableMap()
        var wantStdArgs = stdPars.size
        var hadStdArgs = 0              // stdPars seen
        var wantOptArgs = optPars.size
        var hadOptArgs = 0              // optPars seen
        var hadArgs = 0                 // args seen at all

        var newArglist = ListCollector()
        var supplied_p = mutableMapOf<LSymbol, Boolean>()

        if (arglist !is LCons && arglist !== Nil) {
            throw CallError("$this called with improper arglist: $arglist")
        }

        var wantKeywordParam: LSymbol? = null  // i.e. have seen this keyword
        for (arg in arglist) {
            hadArgs++
            if (wantKeywordParam != null) {
                kwArgs[wantKeywordParam] = arg
                supplied_p[wantKeywordParam] = true
                wantKeywordParam = null
                continue
            }
            if (arg.isKeyword() && arg in kwArgs.keys) {
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
                supplied_p[optPars[hadOptArgs].first] = true
                hadOptArgs++
                continue
            }
            if (restPar != null) {
                newArglist.add(arg)
                continue
            }
            throw ArgumentError(
                "too many arguments for $typeDesc `$name`: $arglist")
        }
        // was it enough?
        if (hadStdArgs < wantStdArgs) {
            throw ArgumentError("too few args for $typeDesc `$name`: $arglist")
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
        callCounter++
        return with_called_function_name(name.name) {
            bfun(newArglist.list, kwArgs, supplied_p)
        }
    }

}
