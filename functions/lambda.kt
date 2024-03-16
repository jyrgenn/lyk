// the lambda function

package org.w21.lyk

val optionalPSym = intern("&optional")
val keyPSym = intern("&key")
val restPSym = intern("&rest")
val emptyString = makeString("")


open class Lambda(                           // Macro will inherit this
    functionName: LSymbol?,                   // present if non anonymous
    stdPars: List<LSymbol>,                   // normal parameters
    keyPars: Map<LSymbol, LispObject>,        // &key name => default
    optPars: List<Pair<LSymbol, LispObject>>, // &optional name, default
    restPar: LSymbol?,                       // &rest parameters
    val bodyForms: LispObject,               //
    docBody: LString,                     // docstring sans signature
    val environment: LEnv,
    isSpecial: Boolean = false          // for Macros only
): Function(functionName, stdPars, keyPars, optPars, restPar,
            intern("value"), isSpecial, docBody)
{
    val lambdatype = "function"
    
    override fun desc(): String {
        var body: String

        if (bodyForms === Nil) {
            body = Nil.toString()
        } else {
            val s = bodyForms.toString()
            body = s.substring(1, s.length - 1)
        }
        
        return "#<${typeDesc()}[$id](${parlist()}) $body>"
    }


    fun bindPars(arglist: LispObject) {
       // first establish the kwArgs[] with the default values
        var kwArgs = keyPars.toMutableMap()
        var wantStdArgs = stdPars.size
        var hadStdArgs = 0              // stdPars seen
        var wantOptArgs = optPars.size
        var hadOptArgs = 0              // optPars seen
        var hadArgs = 0                 // args seen at all

        var restArgs = ListCollector()

        if (arglist !is LCons && arglist !== Nil) {
            throw CallError("$this called with improper arglist: $arglist")
        }

        var wantKeywordParam: LSymbol? = null  // i.e. have seen this keyword
        for (arg in arglist) {
            hadArgs++
            if (wantKeywordParam != null) {
                // these will be bound later together with the default values
                // for keys we did not see
                kwArgs[wantKeywordParam] = arg
                wantKeywordParam = null
                continue
            }
            if (arg.isKeyword()) {
                val sym = key2var(arg as LSymbol)
                if (sym !in kwArgs.keys) {
                    throw ArgumentError("keyword $arg invalid"
                                        + " for function `${this.name}'")
                }
                wantKeywordParam = sym
                continue
            }
            if (hadStdArgs < wantStdArgs) {
                currentEnv.bind(stdPars[hadStdArgs], arg)
                hadStdArgs++
                continue
            }
            if (hadOptArgs < wantOptArgs) {
                currentEnv.bind(optPars[hadOptArgs].first, arg)
                hadOptArgs++
                continue
            }
            if (restPar != null) {
                restArgs.add(arg)
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
            currentEnv.bind(optPars[hadOptArgs].first,
                            optPars[hadOptArgs++].second)
        }
        if (restPar != null) {
            currentEnv.bind(restPar, restArgs.list())
        }
        // bind keyword arguments
        for ((symbol, value) in kwArgs) {
            currentEnv.bind(symbol, value)
        }
    }

    override fun call(arglist: LispObject): LispObject {
        return withNewEnvironment(environment) {
            bindPars(arglist)
            debug(debugLambdaParamsSym) {
                currentEnv.desc() + "\nin " + bodyForms.toString()
            }
            evalProgn(bodyForms)
        }
    }

}


// states of the parameter list reader
enum class PLS {
    in_std,                         // phase 1, 
    in_rest,                        // saw &rest, expect rest param
    exp_opt,                        // after &optional, expect opt param
    in_opt,                         // had opt params, expect more or not
    exp_key,                        // expect key param
    in_key,                         // had key params, expect more or not
    finish,                         // done, but maybe &key
}
// "expecting ..."
val PLS_desc = arrayOf(
    "standard parameter",
    "&rest parameter",
    "&optional parameter",
    "&optional parameter",
    "&key parameter",
    "&key parameter",
    "end of parameters or &key"
)

enum class TC {
    opt_sym,                        // &optional
    key_sym,                        // &key
    rest_sym,                       // &rest
    other,                          // other, i.e. symbol or list
}

enum class Ac {
    st_std,                         // store in stdPars
    st_key,                         // store in keyPars
    st_opt,                         // store in optPars
    rstsym,                         // store &rest symbol
    none,                           // do nothing for now
    error,                          // raise error
}

// state transition table, by state (vertical) and tokenClass
val new_state = arrayOf(      // [PLS, TC] => PLS
    //opt_sym  key_sym   rest_sym  other
    arrayOf(PLS.in_opt, PLS.exp_key, PLS.in_rest, PLS.in_std), // in_std
    arrayOf(PLS.finish, PLS.finish,  PLS.finish,  PLS.finish), // in_rest
    arrayOf(PLS.finish, PLS.in_key,  PLS.finish,  PLS.in_opt), // exp_opt
    arrayOf(PLS.finish, PLS.in_key,  PLS.in_rest, PLS.in_opt), // in_opt
    arrayOf(PLS.finish, PLS.finish,  PLS.finish,  PLS.in_key), // exp_key
    arrayOf(PLS.in_opt, PLS.finish,  PLS.in_rest, PLS.in_key), // in_key
    arrayOf(PLS.finish, PLS.exp_key, PLS.in_rest, PLS.finish)  // finish
)

// action depending on state and token type
val action_table = arrayOf( // [PLS, TC] => Ac
    //opt_sym  key_sym  rest_sym  other
    arrayOf(Ac.none,  Ac.none,  Ac.none,  Ac.st_std), // in_std
    arrayOf(Ac.error, Ac.error, Ac.error, Ac.rstsym), // in_rest
    arrayOf(Ac.error, Ac.error, Ac.error, Ac.st_opt), // exp_opt
    arrayOf(Ac.error, Ac.none,  Ac.none,  Ac.st_opt), // in_opt
    arrayOf(Ac.error, Ac.error, Ac.error, Ac.st_key), // exp_key
    arrayOf(Ac.none,  Ac.error, Ac.none,  Ac.st_key), // in_key
    arrayOf(Ac.none,  Ac.none,  Ac.error, Ac.error)   // finish
)


fun makeLambda(params: LispObject,
               body: LispObject,
               env: LEnv = currentEnv,
               name: LSymbol? = null,
               isMacro: Boolean = false): Lambda
{
    // sort params into the various params arrays
    var argptr = params
    val stdPars = mutableListOf<LSymbol>()
    val optPars = mutableListOf<Pair<LSymbol, LispObject>>()
    val keyPars = mutableMapOf<LSymbol, LispObject>()
    var bodyForms: LispObject
    var docBody: LString = emptyString
    var rest_sym: LSymbol? = null
    val lambda_name = name?.toString() ?: "*anon-lambda*"

    // Parse an &optional parameter description. This will be either a
    // symbol for the parameter name, in which case the default value is
    // Nil, or a list of two elements, the name symbol and the default
    // value. Everything else will be flagged as an error. Return a tuple of
    // the name symbol and the default value.
    fun parse_2parlist(parameter: LispObject): Pair<LSymbol, LispObject> {
        // print("parse_2parlist($parameter): ", terminator: "")
        if (parameter is LSymbol) {
            // print("LSymbol => $(sym, Nil)")
            return Pair(parameter, Nil)
        } else if (parameter is LCons) {
            val sym = parameter.car()
            if (sym !is LSymbol) {
                throw LambdaDefError(
                    "&optional parameter name `$sym` not a symbol"
                    + "in $lambda_name definition")
            }
            if (parameter.cdr() === Nil) {
                // print("short list => $(sym, Nil)")
                return Pair(sym, Nil)
            }
            val elem2 = parameter.cdr()
            if (elem2 is LCons) {
                if (elem2.cdr() != Nil) {
                    throw LambdaDefError(
                        "invalid list in &optional parameter spec for "
                        + "`$sym` in $lambda_name definition")
                }
                // print("list of 2 => $(sym, Nil)")
                return Pair(sym, elem2.car())
            } else {
                throw LambdaDefError(
                    "improper list in &optional parameter spec for `$sym`"
                    + " in $lambda_name definition")
            } 
        }
        throw LambdaDefError("invalid &optional parameter spec "
                             + " in $lambda_name definition")
    }
    
    fun tokenClass(ob: LispObject): TC {
        when (ob) {
            optionalPSym -> return TC.opt_sym
            keyPSym ->      return TC.key_sym
            restPSym ->     return TC.rest_sym
            else ->         return TC.other
        }
    }

    var state = PLS.in_std
    while (argptr is LCons) {
        val elem = argptr.car()
        val tclass = tokenClass(elem)
        val action = action_table[state.ordinal][tclass.ordinal]
        // print("state $state, seen $elem, tclass $tclass, action $action")

        when (action) {
            Ac.st_std -> {
                stdPars.add(symbolArg(elem, "lamba parameter"))
            }
            Ac.st_key -> {
                val (key, defval) = parse_2parlist(elem)
                // print("&key param $key, defval $defval")
                keyPars[key] = defval
            }
            Ac.st_opt -> {
                optPars.add(parse_2parlist(elem))
            }
            Ac.rstsym -> {
                if (rest_sym !== null) {
                    throw LambdaDefError("unexpected &rest parameter "
                                         + "(already stored one), "
                                         + "$lambda_name")
                }
                rest_sym = symbolArg(elem, "&key parameter")
            }
            Ac.none -> {}
            Ac.error -> {
                throw LambdaDefError("unexpected token `$elem` while expec"
                                     + "ting ${PLS_desc[state.ordinal]}, "
                                     + "$lambda_name")
            }
        }
        state = new_state[state.ordinal][tclass.ordinal]
        // print("new state $state")
        argptr = argptr.cdr()
    }
    if (argptr !== Nil) {
        throw LambdaDefError("parameter list of $lambda_name is not a "
                             + "proper list: $params")            
    }
    if (body is LCons) {
        val maybeDoc = body.car()
        if (maybeDoc is LString) {
            docBody = maybeDoc
            bodyForms = body.cdr()
        } else {
            bodyForms = body
        }
    } else {
        bodyForms = body
    }
    if (isMacro) {
        return Macro(name, stdPars, keyPars, optPars, rest_sym,
                     bodyForms, docBody)
    } else {
        return Lambda(name, stdPars, keyPars, optPars, rest_sym,
                      bodyForms, docBody, env)
    }
}
