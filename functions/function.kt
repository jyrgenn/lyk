// what all functions have in common

package org.w21.lyk


val anonLambdaSym = intern("*anon-lambda*")

abstract class LFunction(
    functionName: LSymbol?,                    // present if non anonymous
    val stdPars: List<LSymbol>,                // normal parameters
    val keyPars: Map<LSymbol, LObject>,        // &key name => default
    val optPars: List<Pair<LSymbol, LObject>>, // &optional name, default
    val restPar: LSymbol?,                     // &rest parameters
    val retval: LSymbol?,                      // return value description
    val isSpecial: Boolean,                    // used by Builtins only
    val docBody: LString,                      // docstring sans signature
): LObject() {
    val name: LSymbol
    val has_name: Boolean
    abstract val typeDesc: String

    init {
        has_name = functionName != null
        name = functionName ?: anonLambdaSym
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

    override open fun toString() = "#<${typeDesc}[$id]$name>"

    override open fun desc() = "#<${typeDesc}[$id](${parlist()})=$retval>"

    fun docHeader(): String {
        return "${typeDesc} (${parlist()}) => $retval"
    }

    fun documentation(): String {
        if (docBody.value == "") {
            return docHeader() + "\n"
        }
        return docHeader() + "\n" + docBody.value + "\n"        
    }

    fun myKeywordArg(maybeSym: LObject): LSymbol? {
        // if sym is a keyword *and* in the keyPars, return the variable
        // symbol, used in both Builtins and Lambdas
        if (maybeSym is LSymbol && maybeSym.isKeyword() &&
                maybeSym in keyPars.keys) {
            return intern(maybeSym.name.substring(1))
        }
        return null
    }

    open fun call(arglist: LObject): LObject {
        throw InternalError("calling $this in LFunction, not Subclass!")
    }

    open override fun dump() = desc()

    open fun body() = Nil as LObject
}

// bind function call or macro expansion arguments to the right variables; with
// complex arglists, this is a complex task
fun bindPars(arglist: LObject, func: LFunction) {
    // first establish the kwArgs[] with the default values
    var kwArgs = func.keyPars.toMutableMap()
    var wantStdArgs = func.stdPars.size
    var hadStdArgs = 0              // stdPars seen
    var wantOptArgs = func.optPars.size
    var hadOptArgs = 0              // optPars seen

    var restArgs = ListCollector()

    if (arglist !is LCons && arglist !== Nil) {
        throw CallError("$func called with improper arglist: $arglist")
    }

    var wantKeyArg: LSymbol? = null  // i.e. have seen this keyword
    for (arg in arglist) {
        if (wantKeyArg != null) {
            // these will be bound later together with the default values
            // for keys we did not see
            kwArgs[wantKeyArg] = arg
            wantKeyArg = null
            continue
        }
        if (arg.isKeyword()) {
            val sym = key2var(arg as LSymbol)
            if (sym !in kwArgs.keys) {
                throw ArgumentError("keyword $arg invalid"
                                    + " for ${func.typeDesc} `${func.name}'")
            }
            wantKeyArg = sym
            continue
        }
        if (hadStdArgs < wantStdArgs) {
            currentEnv.bind(func.stdPars[hadStdArgs], arg)
            hadStdArgs++
            continue
        }
        if (hadOptArgs < wantOptArgs) {
            currentEnv.bind(func.optPars[hadOptArgs].first, arg)
            hadOptArgs++
            continue
        }
        if (func.restPar != null) {
            restArgs.add(arg)
            continue
        }
        // don't want this any more
        throw ArgumentError(
            "too many arguments for ${func.typeDesc} `${func.name}`: $arglist")
    }
    // was it enough?
    if (hadStdArgs < wantStdArgs) {
        throw ArgumentError(
            "too few arguments for function `${func.name}`: $arglist")
    }
    // a :keyword left dangling?
    if (wantKeyArg != null) {
        throw ArgumentError("&key `$wantKeyArg` argument missing "
                            + "for ${func.typeDesc} `${func.name}`: $arglist")
    }

    // fill in optArgs with default values if necessary
    while (hadOptArgs < wantOptArgs) {
        currentEnv.bind(func.optPars[hadOptArgs].first,
                        func.optPars[hadOptArgs++].second)
    }
    if (func.restPar != null) {
        currentEnv.bind(func.restPar, restArgs.list)
    }
    // bind keyword arguments
    for ((symbol, value) in kwArgs) {
        currentEnv.bind(symbol, value)
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


fun makeLambdaOrMacro(params: LObject,
                      body: LObject,
                      env: LEnv = currentEnv,
                      name: LSymbol? = null,
                      isMacro: Boolean = false): LFunction
{
    // sort params into the various params arrays
    var argptr = params
    val stdPars = mutableListOf<LSymbol>()
    val optPars = mutableListOf<Pair<LSymbol, LObject>>()
    val keyPars = mutableMapOf<LSymbol, LObject>()
    var bodyForms: LObject
    var docBody: LString = emptyString
    var rest_sym: LSymbol? = null
    val lambda_name = name?.toString() ?: "*anon-lambda*"

    // Parse an &optional parameter description. This will be either a
    // symbol for the parameter name, in which case the default value is
    // Nil, or a list of two elements, the name symbol and the default
    // value. Everything else will be flagged as an error. Return a tuple of
    // the name symbol and the default value.
    fun parse_2parlist(parameter: LObject): Pair<LSymbol, LObject> {
        // print("parse_2parlist($parameter): ", terminator: "")
        if (parameter is LSymbol) {
            // print("LSymbol => $(sym, Nil)")
            return Pair(parameter, Nil)
        } else if (parameter is LCons) {
            val sym = parameter.car
            if (sym !is LSymbol) {
                throw LambdaDefError(
                    "&optional parameter name `$sym` not a symbol"
                    + "in $lambda_name definition")
            }
            if (parameter.cdr === Nil) {
                // print("short list => $(sym, Nil)")
                return Pair(sym, Nil)
            }
            val elem2 = parameter.cdr
            if (elem2 is LCons) {
                if (elem2.cdr != Nil) {
                    throw LambdaDefError(
                        "invalid list in &optional parameter spec for "
                        + "`$sym` in $lambda_name definition")
                }
                // print("list of 2 => $(sym, Nil)")
                return Pair(sym, elem2.car)
            } else {
                throw LambdaDefError(
                    "improper list in &optional parameter spec for `$sym`"
                    + " in $lambda_name definition")
            } 
        }
        throw LambdaDefError("invalid &optional parameter spec "
                             + " in $lambda_name definition")
    }
    
    fun tokenClass(ob: LObject): TC {
        when (ob) {
            optionalPSym -> return TC.opt_sym
            keyPSym ->      return TC.key_sym
            restPSym ->     return TC.rest_sym
            else ->         return TC.other
        }
    }

    var state = PLS.in_std
    while (argptr is LCons) {
        val elem = argptr.car
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
        argptr = argptr.cdr
    }
    if (argptr !== Nil) {
        throw LambdaDefError("parameter list of $lambda_name is not a "
                             + "proper list: $params")            
    }
    if (body is LCons) {
        val maybeDoc = body.car
        if (maybeDoc is LString) {
            docBody = maybeDoc
            bodyForms = body.cdr
        } else {
            bodyForms = body
        }
    } else {
        bodyForms = body
    }
    if (isMacro) {
        return LMacro(name, stdPars, keyPars, optPars, rest_sym,
                     bodyForms, docBody)
    } else {
        return Lambda(name, stdPars, keyPars, optPars, rest_sym,
                      bodyForms, docBody, env)
    }
}
