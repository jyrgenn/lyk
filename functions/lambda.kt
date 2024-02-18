// the lambda function

package org.w21.lyk

val optionalPSym = intern("&optional")
val keyPSym = intern("&key")
val restPSym = intern("&rest")
val emptyString = makeString("")


class Lambda(
    functionName: Symbol?,                       // present if non anonymous
    stdPars: List<Symbol>,                   // normal parameters
    keyPars: Map<Symbol, LispObject>,        // &key name => default
    optPars: List<Pair<Symbol, LispObject>>, // &optional name, default
    restPar: Symbol?,                       // &rest parameters
    val bodyForms: LispObject,               //
    docBody: LispString,                     // docstring sans signature
    val environment: Environment
): Function(functionName, stdPars, keyPars, optPars, restPar, intern("value"),
            docBody, false)
{
    val lambdatype = "function"
    
    fun bindPars(arglist: LispObject) {
        if (arglist !is LispList) {
            throw CallError("Lambda $name called with invalid argaument"
                            +" list ($arglist)")
        }
        var argsi = ListIterator(arglist)
        for (param in stdPars) {
            if (argsi.hasNext()) {
                currentEnv.bind(param, argsi.next())
            } else {
                val atleast = if (minargs == maxargs) "" else "at least "
                throw ArgumentError("too few args for $lambdatype `$name`;"
                                    + " needs $atleast$minargs")
            }
        }
        for ((sym, defval) in optPars) {
            if (argsi.hasNext()) {
                currentEnv.bind(sym, argsi.next())
            } else {
                currentEnv.bind(sym, eval(defval))
            }
        }
        if (!argsi.hasNext()) {
            return
        }
        // keywords and rest
        var keyBound = mutableSetOf<LispObject>()
        var wantKeywordParam: Symbol? = null
        var restArgs = mutableListOf<LispObject>()
        for (arg in argsi) {
            // while ((arg = argsi.next()) != null) {
            if (wantKeywordParam != null) {
                currentEnv.bind(wantKeywordParam, arg)
                keyBound.add(wantKeywordParam)
                wantKeywordParam = null
            } else if (arg.isKeyword()) {
                val variable =  key2var(arg)
                if (variable in keyPars.keys) {
                    wantKeywordParam = variable
                } else {
                    throw ArgumentError("&key `$wantKeywordParam` not "
                                        + "valid for $lambdatype `$name`")
                }
            } else {
                restArgs.add(arg)
            }
        }
        if (wantKeywordParam != null) {
            throw ArgumentError("&key `$wantKeywordParam` argument missing "
                                + "calling $lambdatype `$name`")
        }
        for ((sym, defval) in keyPars) {
            if (sym !in keyBound) {
                currentEnv.bind(sym, defval)
            }
        }
        if (restPar != null) {
            currentEnv.bind(restPar, list2lisp(restArgs))
        } else {
            if (restArgs.count() > 0) {
                throw ArgumentError("too many args for $lambdatype `$name`")
            }
        }
    }

    override fun call(arglist: LispObject): LispObject {
        // print("$self.call($arglist)")
        return with_new_environment(environment) {
            bindPars(arglist)
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
                env: Environment,
                name: Symbol? = null): Lambda
{
    // sort params into the various params arrays
    var argptr = params
    val stdPars = mutableListOf<Symbol>()
    val optPars = mutableListOf<Pair<Symbol, LispObject>>()
    val keyPars = mutableMapOf<Symbol, LispObject>()
    var bodyForms: LispObject
    var docBody: LispString = emptyString
    var rest_sym: Symbol? = null
    val lambda_name = name?.toString() ?: "*anon-lambda*"

    // Parse an &optional parameter description. This will be either a
    // symbol for the parameter name, in which case the default value is
    // Nil, or a list of two elements, the name symbol and the default
    // value. Everything else will be flagged as an error. Return a tuple of
    // the name symbol and the default value.
    fun parse_2parlist(parameter: LispObject): Pair<Symbol, LispObject> {
        // print("parse_2parlist($parameter): ", terminator: "")
        if (parameter is Symbol) {
            // print("Symbol => $(sym, Nil)")
            return Pair(parameter, Nil)
        } else if (parameter is Cons) {
            val sym = parameter.car()
            if (sym !is Symbol) {
                throw LambdaDefError(
                    "&optional parameter name `$sym` not a symbol"
                    + "in $lambda_name definition")
            }
            if (parameter.cdr() === Nil) {
                // print("short list => $(sym, Nil)")
                return Pair(sym, Nil)
            }
            val elem2 = parameter.cdr()
            if (elem2 is Cons) {
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
    while (argptr is Cons) {
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
    if (body is Cons) {
        val maybeDoc = body.car()
        if (maybeDoc is LispString) {
            docBody = maybeDoc
            bodyForms = body.cdr()
        } else {
            bodyForms = body
        }
    } else {
        bodyForms = body
    }
    return Lambda(name, stdPars, keyPars, optPars, rest_sym,
                  bodyForms, docBody, env)
}
