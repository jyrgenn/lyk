// the lambda function

package org.w21.lyk

val optionalPSym = intern("&optional")
val keyPSym = intern("&key")
val restPSym = intern("&rest")
val emptyString = makeString("")


class Lambda: Function {
    val bodyforms: LispObject
    val restPars_sym: Symbol?
    val env: Environment

    val lambdatype = "function"

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
    val PLS_desc = [
        "standard parameter",
        "&rest parameter",
        "&optional parameter",
        "&optional parameter",
        "&key parameter",
        "&key parameter",
        "end of parameters or &key"
    ]

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


    constructor(params: LispObject,
                val bodyforms: LispObject,
                val env: Environment,
                val name: Symbol? = null) {

        
        // sort params into the various params arrays
        var argptr = params
        var stdPars = mutableListOf<Symbol>()
        var optPars = mutableListOf<Pair<Symbol, LispObject>>()
        var keyPars = mutableMapOf<Symbol, LispObject>()
        var docstring: LispString = emptyString
        var rest_sym: Symbol? = null

        val lambda_name = name?.toString() ?: "*anon-lambda*"

        // Parse an &optional parameter description. This will be either a
        // symbol for the parameter name, in which case the default value is
        // Nil, or a list of two elements, the name symbol and the default
        // value. Everything else will be flagged as an error. Return a tuple of
        // the name symbol and the default value.
        fun parse_2parlist(parameter: LispObject): Pair<Symbol, LispObject> {
            // print("parse_2parlist(\(parameter)): ", terminator: "")
            if (parameter is Symbol) {
                // print("Symbol => \((sym, Nil))")
                return Pair(parameter, Nil)
            } else if (parameter is Pair) {
                val sym = parameter.car()
                if (sym !is Symbol) {
                    throw LambdaDefError(
                        "&optional parameter name `$sym` not a symbol"
                        + "in $lambda_name definition")
                }
                if (parameter.cdr() === Nil) {
                    // print("short list => \((sym, Nil))")
                    return Pair(sym, Nil)
                }
                val elem2 = parameter.cdr()
                if (elem2 != Cons) {
                    throw LambdaDefError(
                        "improper list in &optional parameter spec for `$sym`"
                        + " in $lambda_name definition")
                }
                if (elem2.cdr() != Nil) {
                    throw LambdaDefError(
                        "invalid list in &optional parameter spec for `$sym`"
                        + " in $lambda_name definition")
                }
                // print("list of 2 => \((sym, Nil))")
                return Pair(sym, pair2.car())
            } else {
                throw LambdaDefError("invalid &optional parameter spec "
                                     + " in $lambda_name definition")
            }
        }
        
        fun tokenClass(ob: LispObject): TC {
            when (ob) {
                optionalPSym -> return .opt_sym
                keyPSym ->      return .key_sym
                restPSym ->     return .rest_sym
                else ->         return .other
            }
        }

        var state = PLS.in_std
        while (argptr is Cons) {
            val elem = argptr.car()
            val tclass = tokenClass(elem)
            val action = action_table[state.rawValue][tclass.rawValue]
            // print("state \(state), seen \(elem), tclass \(tclass), action \(action)")

            when (action) {
                Ac.st_std -> {
                    stdPars.add(symbolArg(elem, "lamba parameter"))
                }
                Ac.st_key -> {
                    val (key, defval) = parse_2parlist(elem)
                    // print("&key param \(key), defval \(defval)")
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
                                         + "ting ${PLS_desc[state.rawValue]}, "
                                         + "$lambda_name")
                }
            }
            state = new_state[state.rawValue][tclass.rawValue]
            // print("new state \(state)")
            argptr = argptr.cdr()
        }
        if (argptr !== Nil) {
            throw LambdaDefError("parameter list of $lambda_name is not a "
                                 + "proper list: $params")            
        }
        if (bodyforms is Pair) {
            if (bodyforms.car() is LispString) {
                docstring = bodyforms.car()
                argptr = bodyforms.cdr()
            }
        }
        restPars_sym = rest_sym // assign only once
        bodyforms = bodyforms
        self.env = env
        super.init(name = name,
                   stdPars = stdPars,
                   keyPars = keyPars,
                   optPars = optPars,
                   restPars = restPars_sym,
                   retval = intern("value"),
                   docbody = docstring)
    }

    fun bindPars(arglist: LispObject) {
        var args = ListIterator(arglist)
        for (param in stdPars) {
            
            guard val arg = args.next() else {
                val atleast = minargs == maxargs ? "" : "at least "
                throw ArgumentError("too few args for \(lambdatype) `\(name)`;"
                                      + " needs \(atleast)\(minargs)")
            }
            currentEnv.bind(param, arg)
        }
        for (sym, defval) in optPars {
            if val arg = args.next() {
                currentEnv.bind(sym, arg)
            } else {
                currentEnv.bind(sym, eval(defval))
            }
        }
        if args.isEmpty() {
            return
        }
        // keywords and rest
        var keyBound: Set<LispObject> = []
        var wantKeywordParam: Symbol? = null
        var restArgs: [LispObject] = []
        while val arg = args.next() {
            if val key = wantKeywordParam {
                currentEnv.bind(key, arg)
                keyBound.insert(key)
                wantKeywordParam = null
            } else if val varsym = key2var(arg) {
                wantKeywordParam = varsym
            } else {
                restArgs.add(arg)
            }
        }
        if val key = wantKeywordParam {
            throw ArgumentError("&key `\(key)` argument missing "
                                  + "calling \(lambdatype) `\(name)`")
        }
        for (sym, defval) in keyPars {
            if !keyBound.contains(sym) {
                currentEnv.bind(sym, defval)
            }
        }
        if val param = restPars {
            currentEnv.bind(param, array2list(restArgs))
        } else {
            if restArgs.count > 0 {
                throw ArgumentError("too many args for \(lambdatype) `\(name)`")
            }
        }
    }

    override fun call(arglist: LispObject): LispObject {
        // print("\(self).call(\(arglist))")
        return with_new_environment(parent: env) {
            bindPars(arglist)
            return evalProgn(bodyforms)
        }
    }

}
