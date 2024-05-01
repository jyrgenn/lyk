// Basic Lisp functionality.

package org.w21.lyk

/// builtin car
/// fun     bi_car
/// std     list
/// key     
/// opt     
/// rest    
/// ret     object
/// special no
/// doc {
/// Return the contents of the address part of the `list` register.
/// The car of nil is nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_car(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return listArg(arg1(args), "car").car
}

/// builtin cdr
/// fun     bi_cdr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     object
/// special no
/// doc {
/// Return the contents of the decrement part of the `list` register.
/// The cdr of nil is nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cdr(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return listArg(arg1(args), "cdr").cdr
}

/// builtin rplaca
/// fun     bi_rplaca
/// std     cons new-car
/// key     
/// opt     
/// rest    
/// ret     cons
/// special no
/// doc {
/// Replace the car of `cons` with `new-car` and return `cons`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_rplaca(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    var (cons, newcar) = args2(args)
    consArg(cons, "rplaca").car = newcar
    return cons
}

/// builtin rplacd
/// fun     bi_rplacd
/// std     cons new-cdr
/// key     
/// opt     
/// rest    
/// ret     cons
/// special no
/// doc {
/// Replace the cdr of `cons` with `new-cdr` and return `cons`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_rplacd(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    var (cons, newcar) = args2(args)
    consArg(cons, "rplacd").cdr = newcar
    return cons
}

/// builtin intern
/// fun     bi_intern
/// std     name
/// key     
/// opt     
/// rest    
/// ret     symbol
/// special no
/// doc {
/// Return the (maybe new) interned symbol with the name `name` (a string).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_intern(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return intern(stringArg(arg1(args), "intern"))
}

/// builtin list
/// fun     bi_list
/// std     
/// key     
/// opt     
/// rest    elems
/// ret     list
/// special no
/// doc {
/// Return a list with the elements `elems`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_list(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return args
}

/// builtin list*
/// fun     bi_list_star
/// std     
/// key     
/// opt     
/// rest    elems+
/// ret     list
/// special no
/// doc {
/// Return a list of `elems`, with the last as the end of the list.
/// list* is like list except that the last argument to list becomes
/// the car of the last cons constructed, while the last argument to
/// list* becomes the cdr of the last cons constructed.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_list_star(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return spreadArglist(args)
}

/// builtin cons
/// fun     bi_cons
/// std     car cdr
/// key     
/// opt     
/// rest    
/// ret     cons
/// special no
/// doc {
/// Return a new cons consisting of `car` and `cdr`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cons(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (car, cdr) = args2(args)
    return LCons(car, cdr)
}

/// builtin set
/// fun     bi_set
/// std     value symbol
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Assign `value` to the variable `symbol`; return the new value.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_set(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (sym, value) = args2(args)
    symbolArg(sym, "set").setValue(value)
    return value
}

/// builtin quote
/// fun     bi_quote
/// std     expr
/// key     
/// opt     
/// rest    
/// ret     expr
/// special yes
/// doc {
/// Return the expression `expr` without evaluating it.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_quote(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return arg1(args)
}

/// builtin setq
/// fun     bi_setq
/// std     
/// key     
/// opt     
/// rest    symbol-value-settings
/// ret     new-value
/// special yes
/// doc {
/// Assign the value of `expr` to (un-evaluated) `symbol` and return the value.
/// Multiple settings like `(setq var1 form1 var2 form2 ...)` are possible.
/// First form1 is evaluated and the result is stored in the variable var1,
/// then form2 is evaluated and the result stored in var2, and so forth.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_setq(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    if (args.length % 2 != 0) {
        throw ArgumentError("odd number of arguments to setq")
    }
    var list: LObject = args
    var value: LObject = Nil
    while (list is LCons) {
        val symbol = symbolArg(list.car, "setq symbol")
        list = list.cdr
        value = eval(list.car)
        debug(debugSetqSym) {
            "setq $symbol to ${list.car} => $value"
        }
        symbol.setValue(value)
        list = list.cdr
    }
    return value
}

fun destructuringBind(name: String, vars: LObject, values: LObject,
                      syms: MutableList<LSymbol>, vals: MutableList<LObject>) {
    val lfvars = ListFeed(vars)
    var lfvalues = ListFeed(values)
    
    while (lfvars.hasNext()) {
        if (!lfvalues.isList()) {
            ArgumentError("$name: bind value for ${lfvars.rest} not "
                          +"a pair: ${lfvalues.rest}")
        }
        val curvar = lfvars.next()
        val curvalue = lfvalues.next()
        when (curvar) {
            is LSymbol -> if (curvar !== Nil) {
			      // skip assignment if symbol is Nil, meaning we
			      // don't need that particular value
                              syms.add(curvar)
                              vals.add(curvalue)
                          }
            is LCons -> destructuringBind(name, curvar, curvalue, syms, vals)
            else -> throw TypeError(curvar, "symbol", "$name varlist")
        }
    }
    // if the variable list is ended by a non-nil symbol, bind remaining
    // values to it (or nil)
    val the_end = lfvars.rest
    when (the_end) {
        Nil -> return
        is LSymbol -> {
            syms.add(the_end)
            vals.add(lfvalues.rest)
        }
        else ->
            throw TypeError(the_end, "symbol", "$name binding")
    }
}

fun let_internal(args: LObject, is_letrec: Boolean): LObject {
    if (args === Nil) {
        return Nil                      // no bindings *and* no bodyforms
    }

    val name = if (is_letrec) "let*" else "let"
    val (bindings, bodyforms) = args
    // now we're talking
    var syms = mutableListOf<LSymbol>()     // variable symbols to bind
    var vals = mutableListOf<LObject>() // values to bind to them
    
    for (binding in bindings) {
        if (binding is LSymbol) {
            syms.add(binding)
            vals.add(Nil)
	    debug(debugLetBindSym) {
                "will bind lone $binding to nil"
            }
        } else if (binding is LCons) {
	    val (varlist, rest) = binding
	    if (rest != Nil && rest !is LCons) {
		throw ArgumentError(
                    "$name: malformed variable clause for `$varlist`")
	    }
	    if (rest.cdr != Nil) {
		throw ArgumentError(
                    "$name: malformed binding clause for `$varlist`") 
	    }
	    var value = rest.car
            if (!is_letrec) {
	        value = eval(value)
            }
            // I could check for a single symbol to bind before pulling out the
            // big gun, but it turns out this optimisation does not bring a
            // preceptible speed advantage.
            destructuringBind(name, varlist, value, syms, vals)
	    debug(debugLetBindSym) {
                if (is_letrec) {
                    "will bind $varlist to eval($value)"
                } else {
                    "will bind $varlist to $value"
                }
            }
        } else {
            throw ArgumentError("$name: malformed variables list")
        }
    }
    // do the bindings
    return withNewEnvironment() {
        val sym_i = syms.iterator()
        val val_i = vals.iterator()

        while (sym_i.hasNext()) {
            val sym = sym_i.next()
            var value = val_i.next()
            if (is_letrec) {
                value = eval(value)
            }
            sym.bind(value)
        }
        evalProgn(bodyforms)
    }
}

/// builtin let
/// fun     bi_let
/// std     bindings
/// key     
/// opt     
/// rest    bodyforms
/// ret     value
/// special yes
/// doc {
/// Evaluate bodyforms with local bindings, return value of last bodyform.
/// Bindings are of the form `symbol` or `(symbol)` or `(symbol value)`,
/// where the first two bind `symbol` to nil. All `value`s are evaluated
/// before any variable bindings are done.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_let(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return let_internal(args, false)
}

/// builtin let*
/// fun     bi_letrec
/// std     bindings
/// key     
/// opt     
/// rest    bodyforms
/// ret     value
/// special yes
/// doc {
/// Evaluate bodyforms with local bindings, return value of last bodyform.
/// Bindings are of the form `symbol` or `(symbol)` or `(symbol value)`,
/// where the first two bind `symbol` to nil. VALUE is evaluated with
/// bindings of earlier variables in the same let* already in place.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_letrec(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject{
    return let_internal(args, true)
}


/// builtin and
/// fun     bi_and
/// std     
/// key     
/// opt     
/// rest    args
/// ret     value
/// special yes
/// doc {
/// Evaluate `args` until one is nil; return the last evaluated value.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_and(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    var value: LObject = T
    for (arg in args) {
        value = eval(arg)
        if (value === Nil) {
            break
        }
    }
    return value
}

/// builtin or
/// fun     bi_or
/// std     
/// key     
/// opt     
/// rest    args
/// ret     value
/// special yes
/// doc {
/// Evaluate `args` until one is non-nil; return the last evaluated value.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_or(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    var value: LObject = Nil
    for (arg in args) {
        value = eval(arg)
        if (value !== Nil) {
            break
        }
    }
    return value
}

// For each clause of the form (condition expr1 ...) evaluate condition, and for the
// first one that is non-nil, return the value of evaluating expr.
/// builtin cond
/// fun     bi_cond
/// std     
/// key     
/// opt     
/// rest    clauses
/// ret     value
/// special yes
/// doc {
/// For each clause of the form (condition expressions...) evaluate condition.
/// For the first one that is non-nil, return the last value of evaluating the
/// expressions. If none of the conditions is non-nil, return nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cond(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    for (arg in args) {
        val clause = consArg(arg, "cond clause")
        val condition = eval(clause.car)
        if (condition !== Nil) {
            val forms = clause.cdr
            if (forms === Nil) {
                /// CLHS says: If there are no forms in that clause, the primary
                /// value of the test-form is returned by the cond form.
                return condition
            }
            return evalProgn(forms)
        }
    }
    return Nil
}

/// builtin if
/// fun     bi_if
/// std     condition then-clause
/// key     
/// opt     
/// rest    else-clauses
/// ret     value
/// special yes
/// doc {
/// If `condition` evals to non-nil, eval `then-clause` and return the value.
/// Otherwise, evaluate `else-clauses` and return the last value.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_if(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (condition, rest) = args
    val (when_clause, else_clauses) = rest
    if (eval(condition) !== Nil) {
        return eval(when_clause)
    }
    return evalProgn(else_clauses)
}

/// builtin when
/// fun     bi_when
/// std     condition
/// key     
/// opt     
/// rest    then-clauses
/// ret     value
/// special yes
/// doc {
/// If `condition` evaluates to non-nil, eval all `then-clauses` and return
/// the value of the last. Otherwise return nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_when(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (condition, when_clauses) = args
    if (eval(condition) !== Nil) {
        return evalProgn(when_clauses)
    }
    return Nil
}

/// builtin unless
/// fun     bi_unless
/// std     condition
/// key     
/// opt     
/// rest    else-clauses
/// ret     value
/// special yes
/// doc {
/// If `condition` evaluates to nil, eval all `else-clauses` and return
/// the value of the last. Otherwise, returl nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_unless(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (condition, else_clauses) = args
    if (eval(condition) === Nil) {
        return evalProgn(else_clauses)
    }
    return Nil
}

/// builtin progn
/// fun     bi_progn
/// std     
/// key     
/// opt     
/// rest    bodyforms
/// ret     last-value
/// special yes
/// doc {
/// Evaluate all `bodyforms` and return the value of the last one.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_progn(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return evalProgn(args)
}

/// builtin prog1
/// fun     bi_prog1
/// std     result-form
/// key     
/// opt     
/// rest    bodyforms
/// ret     first-value
/// special yes
/// doc {
/// Evaluate all forms and return the value of the first one.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_prog1(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (form1, rest) = args
    val result = eval(form1)
    evalProgn(rest)
    return result
}

// construct and return a lambda function
/// builtin lambda
/// fun     bi_lambda
/// std     lambda-list
/// key     
/// opt     
/// rest    bodyforms
/// ret     function
/// special yes
/// doc {
/// Return an anonymous function with `lambda-list` and `bodyforms`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_lambda(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    var (params, bodyforms) = args
    return makeLambdaOrMacro(params, bodyforms,
                             location = lastTopLevelLocation)
}

/// builtin defun
/// fun     bi_defun
/// std     name lambda-list
/// key     
/// opt     docstring
/// rest    bodyforms
/// ret     name
/// special yes
/// doc {
/// Create a function with name `name`, `lambda-list`, and `bodyforms`.
/// Optional `docstring` should describe what the function does.
/// Return the function symbol `name`.
///
/// On calling the function, `bodyforms` will be evaluated in an environment
/// with the parameters bound to the actual arguments. The value of the last
/// form evaluated will be returned.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_defun(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (name, rest) = args
    val (params, bodyforms) = rest
    val sym = symbolArg(name, "function name")
    debug (debugDefunSym) {
        LCons(sym, params).toString()
    }
    sym.setFunction(makeLambdaOrMacro(params, bodyforms, currentEnv, sym,
                                      false, lastTopLevelLocation))
    return sym
}

/// builtin null
/// fun     bi_null
/// std     expr
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `expr` is nil, else nil.
/// }
/// end builtin
/// builtin not
/// fun     bi_null
/// std     expr
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `expr` is nil, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_null(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return bool2ob(arg1(args) === Nil)
}

/// builtin eq
/// fun     bi_eq
/// std     arg1 arg2
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return t if arguments have the same atomic value or are the same object.
/// Strings and numbers are atomic, and equal strings/numbers are also eq.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_eq(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (a1, a2) = args2(args)
    return bool2ob(a1 === a2)
}


/// builtin function
/// fun     bi_function
/// std     arg
/// key     
/// opt     
/// rest    
/// ret     function
/// special yes
/// doc {
/// Return the function value of a symbol, or the argument if it is a function.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_function(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val arg = arg1(args)
    if (arg is LFunction) {
        return arg
    }
    if (arg is LSymbol) {
        val function = arg.function
        if (function != null) {
            return function
        }
    }
    if (arg is LCons && (arg.car === LambdaSym ||
                             arg.car === greekLambdaSym)) {
        return eval(arg)
    }
    throw ArgumentError("$arg is not a function or function symbol")
}

/// builtin symbol-function
/// fun     bi_symbol_function
/// std     symbol
/// key     
/// opt     
/// rest    
/// ret     symbol
/// special no
/// doc {
/// Return the function bound to `symbol`.
/// It is an error if there is no function bound to `symbol`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_symbol_function(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val sym = symbolArg(arg1(args), "symbol")
    val function = sym.function
    if (function != null) {
            return function
    }
    throw ArgumentError("symbol $sym has no function value")
}

/// builtin error
/// fun     bi_error
/// std     message
/// key     
/// opt     data
/// rest    
/// ret     no-return
/// special no
/// doc {
/// Raise error with the message `message` and optional additional data.
/// The error exits all active calls immediately, except for errset.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_error(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (message, data) = args2(args)
    throw LispError(message.toString(), data)
}

/// builtin catch
/// fun     bi_catch
/// std     tag
/// key     
/// opt     
/// rest    bodyforms
/// ret     value
/// special yes
/// doc {
/// Eval `bodyforms` as implicit progn. If a throw occurs to the `tag`,
/// which is evaluated, return the value that is thrown. Otherwise, return
/// the value of the last bodyform.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_catch(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (tagform, bodyforms) = args
    val tag = eval(tagform)
    
    try {
	debug(debugCatchThrowSym) {
            "catch ($tag) opened"
        }
        val result = evalProgn(bodyforms)
	debug(debugCatchThrowSym) {
            "catch ($tag) closed"
        }
	return result
    } catch (sig: ThrowSignal) {
        if (sig.tag === tag) {
	    debug(debugCatchThrowSym) {
                "catch ($tag) caught $sig, will return"
            }
            return sig.value
        } else {
	    debug(debugCatchThrowSym) {
                "catch ($tag) caught $sig, will rethrow"
            }
            throw sig
	}
    }
}

/// builtin throw
/// fun     bi_throw
/// std     tag value
/// key     
/// opt     
/// rest    
/// ret     no-return
/// special no
/// doc {
/// Cause a non-local control transfer to the nearest enclosing catch
/// whose tag is eq to `tag`. The value returned by that catch is `value`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_throw(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (tag, value) = args2(args)
    debug(debugCatchThrowSym) {
         "throw ($tag) with $value"
    }
    throw ThrowSignal(tag, value)
}

/// builtin boundp
/// fun     bi_boundp
/// std     symbol
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if a value is bound to `symbol`, nil otherwise.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_boundp(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val sym = symbolArg(arg1(args), "boundp")
    return bool2ob(sym.getValueOptional() != null)
}

/// builtin fboundp
/// fun     bi_fboundp
/// std     symbol
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if a function is bound to `symbol`, nil otherwise.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_fboundp(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val sym = symbolArg(arg1(args), "fboundp")
    if (sym.function != null) {
        return T
    }
    return Nil
}

/// builtin errset
/// fun     bi_errset
/// std     expr
/// key     
/// opt     print-error T
/// rest    
/// ret     result
/// special yes
/// doc {
/// Return the value of `expr` as a singleton list; on error return nil.
/// In the latter case, a description of the error is in *last-error*, and,
/// if optional `print-error` is non-nil or omitted, it is printed as well.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_errset(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (expr, print_error) = args2(args)
    val saveErrset = inErrset
    inErrset = true
    try {
        return LCons(eval(expr), Nil)
    } catch (lerror: LispError) {
        val errObj = lerror.toObject()
        lastError.setValue(errObj, silent = true)
        if (print_error != Nil) {
            stderr.println(errObj)
        }
        return Nil
    } finally {
        inErrset = saveErrset
    }
}

/// builtin makunbound
/// fun     bi_makunbound
/// std     symbol
/// key     
/// opt     
/// rest    
/// ret     symbol
/// special no
/// doc {
/// Make `symbol`s value be undefined, return `symbol`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_makunbound(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val sym = symbolArg(arg1(args), "makunbound")
    currentEnv.unbind(sym)
    return sym
}

/// builtin fmakunbound
/// fun     bi_fmakunbound
/// std     symbol
/// key     
/// opt     
/// rest    
/// ret     symbol
/// special no
/// doc {
/// Make `symbol`s function be undefined, return `symbol`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_fmakunbound(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val sym = symbolArg(arg1(args), "makunbound")
    sym.setFunction(null)               // checked in the symbol object
    return sym
}

/// builtin funcall
/// fun     bi_funcall
/// std     function
/// key     
/// opt     
/// rest    arguments
/// ret     value
/// special no
/// doc {
/// Apply `function` to `arguments` and return the result value.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_funcall(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (function, rest) = args
    return functionArg(function, "funcall").call(rest)
}

/// builtin apply
/// fun     bi_apply
/// std     function
/// key     
/// opt     
/// rest    args+
/// ret     value
/// special no
/// doc {
/// Apply `function` to `args+` and return the result value.
/// `args+` is a spreadable argument list, meaning the last argument (meant
/// to be a list) is appended to the previous ones.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_apply(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (function, rest) = args
    val allargs = spreadArglist(rest)
    return functionArg(function, "funcall").call(allargs)
}

/// builtin environmentp
/// fun     bi_environmentp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `object` is an environment, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_environmentp(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return bool2ob(arg1(args) is LEnv)
}

/// builtin errorp
/// fun     bi_errorp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `object` is an error object, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_errorp(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return bool2ob(arg1(args) is ErrorObject)
}

/// builtin stringp
/// fun     bi_stringp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t is `object` is a string, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_stringp(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return bool2ob(arg1(args) is LString)
}

/// builtin numberp
/// fun     bi_numberp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t is `object` is a number, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_numberp(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return bool2ob(arg1(args) is LNumber)
}

/// builtin consp
/// fun     bi_consp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t is `object` is a cons cell, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_consp(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return bool2ob(arg1(args) is LCons)
}

/// builtin regexpp
/// fun     bi_regexpp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t is `object` is a regexp, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_regexpp(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return bool2ob(arg1(args) is LRegexp)
}

/// builtin symbolp
/// fun     bi_symbolp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `object` is a symbol, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_symbolp(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return bool2ob(arg1(args) is LSymbol)
}

/// builtin tablep
/// fun     bi_tablep
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t is `object` is a table, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_tablep(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return bool2ob(arg1(args) is LTable)
}

/// builtin vectorp
/// fun     bi_vectorp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t is `object` is a vector, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_vectorp(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return bool2ob(arg1(args) is LVector)
}

/// builtin listp
/// fun     bi_listp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `object` is a list, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_listp(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val arg = arg1(args)
    return bool2ob(arg is LCons || arg === Nil)
}

/// builtin sequencep
/// fun     bi_sequencep
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `object` is a sequence, else nil.
/// Lists, strings, and vectors are sequences.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_sequencep(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val ob = arg1(args)
    return bool2ob(if (ob is LSymbol) {
                       ob === Nil
                   } else {
                       ob is LSeq
                   })
}

/// builtin functionp
/// fun     bi_functionp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `object` is a function, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_functionp(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return bool2ob(arg1(args) is LFunction)
}

/// builtin builtinp
/// fun     bi_builtinp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `object` is a builtin function, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_builtinp(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return bool2ob(arg1(args) is LBuiltin)
}

/// builtin macrop
/// fun     bi_macrop
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `object` is a macro, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_macrop(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return bool2ob(arg1(args) is LMacro)
}

/// builtin length
/// fun     bi_length
/// std     sequence
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return the length of `sequence` as a number.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_length(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return makeNumber(arg1(args).length)
}

/// builtin type-of
/// fun     bi_typeof
/// std     object
/// key     
/// opt     
/// rest    
/// ret     symbol
/// special no
/// doc {
/// Return the type of `object` as a symbol.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_typeof(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return intern(arg1(args).type)
}

/// builtin loop
/// fun     bi_loop
/// std     
/// key     
/// opt     
/// rest    bodyforms
/// ret     none
/// special yes
/// doc {
/// Eval `bodyforms` again and again.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_loop(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    while (true) {
        evalProgn(args)
    }
}

/// builtin while
/// fun     bi_while
/// std     condition
/// key     
/// opt     
/// rest    bodyforms
/// ret     value
/// special yes
/// doc {
/// If `condition` evaluates non-nil, evaluate `bodyforms`; repeat.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_while(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (condition, bodyforms) = args
    
    while (eval(condition) !== Nil) {
        evalProgn(bodyforms)
    }
    return Nil
}

/// builtin until
/// fun     bi_until
/// std     condition
/// key     
/// opt     
/// rest    bodyforms
/// ret     value
/// special yes
/// doc {
/// If `condition` evaluates nil, evaluate `bodyforms`; repeat.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_until(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (condition, _) = args
    
    while (eval(condition) === Nil) {
        evalProgn(args)
    }
    return Nil
}

/// builtin unwind-protect
/// fun     bi_unwind_protect
/// std     bodyform
/// key     
/// opt     
/// rest    cleanupforms
/// ret     value
/// special yes
/// doc {
/// Eval `bodyform`, and even in case of an error or throw, eval `cleanupforms`.
/// If `bodyform` completes normally, return its value after executing the
/// `cleanupforms`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_unwind_protect(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (bodyform, unwindforms) = args
    try {
        return eval(bodyform)
    } finally {
        evalProgn(unwindforms)
    }
}

/// builtin gensym
/// fun     bi_gensym
/// std     
/// key     
/// opt     prefix makeString("G#")
/// rest    
/// ret     symbol
/// special no
/// doc {
/// Return a new, uninterned and unused symbol with a name prefix \"G#\".
/// The symbol is not bound or fbound and has an empty property list.
/// If a different `prefix` is given, it is tried as the name for the
/// symbol or, if the name is already in use, as the prefix of the name.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_gensym(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val prefix = arg1(args).toString()

    if (prefix != "G#" && symbolTable[prefix] == null) {
        return LSymbol.uninterned(prefix)
    }
    while (true) {
        val name = prefix + gensymCounter.toString()
        gensymCounter += 1
        if (symbolTable[name] == null) {
            return LSymbol.uninterned(name)
        }
    }
}


// Returns a new list that is the concatenation of LISTS.
// The list structure of all but the last list is copied.
/// builtin append
/// fun     bi_append
/// std     
/// key     
/// opt     
/// rest    lists
/// ret     value
/// special no
/// doc {
/// Return a new list that is the concatenation of `lists`.
/// The list structure of all but the last list is copied.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_append(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val lc = ListCollector()

    var pair = args
    while (pair is LCons) {
        val arg = pair.car
        if (pair.cdr === Nil) {
            lc.lastcdr(arg)
            return lc.list
        }
        for (elem in arg) {
            lc.add(elem)
        }
        pair = pair.cdr
    }
    return lc.list
}

/// builtin nconc
/// fun     bi_nconc
/// std     
/// key     
/// opt     
/// rest    lists
/// ret     value
/// special no
/// doc {
/// Return a new list that is the concatenation of `lists`.
/// The list structure of all but the last list is modified.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_nconc(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    var acc: LCons? = null              // accumulated lists
    var lastcell: LCons? = null         // last cons of acc

    for (list in args) {
        if (list === Nil) {
            continue
        }
        val next = consArg(list, "nconc")
        if (acc == null) {
            acc = next
            lastcell = lastCons(acc)
        } else {
            lastcell!!.cdr = next
            lastcell = lastCons(lastcell)
        }
    }
    return acc ?: Nil
}

/// builtin fset
/// fun     bi_fset
/// std     symbol new-func
/// key     
/// opt     
/// rest    
/// ret     new-func
/// special no
/// doc {
/// Set function of `symbol` to `new-func` (a function) and return `new-func`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_fset(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (sym, func) = args2(args)
    val symbol = symbolArg(sym, "fset symbol")
    val function = functionArg(func, "fset function")
    symbol.setFunction(function)
    return function
}

/// builtin defvar
/// fun     bi_defvar
/// std     symbol
/// key     
/// opt     initial-value, docstring
/// rest    
/// ret     symbol
/// special yes
/// doc {
/// Define variable `symbol` with optional `initial-value` and `docstring`.
/// If the variable is already bound, its value is not changed.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_defvar(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (sym, value, doc) = args3(args)
    val symbol = symbolArg(sym, "defvar symbol")
    if (symbol.getValueOptional() == null) {
        symbol.setValue(eval(value), silent = true)
    }
    if (doc !== Nil) {
        val docstring = stringArg(doc, "defvar docstring")
        symbol.putprop(intern("docstring"),
                       makeString(docstring))
    }
    symbol.putprop(varDefinedInPropSym, makeString(lastTopLevelLocation))
    return symbol
}

/// builtin defparameter
/// fun     bi_defparameter
/// std     symbol
/// key     
/// opt     initial-value, docstring
/// rest    
/// ret     symbl
/// special yes
/// doc {
/// Define variable `symbol` with optional `initial-value` and `docstring`.
/// If the variable is already bound, its value is changed nonetheless.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_defparameter(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (sym, value, doc) = args3(args)
    val symbol = symbolArg(sym, "defvar symbol")
    symbol.setValue(eval(value), silent = true)
    if (doc !== Nil) {
        val docstring = stringArg(doc, "defvar docstring")
        symbol.putprop(intern("docstring"),
                       makeString(docstring))
    }
    symbol.putprop(varDefinedInPropSym, makeString(lastTopLevelLocation))
    return symbol
}


/// builtin last
/// fun     bi_last
/// std     l
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the last pair of list `l`, or nil if `l` is nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_last(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val list_arg = arg1(args)
    if (list_arg === Nil) {
        return Nil
    }
    return lastCons(consArg(list_arg, "last"))
}

/// builtin read
/// fun     bi_read
/// std     
/// key     
/// opt     input-stream, eof-error-p, eof-value
/// rest    
/// ret     value
/// special no
/// doc {
/// Read an expression from `input-stream` (or stdin) and return it.
/// `input-stream` may be a stream or a string.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_read(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (tentative_stream, eof_error_p, eof_value) = args3(args)
    var input_stream: LStream? = null

    if (tentative_stream === Nil) {
        input_stream = stdin
    } else if (tentative_stream is LStream) {
        input_stream = tentative_stream
    } else if (tentative_stream is LString) {
        input_stream = StringReaderStream(tentative_stream.the_string)
    }
    if (input_stream == null) {
        throw ArgumentError("read argument not a stream or string: "
                            + " $input_stream")
    }
    val obj = Reader(input_stream, "*expr*").read().first
    if (obj == null) {
        if (eof_error_p !== Nil) {
            throw EOFError("unexpected EOF in read")
        }
        return eof_value
    }        
    return obj
}

/// builtin flet
/// fun     bi_flet
/// std     bindings
/// key     
/// opt     
/// rest    bodyforms
/// ret     value
/// special yes
/// doc {
/// Bind one or more functions to symbols and evaluate `bodyforms`.
/// The `bindings` are of the form (symbol (lambda-list) . bodyforms).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_flet(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (bindings, body) = args
    // These are the previous function bindings to be restored after
    var previous_bindings = mutableListOf<Pair<LSymbol, LFunction?>>()

    try {
        for (binding in listArg(bindings, "flet bindings")) {
            val (sym, funcrest) = listArg(binding, "flet binding")
            val symbol = symbolArg(sym, "flet function variable")
            previous_bindings.add(Pair(symbol, symbol.function))
            val (params, bodyforms) = listArg(funcrest,
                                              "flet function $symbol")
            symbol.setFunction(makeLambdaOrMacro(
                                   params, bodyforms))
        }
        return evalProgn(body)
    } finally {
        // restore previous function bindings
        for ((symbol, function) in previous_bindings) {
            symbol.setFunction(function)
        }
    }
}

/// builtin symbol-name
/// fun     bi_symbol_name
/// std     symbol
/// key     
/// opt     
/// rest    
/// ret     string
/// special no
/// doc {
/// Return the name of `symbol` as a string.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_symbol_name(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return makeString(symbolArg(arg1(args), "symbol-name").name)
}

/// builtin atom
/// fun     bi_atom
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `arg` is atomic (i.e. symbol, number, string, char), nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_atom(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return bool2ob(arg1(args).isAtom())
}

/// builtin equal
/// fun     bi_equal
/// std     obj1 obj1
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return non-nil iff the arguments are the same or have the same contents.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_equal(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (arg1, arg2) = args2(args)
    return bool2ob(arg1.equal(arg2))
}

/// builtin eval
/// fun     bi_eval
/// std     expr
/// key     
/// opt     environment
/// rest    
/// ret     value
/// special no
/// doc {
/// Evaluate `expr` in optional `environment` and return the value.
/// `environment` may be nil, in which case the current environment is used.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_eval(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (expr, env) = args2(args)

    if (env === Nil) {
        return eval(expr)
    }
    return withEnvironment(envArg(env, "eval environment")) {
        eval(expr)
    }
}

/// builtin get
/// fun     bi_getprop
/// std     symbol property
/// key     
/// opt     default-value
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the value of `symbol`'s property `property` (or nil, if not set).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_getprop(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (symbol, property, default) = args3(args)
    val sym = symbolArg(symbol, "get symbol")
    val prop = symbolArg(property, "get property")
    return sym.getProp(prop, default)
}

/// builtin put
/// fun     bi_putprop
/// std     symbol property value
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the value of `symbol`'s property `property` (or nil, if not set).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_putprop(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (symbol, property, value) = args3(args)
    val sym = symbolArg(symbol, "put symbol")
    val prop = symbolArg(property, "put property")
    sym.setProp(prop, value)
    return value
}

/// builtin remprop
/// fun     bi_remprop
/// std     symbol property
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Remove `property` from `symbol` and return the previous value (or Nil).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_remprop(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (symbol, property) = args3(args)
    val sym = symbolArg(symbol, "put symbol")
    val prop = symbolArg(property, "put property")
    return sym.remProp(prop)
}

/// builtin reverse
/// fun     bi_reverse
/// std     sequence
/// key     
/// opt     
/// rest    
/// ret     list
/// special no
/// doc {
/// Reverse `sequence` (by copying) and return the result.
/// The resulting sequence does not share structure with the argument list.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_reverse(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return seqArg(arg1(args), "reverse").reversed()
}

/// builtin nreverse
/// fun     bi_nreverse
/// std     list
/// key     
/// opt     
/// rest    
/// ret     list
/// special no
/// doc {
/// Reverse `list` (maybe by modifying a list's cdrs) and return the result.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_nreverse(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    var cell = arg1(args)
    var next = cell.cdr

    if (cell === Nil) {
        return Nil
    }
    (cell as LCons).cdr = Nil
    while (next is LCons) {
        val nextPair = next
        next = nextPair.cdr
        nextPair.cdr = cell
        cell = nextPair
    }
    return cell
}

/// builtin mapcar
/// fun     bi_mapcar
/// std     function
/// key     
/// opt     
/// rest    lists+
/// ret     value-list`
/// special no
/// doc {
/// Apply `function` to the first members of the argument lists, then the
/// second and so on; return the list of resulting values. Any excess
/// values are discarded.
/// Example:
///   (mapcar #'cons '(3 4 5) '(a b c d))
///   => ((3 . a) (4 . b) (5 . c))
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_mapcar(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (func, lists) = args
    val function = functionArg(func, "mapcar function")
    var listArgs = lists
    val results = ListCollector()

    fun allCars(): LObject? {
        val cars = ListCollector()
        var overLists = listArgs

        while (overLists is LCons) {
            val thisList = overLists.car
            if (thisList is LCons) {
                cars.add(thisList.car)
                overLists.car = thisList.cdr
            } else {
                return null
            }
            overLists = overLists.cdr
        }
        return cars.list
    }

    while (true) {
        val cars = allCars()
        if (cars == null) {
            return results.list
        }
        results.add(function.call(cars))
    }
}

/// builtin rplaca-ret-value
/// fun     bi_rplaca_ret_value
/// std     cons new-car
/// key     
/// opt     
/// rest    
/// ret     new-car
/// special no
/// doc {
/// Replace the car of `cons` with `new-car` and return `new-car`.
/// Intended for use by setf.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_rplaca_ret_value(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (the_cons, new_value) = args2(args)
    consArg(the_cons, "rplaca-ret-value cons").car = new_value
    return new_value
}

/// builtin rplacd-ret-value
/// fun     bi_rplacd_ret_value
/// std     cons new-cdr
/// key     
/// opt     
/// rest    
/// ret     new-cdr
/// special no
/// doc {
/// Replace the cdr of `cons` with `new-car` and return `new-cdr`.
/// Intended for use by setf.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_rplacd_ret_value(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (the_cons, new_value) = args2(args)
    consArg(the_cons, "rplacd-ret-value cons").cdr = new_value
    return new_value
}

/// builtin function-body
/// fun     bi_function_body
/// std     function
/// key     
/// opt     
/// rest    
/// ret     bodyforms
/// special no
/// doc {
/// Return the body forms of `function`.
/// The returned object is a list of the actual body forms of the function,
/// not a copy; modifying it will have direct impact on the function's
/// behaviour.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_function_body(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return functionArg(arg1(args), "function-body").body()
}

/// builtin identity
/// fun     bi_identity
/// std     arg
/// key     
/// opt     
/// rest    
/// ret     arg
/// special no
/// doc {
/// Return `arg` unchanged.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_identity(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return arg1(args)
}

/// builtin ignore
/// fun     bi_ignore
/// std     
/// key     
/// opt     
/// rest    args
/// ret     nil
/// special no
/// doc {
/// Return nil
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_ignore(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return Nil
}

/// builtin nth
/// fun     bi_nth
/// std     n list
/// key     
/// opt     
/// rest    
/// ret     object
/// special no
/// doc {
/// Return the `n`th element of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_nth(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (n, list) = args2(args)
    var l = listArg(list, "nth list")
    val n_n = intArg(n, "nth n")
    if (n_n < 0) {
        throw ArgumentError("nth `n` argument must not be negative")
    }

    for (i in 0..<n_n) {
        l = l.cdr
    }
    return l.car
}

/// builtin nthcdr
/// fun     bi_nthcdr
/// std     n list
/// key     
/// opt     
/// rest    
/// ret     object
/// special no
/// doc {
/// Return the `n`th cons of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_nthcdr(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (n, list) = args2(args)
    var l = listArg(list, "nthcdr list")
    val n_n = intArg(n, "nthcdr n")
    if (n_n < 0) {
        throw ArgumentError("nthcdr `n` argument must not be negative")
    }

    for (i in 0..<n_n) {
        l = l.cdr
    }
    return l
}

/// builtin charp
/// fun     bi_charp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t iff `object` is a character, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_charp(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return bool2ob(arg1(args) is LChar)
}

/// builtin lambdap
/// fun     bi_lambdap
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t iff `object` is a lambda, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_lambdap(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return bool2ob(arg1(args) is Lambda)
}

