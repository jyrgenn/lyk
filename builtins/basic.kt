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
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_car(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return listArg(arg1(args), "car").car()
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
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cdr(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return listArg(arg1(args), "cdr").cdr()
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
fun bi_rplaca(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var (cons, newcar) = args2(args)
    consArg(cons, "rplaca").rplaca(newcar)
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
fun bi_rplacd(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var (cons, newcar) = args2(args)
    consArg(cons, "rplacd").rplacd(newcar)
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
fun bi_intern(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return Symbol.intern(stringArg(arg1(args), "intern"))
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
fun bi_list(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return args
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
fun bi_cons(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (car, cdr) = args2(args)
    return Cons(car, cdr)
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
fun bi_set(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
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
fun bi_quote(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return arg1(args)
}

/// builtin setq
/// fun     bi_setq
/// std     symbol expr
/// key     
/// opt     
/// rest    
/// ret     new-value
/// special yes
/// doc {
/// Assign the value of `expr` to (un-evaluated) `symbol` and return the value.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_setq(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (sym, value) = args2(args)
    val newvalue = eval(value)
    symbolArg(sym, "setq").setValue(newvalue)
    return value
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
fun bi_let(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    if (args === Nil) {
        return Nil                      // no bindings *and* no bodyforms
    }

    val (bindings, bodyforms) = args
    // now we're talking
    var syms = mutableListOf<Symbol>()     // variable symbols to bind
    var vals = mutableListOf<LispObject>() // values to bind to them
    
    for (arg in bindings) {
        if (arg is Symbol) {
            syms.add(arg)
            vals.add(Nil)
        } else if (arg is Cons) {
            syms.add(symbolArg(arg.car(), "let binding variable"))
            if (arg.cdr() === Nil) {
                vals.add(Nil)
            } else {
                val rest = arg.cdr()
                if (rest is Cons) {
                    if (rest.cdr() !== Nil) {
                        throw ArgumentError("let: malformed binding clause")
                    }
                    vals.add(eval(rest.car()))
                } else {
                    throw ArgumentError("let: malformed variable clause")
                }
            }
        } else {
            throw ArgumentError("let: malformed variables list")
        }
    }
    // do the bindings
    return with_new_environment() {
        val sym_i = syms.iterator()
        val val_i = syms.iterator()

        while (sym_i.hasNext()) {
            val sym = sym_i.next()
            val value = val_i.next()
            sym.bind(value)
        }
        evalProgn(bodyforms)
    }
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
fun bi_letrec(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject{
    if (args === Nil) {
        return Nil                      // no bindings *and* no bodyforms
    }

    val (bindings, bodyforms) = args
    // now we're talking
    var syms = mutableListOf<Symbol>()     // variable symbols to bind
    var vals = mutableListOf<LispObject>() // values to bind to them
    
    for (arg in bindings) {
        if (arg is Symbol) {
            syms.add(arg)
            vals.add(Nil)
        } else if (arg is Cons) {
            syms.add(symbolArg(arg.car(), "let binding variable"))
            if (arg.cdr() === Nil) {
                vals.add(Nil)
            } else {
                val rest = arg.cdr()
                if (rest is Cons) {
                    if (rest.cdr() !== Nil) {
                        throw ArgumentError("let: malformed binding clause")
                    }
                    vals.add(rest.car())
                } else {
                    throw ArgumentError("let: malformed variable clause")
                }
            }
        } else {
            throw ArgumentError("let: malformed variables list")
        }
    }
    // do the bindings
    return with_new_environment() {
        val sym_i = syms.iterator()
        val val_i = syms.iterator()

        while (sym_i.hasNext()) {
            val sym = sym_i.next()
            val value = val_i.next()
            sym.bind(eval(value))
        }
        evalProgn(bodyforms)
    }
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
fun bi_and(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var value: LispObject = T
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
fun bi_or(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var value: LispObject = Nil
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
/// fun     bi_cons
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
fun bi_cond(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    for (arg in args) {
        val clause = consArg(arg, "cond clause")
        if (eval(clause.car()) !== Nil) {
            return evalProgn(clause.cdr())
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
fun bi_if(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
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
fun bi_when(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
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
fun bi_unless(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
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
fun bi_progn(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return evalProgn(args)
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
fun bi_lambda(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var (params, bodyforms) = args2(args)
    return makeLambda(params, bodyforms)
}

/// builtin defun
/// fun     bi_defun
/// std     name lambda-list
/// key     
/// opt     
/// rest    [docstring] bodyforms
/// ret     value
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
fun bi_defun(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (name, rest) = args
    val (params, bodyforms) = rest

    val sym = symbolArg(name, "function name")
    sym.setFunction(makeLambda(params, bodyforms, currentEnv, sym))
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
@Suppress("UNUSED_PARAMETER")
fun bi_null(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
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
fun bi_eq(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
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
fun bi_function(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val arg = arg1(args)
    if (arg is Function) {
        return arg
    }
    if (arg is Symbol) {
        val function = arg.function
        if (function != null) {
            return function
        }
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
fun bi_symbol_function(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
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
fun bi_error(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (message, data) = args2(args)
    throw UserError(message.desc(), data)
}

/// builtin catch
/// fun     bi_catch
/// std     tag
/// key     
/// opt     
/// rest    
/// ret     value
/// special yes
/// doc {
/// Eval `bodyforms` as implicit progn. If a throw occurs to the `tag`,
/// which is evaluated, return the value that is thrown. Otherwise, return
/// the value of the last bodyform.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_catch(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (tagform, bodyforms) = args
    val tag = eval(tagform)
    
    try {
        return evalProgn(bodyforms)
    } catch (sig: ThrowSignal) {
        if (sig.tag === tag) {
            return sig.value
        }
        throw sig
    }
}

/// builtin throw
/// fun     bi_throw
/// std     tag value
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Cause a non-local control transfer to the nearest enclosing catch
/// whose tag is eq to `tag`. The value returned by that catch is `value`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_throw(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (tag, value) = args2(args)
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
fun bi_boundp(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val sym = symbolArg(arg1(args), "boundp")
    if (sym.getValueOptional() != null) {
        return T
    }
    return Nil
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
fun bi_fboundp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
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
fun bi_errset(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (expr, print_error) = args2(args)
    val saveErrset = inErrset
    inErrset = true
    try {
        return Cons(eval(expr), Nil)
    } catch (lerror: LispError) {
        val errObj = lerror.asObject()
        Symbol.intern("*last-error*").setValue(errObj, silent = true)
        if (print_error != Nil) {
            print(errObj)
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
fun bi_makunbound(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
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
fun bi_fmakunbound(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
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
fun bi_funcall(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
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
fun bi_apply(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (function, rest) = args
    return functionArg(function, "funcall").call(spreadArglist(rest))
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
fun bi_environmentp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Environment)
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
fun bi_errorp(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
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
fun bi_stringp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is LispString)
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
fun bi_numberp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args)is Number)
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
fun bi_consp(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return bool2ob(arg1(args) is Cons)
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
fun bi_regexpp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Regexp)
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
/// Return t is `object` is a symbol, else nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_symbolp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Symbol)
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
fun bi_tablep(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return bool2ob(arg1(args) is Table)
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
fun bi_vectorp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Vector)
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
fun bi_listp(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val arg = arg1(args)
    return bool2ob(arg is Cons || arg === Nil)
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
fun bi_functionp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Function)
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
fun bi_builtinp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Builtin)
}

// /// builtin macrop
// /// fun     bi_macrop
// /// std     object
// /// key     
// /// opt     
// /// rest    
// /// ret     t/nil
// /// special no
// /// doc {
// /// Return t if `object` is a macro, else nil.
// /// }
// /// end builtin
// @Suppress("UNUSED_PARAMETER")
// fun bi_macrop(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
//     return bool2ob(arg1(args) is Macro)
// }

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
fun bi_length(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return makeNumber(arg1(args).length())
}

/// builtin typeof
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
fun bi_typeof(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return Symbol.intern(typeOf(arg1(args)))
}

/// builtin loop
/// fun     bi_loop
/// std     bodyform
/// key     
/// opt     
/// rest    more-bodyforms
/// ret     no-return
/// special yes
/// doc {
/// Eval `bodyforms` again and again.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_loop(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
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
fun bi_while(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (condition, _) = args
    
    while (eval(condition) !== Nil) {
        evalProgn(args)
    }
    return Nil
}

/// builtin unwind_protect
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
fun bi_unwind_protect(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
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
fun bi_gensym(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val prefix = arg1(args).toString()

    if (prefix != "G#" && symbolTable[prefix] == null) {
        return Symbol.uninterned(prefix)
    }
    while (true) {
        val name = prefix + gensymCounter.toString()
        gensymCounter += 1
        if (symbolTable[name] == null) {
            return Symbol.uninterned(name)
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
fun bi_append(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val lc = ListCollector()

    var pair = args
    while (pair is Cons) {
        val arg = pair.car()
        if (pair.cdr() === Nil) {
            lc.lastcdr(arg)
            return lc.list()
        }
        for (elem in arg) {
            lc.add(elem)
        }
        pair = pair.cdr()
    }
    return lc.list()
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
fun bi_fset(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
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
/// special no
/// doc {
/// Define variable `symbol` with optional `initial-value` and `docstring`.
/// If the variable is already bound, its value is not changed.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_defvar(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (sym, value, doc) = args3(args)
    val symbol = symbolArg(sym, "defvar symbol")
    if (symbol.getValueOptional() == null) {
        symbol.setValue(eval(value), silent = true)
    }
    if (doc !== Nil) {
        val docstring = stringArg(doc, "defvar docstring")
        symbol.putprop(Symbol.intern("docstring"), makeString(docstring))
    }
    return symbol
}

/// builtin defparameter
/// fun     bi_defparameter
/// std     symbol
/// key     
/// opt     initial-value, docstring
/// rest    
/// ret     symbl
/// special no
/// doc {
/// Define variable `symbol` with optional `initial-value` and `docstring`.
/// If the variable is already bound, its value is changed nonetheless.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_defparameter(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (sym, value, doc) = args3(args)
    val symbol = symbolArg(sym, "defvar symbol")
    symbol.setValue(eval(value), silent = true)
    if (doc !== Nil) {
        val docstring = stringArg(doc, "defvar docstring")
        symbol.putprop(Symbol.intern("docstring"), makeString(docstring))
    }
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
fun bi_last(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
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
fun bi_read(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (tentative_stream, eof_error_p, eof_value) = args3(args)
    var input_stream: Stream? = null

    if (tentative_stream === Nil) {
        input_stream = stdinStream
    } else if (tentative_stream is Stream) {
        input_stream = tentative_stream
    } else if (tentative_stream is LispString) {
        input_stream = StringReaderStream(tentative_stream.value)
    }
    if (input_stream == null) {
        throw ArgumentError("read argument not a stream or string: "
                            + " $input_stream")
    }
    val obj = Reader(input_stream, "*expr*").read()
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
/// rest    
/// ret     value
/// special no
/// doc {
/// Bind one or more functions to symbols and evaluate `bodyforms`.
/// The `bindings` are of the form (symbol (lambda-list) . bodyforms).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_flet(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (bindings, body) = args
    // These are the previous function bindings to be restored after
    var previous_bindings = mutableListOf<Pair<Symbol, Function?>>()

    try {
        for (binding in listArg(bindings, "flet bindings")) {
            val (sym, funcrest) = listArg(binding, "flet binding")
            val symbol = symbolArg(sym, "flet function variable")
            previous_bindings.add(Pair(symbol, symbol.function))
            val (params, bodyforms) = listArg(funcrest,
                                              "flet function $symbol")
            symbol.setFunction(makeLambda(params, bodyforms))
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
fun bi_symbol_name(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
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
fun bi_atom(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
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
fun bi_equal(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (arg1, arg2) = args
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
fun bi_eval(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (expr, env) = args

    if (env === Nil) {
        return eval(expr)
    }
    return with_environment(envArg(env, "eval environment")) {
        eval(expr)
    }
}

