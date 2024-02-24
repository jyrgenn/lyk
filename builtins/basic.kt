// Basic Lisp functionality.

package org.w21.lyk

// Return the contents of the address part of the `list` register.
@Suppress("UNUSED_PARAMETER")
fun bi_car(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return listArg(arg1(args), "car").car()
}

// Return the contents of the decrement part of the `list` register.
@Suppress("UNUSED_PARAMETER")
fun bi_cdr(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return listArg(arg1(args), "cdr").cdr()
}

// Replace the car of `cons` with `new-car` and return `cons`.
@Suppress("UNUSED_PARAMETER")
fun bi_rplaca(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var (cons, newcar) = args2(args)
    consArg(cons, "rplaca").rplaca(newcar)
    return cons
}

// Replace the cdr of `cons` with `new-cdr` and return `cons`.
@Suppress("UNUSED_PARAMETER")
fun bi_rplacd(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var (cons, newcar) = args2(args)
    consArg(cons, "rplacd").rplacd(newcar)
    return cons
}

// Return the (potentially new) interned symbol with the name `name` (a string).
@Suppress("UNUSED_PARAMETER")
fun bi_intern(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return intern(stringArg(arg1(args), "intern"))
}

// Return a list with the elements `elems`.
@Suppress("UNUSED_PARAMETER")
fun bi_list(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return args
}

// Return a new cons consisting of `car` and `cdr`.
@Suppress("UNUSED_PARAMETER")
fun bi_cons(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (car, cdr) = args2(args)
    return Cons(car, cdr)
}

// Assign `value` to the variable `symbol`; return the new value.
@Suppress("UNUSED_PARAMETER")
fun bi_set(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (sym, value) = args2(args)
    symbolArg(sym, "set").setValue(value)
    return value
}

// Return the expression `expr` without evaluating it.
@Suppress("UNUSED_PARAMETER")
fun bi_quote(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return arg1(args)
}

// Assign the value of `expr` to (un-evaluated) `variable` and return the value.
@Suppress("UNUSED_PARAMETER")
fun bi_setq(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (sym, value) = args2(args)
    symbolArg(sym, "setq").setValue(eval(value))
    return value
}

// Evaluate bodyforms with local bindings, return value of last bodyform.
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

// Evaluate bodyforms with local bindings, return value of last bodyform.
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

// Evaluate `args` until one is nil; return the last evaluated value.
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

// Evaluate `args` until one is non-nil; return the last evaluated value.
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

// For each clause of the form (condition expr) evaluate condition, and for the
// first one that is non-nil, return the value of evaluating expr.
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

// If `condition` evals to non-nil, eval `then-clause` and return the value.
// Otherwise, evaluate `else-clauses` and return the last value.
@Suppress("UNUSED_PARAMETER")
fun bi_if(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (condition, rest) = args
    val (when_clause, else_clauses) = rest
    if (eval(condition) !== Nil) {
        return eval(when_clause)
    }
    return evalProgn(else_clauses)
}

// If `condition` evaluates to non-nil, eval all `then-clauses` and return
// the value of the last.
@Suppress("UNUSED_PARAMETER")
fun bi_when(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (condition, when_clauses) = args
    if (eval(condition) !== Nil) {
        return evalProgn(when_clauses)
    }
    return Nil
}

// If `condition` evaluates to nil, eval all `else-clauses` and return
// the value of the last.
@Suppress("UNUSED_PARAMETER")
fun bi_unless(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (condition, else_clauses) = args
    if (eval(condition) === Nil) {
        return evalProgn(else_clauses)
    }
    return Nil
}

// Evaluate all `bodyforms` and return the value of the last one.
@Suppress("UNUSED_PARAMETER")
fun bi_progn(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return evalProgn(args)
}

// construct and return a lambda function
@Suppress("UNUSED_PARAMETER")
fun bi_lambda(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var (params, bodyforms) = args2(args)
    return makeLambda(params, bodyforms)
}

// construct and return a function
@Suppress("UNUSED_PARAMETER")
fun bi_defun(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var (name, params, bodyforms) = args3(args)
    val sym = symbolArg(name, "function name")
    sym.setFunction(makeLambda(params, bodyforms, currentEnv, sym))
    return sym
}

// Return t if `expr` is nil, else nil.
@Suppress("UNUSED_PARAMETER")
fun bi_null(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return bool2ob(arg1(args) === Nil)
}

// Return t if arguments have the same atomic value or are the same object.
// Strings are atomic, and equal strings are also eq.

@Suppress("UNUSED_PARAMETER")
fun bi_eq(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (a1, a2) = args2(args)
    return bool2ob(a1 === a2)
}


// Return the function value of a symbol, or the argument if it is a function.
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

// Return the function bound to SYMBOL.
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

@Suppress("UNUSED_PARAMETER")
fun bi_error(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (message, data) = args2(args)
    throw UserError(message.desc(), data)
}

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

@Suppress("UNUSED_PARAMETER")
fun bi_throw(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (tag, value) = args2(args)
    throw ThrowSignal(tag, value)
}

@Suppress("UNUSED_PARAMETER")
fun bi_boundp(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val sym = symbolArg(arg1(args), "boundp")
    if (sym.getValueOptional() != null) {
        return T
    }
    return Nil
}

@Suppress("UNUSED_PARAMETER")
fun bi_fboundp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val sym = symbolArg(arg1(args), "fboundp")
    if (sym.function != null) {
        return T
    }
    return Nil
}

@Suppress("UNUSED_PARAMETER")
fun bi_errset(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (expr, print_error) = args2(args)
    val saveErrset = inErrset
    inErrset = true
    try {
        return Cons(eval(expr), Nil)
    } catch (lerror: LispError) {
        val errObj = lerror.asObject()
        intern("*last-error*").setValue(errObj, silent = true)
        if (print_error != Nil) {
            print(errObj)
        }
        return Nil
    } finally {
        inErrset = saveErrset
    }
}

@Suppress("UNUSED_PARAMETER")
fun bi_makunbound(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val sym = symbolArg(arg1(args), "makunbound")
    currentEnv.unbind(sym)
    return sym
}

@Suppress("UNUSED_PARAMETER")
fun bi_fmakunbound(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val sym = symbolArg(arg1(args), "makunbound")
    sym.setFunction(null)               // checked in the symbol object
    return sym
}

@Suppress("UNUSED_PARAMETER")
fun bi_funcall(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val (function, rest) = args
    return functionArg(function, "funcall").call(rest)
}

@Suppress("UNUSED_PARAMETER")
fun bi_apply(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (function, rest) = args
    return functionArg(function, "funcall").call(spreadArglist(rest))
}

@Suppress("UNUSED_PARAMETER")
fun bi_environmentp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Environment)
}

@Suppress("UNUSED_PARAMETER")
fun bi_errorp(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return bool2ob(arg1(args) is ErrorObject)
}

@Suppress("UNUSED_PARAMETER")
fun bi_stringp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is LispString)
}

@Suppress("UNUSED_PARAMETER")
fun bi_numberp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args)is Number)
}

@Suppress("UNUSED_PARAMETER")
fun bi_consp(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return bool2ob(arg1(args) is Cons)
}

@Suppress("UNUSED_PARAMETER")
fun bi_regexpp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Regexp)
}

@Suppress("UNUSED_PARAMETER")
fun bi_symbolp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Symbol)
}

@Suppress("UNUSED_PARAMETER")
fun bi_tablep(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return bool2ob(arg1(args) is Table)
}

@Suppress("UNUSED_PARAMETER")
fun bi_vectorp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Vector)
}

@Suppress("UNUSED_PARAMETER")
fun bi_listp(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val arg = arg1(args)
    return bool2ob(arg is Cons || arg === Nil)
}

@Suppress("UNUSED_PARAMETER")
fun bi_functionp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Function)
}

@Suppress("UNUSED_PARAMETER")
fun bi_builtinp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Builtin)
}

// @Suppress("UNUSED_PARAMETER")
// fun bi_macrop(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
//     return bool2ob(arg1(args) is Macro)
// }

@Suppress("UNUSED_PARAMETER")
fun bi_length(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return makeNumber(arg1(args).length())
}

@Suppress("UNUSED_PARAMETER")
fun bi_typeof(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return intern(typeOf(arg1(args)))
}

@Suppress("UNUSED_PARAMETER")
fun bi_loop(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    while (true) {
        evalProgn(args)
    }
}

@Suppress("UNUSED_PARAMETER")
fun bi_while(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (condition, rest) = args
    
    while (eval(condition) !== Nil) {
        evalProgn(args)
    }
    return Nil
}

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

@Suppress("UNUSED_PARAMETER")
fun bi_gensym(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val prefix = arg1(args).toString()

    if (prefix != "G#" && symbolTable[prefix] == null) {
        return uninternedSymbol(prefix)
    }
    while (true) {
        val name = prefix + gensymCounter.toString()
        gensymCounter += 1
        if (symbolTable[name] == null) {
            return uninternedSymbol(name)
        }
    }
}


// Returns a new list that is the concatenation of LISTS.
// The list structure of all but the last list is copied.
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

@Suppress("UNUSED_PARAMETER")
fun bi_fset(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (sym, func) = args2(args)
    val symbol = symbolArg(sym, "fset symbol")
    val function = functionArg(func, "fset function")
    symbol.setFunction(function)
    return function
}

// Assign the value of `expr` to (un-evaluated) `variable` and return the value.
@Suppress("UNUSED_PARAMETER")
fun bi_defvar(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (sym, value, doc) = args3(args)
    val symbol = symbolArg(sym, "defvar symbol")
    if (symbol.getValueOptional() == null) {
        symbol.setValue(eval(value), silent = true)
    }
    if (doc !== Nil) {
        val docstring = stringArg(doc, "defvar docstring")
        symbol.putprop(intern("docstring"), makeString(docstring))
    }
    return symbol
}

// Define variable SYMBOL with optional INITIAL-VALUE and DOCSTRING.
// If the variable is already bound, its value is changed nonetheless.
@Suppress("UNUSED_PARAMETER")
fun bi_defparameter(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (sym, value, doc) = args3(args)
    val symbol = symbolArg(sym, "defvar symbol")
    symbol.setValue(eval(value), silent = true)
    if (doc !== Nil) {
        val docstring = stringArg(doc, "defvar docstring")
        symbol.putprop(intern("docstring"), makeString(docstring))
    }
    return symbol
}


// 
@Suppress("UNUSED_PARAMETER")
fun bi_last(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val list_arg = arg1(args)
    if (list_arg === Nil) {
        return Nil
    }
    return lastCons(consArg(list_arg, "last"))
}

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

@Suppress("UNUSED_PARAMETER")
fun bi_symbol_name(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return makeString(symbolArg(arg1(args), "symbol-name").name)
}

@Suppress("UNUSED_PARAMETER")
fun bi_atom(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return bool2ob(arg1(args).isAtom())
}

@Suppress("UNUSED_PARAMETER")
fun bi_equal(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (arg1, arg2) = args
    return bool2ob(arg1.equal(arg2))
}

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

