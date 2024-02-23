// Basic Lisp functionality.

// Return the contents of the address part of the `list` register.
fun bi_car(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return listArg(arg1(args), "car").car()
}

// Return the contents of the decrement part of the `list` register.
fun bi_cdr(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return listArg(arg1(args), "cdr").cdr()
}

// Replace the car of `cons` with `new-car` and return `cons`.
fun bi_rplaca(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var (cons, newcar) = args2(arglist)
    consArg(cons, "rplaca").rplaca(newcar)
    return cons
}

// Replace the cdr of `cons` with `new-cdr` and return `cons`.
fun bi_rplacd(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var (cons, newcar) = args2(arglist)
    consArg(cons, "rplacd").rplacd(newcar)
    return cons
}

// Return the (potentially new) interned symbol with the name `name` (a string).
fun bi_intern(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return Symbol.intern(stringArg(arg1(args), "intern"))
}

// Return a list with the elements `elems`.
fun bi_list(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return args
}

// Return a new cons consisting of `car` and `cdr`.
fun bi_cons(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (car, cdr) = args2(args)
    return Cons(car, cdr)
}

// Assign `value` to the variable `symbol`; return the new value.
fun bi_set(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (sym, value) = args2(args)
    symbolArg(sym, "set").setValue(value)
    return value
}

// Return the expression `expr` without evaluating it.
fun bi_quote(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return arg1(args)
}

// Assign the value of `expr` to (un-evaluated) `variable` and return the value.
fun bi_setq(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (sym, value) = args2(args)
    symbolArg(sym, "setq").setValue(eval(value))
    return value
}

// Evaluate bodyforms with local bindings, return value of last bodyform.
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
            if (cons.cdr() === Nil) {
                vals.add(Nil)
            } else {
                val rest = cons.cdr()
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
        for ((sym, value) in zip(syms, vals)) {
            sym.bindValue(value)
        }
        return evalProgn(bodyforms)
    }
}

// Evaluate bodyforms with local bindings, return value of last bodyform.
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
            if (cons.cdr() === Nil) {
                vals.add(Nil)
            } else {
                val rest = cons.cdr()
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
        for ((sym, value) in zip(syms, vals)) {
            sym.bindValue(eval(value))
        }
        return evalProgn(bodyforms)
    }
}

// Evaluate `args` until one is nil; return the last evaluated value.
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
fun bi_when(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (condition, when_clauses) = args
    if (eval(condition) !== Nil) {
        return evalProgn(when_clauses)
    }
    return Nil
}

// If `condition` evaluates to nil, eval all `else-clauses` and return
// the value of the last.
fun bi_unless(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (condition, else_clauses) = args
    if (eval(condition) === Nil) {
        return evalProgn(else_clauses)
    }
    return Nil
}

// Evaluate all `bodyforms` and return the value of the last one.
fun bi_progn(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return evalProgn(args)
}

// construct and return a lambda function
fun bi_lambda(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var (params, bodyforms) = args2(args)
    return Lambda(params, bodyforms, currentEnv)
}

// construct and return a function
fun bi_defun(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var (name, params, bodyforms) = args3(args)
    val sym = symbolArg(name, "function name")
    sym.set_function(Lambda(params, bodyforms, currentEnv, sym))
    return sym
}

// Return t if `expr` is nil, else nil.
fun bi_null(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return bool2ob(arg1(args) === Nil)
}

// Return t if arguments have the same atomic value or are the same object.
// Strings are atomic, and equal strings are also eq.

fun bi_eq(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (a1, a2) = args2(args)
    return bool2ob(a1 === a2)
}


// Return the function value of a symbol, or the argument if it is a function.
fun bi_function(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val arg = arg1(args)
    if (arg is Function) {
        return arg
    }
    if (arg is Symbol) {
        if (arg.function != null) {
            return arg.function
        }
    }
    throw ArgumentError("$arg is not a function or function symbol")
}

// Return the function bound to SYMBOL.
fun bi_symbol_function(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val sym = symbolArg(arg1(args), "symbol")
    if (sym.function != null) {
            return sym.function
    }
    throw ArgumentError("symbol $sym has no function value")
}

fun bi_error(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (message, data) = args2(args)
    throw UserError(message.desc, data)
}

fun bi_catch(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (tagform, bodyforms) = args
    val tag = eval(tagform)
    try {
        return evalProgn(bodyforms)
    } catch (sig: ThrowSignal) {
        if (sig.tag === tag) {
            return sig.value
        }
        throw error
    }
}

fun bi_throw(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (tag, value) = args2(args)
    throw ThrowSignal(tag, value)
}

fun bi_boundp(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val sym = symbolArg(arg1(args), "boundp")
    if (sym.getValueOptional() != null) {
        return T
    }
    return Nil
}

fun bi_fboundp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val sym = symbolArg(arg1(args), "fboundp")
    if (sym.function != null) {
        return T
    }
    return Nil
}

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

fun bi_makunbound(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val sym = symbolArg(arg1(args), "makunbound")
    currentEnv.unbind(sym)
    return sym
}

fun bi_fmakunbound(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val sym = symbolArg(arg1(args), "makunbound")
    sym.set_function(null)              // checked in the symbol object
    return sym
}

fun bi_funcall(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val (function, rest) = args
    return functionArg(function, "funcall").call(args)
}

fun bi_apply(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (function, rest) = args
    return functionArg(function, "funcall").call(spreadArglist(rest))
}

fun bi_environmentp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Environment)
}

fun bi_errorp(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return bool2ob(arg1(args) is LispError)
}

fun bi_stringp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is LispString)
}

fun bi_numberp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args)is Number)
}

fun bi_consp(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return bool2ob(arg1(args) is Cons)
}

fun bi_regexpp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Regexp)
}

fun bi_symbolp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Symbol)
}

fun bi_tablep(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Table)
}

fun bi_vectorp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Vector)
}

fun bi_listp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val arg = arg1(args)
    return bool2ob(arg is Cons || arg === Nil)
}

fun bi_functionp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Function)
}

fun bi_builtinp(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Builtin)
}

fun bi_macrop(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return bool2ob(arg1(args) is Macro)
}

fun bi_length(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return makeNumber(arg1(args).length())
}

fun bi_typeof(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return intern(typeOf(arg1(args)))
}

fun bi_loop(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    while (true) {         // fake to suppress the warning
        evalProgn(args)
    }
    return Nil
}

fun bi_while(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (condition, rest) = args
    
    while (eval(condition) !== Nil) {
        evalProgn(args)
    }
    return Nil
}

fun bi_unwind_protect(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    val (bodyform, unwindforms) = args
    try {
        return eval(bodyform)
    } finally {
        evalProgn(unwindforms)
    }
}

fun bi_gensym(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val prefix = arg1(args).toString()

    if (prefix != "G#" && symbolTable[prefix] == null) {
        return Symbol.uninterned(prefix)
    }
    while (true) {
        val name = prefix + String(gensymCounter)
        gensymCounter += 1
        if (symbolTable[name] == null) {
            return Symbol.uninterned(name)
        }
    }
}


// Returns a new list that is the concatenation of LISTS.
// The list structure of all but the last list is copied.
fun bi_append(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val lc = ListCollector()

    while (args is Cons) {
        val arg = args.car()
        if (args.cdr() === Nil) {
            lc.lastcdr(arg)
            return lc.list()
        }
        for (elem in arg) {
            lc.add(elem)
        }
        args = args.cdr()
    }
    return lc.list
}

fun bi_fset(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (sym, func) = args2(args)
    val symbol = symbolArg(sym, "fset symbol")
    val function = functionArg(func, "fset function")
    symbol.set_function(function)
    return function
}

// Assign the value of `expr` to (un-evaluated) `variable` and return the value.
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
fun bi_last(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val list_arg = arg1(args)
    if (list_arg === Nil) {
        return Nil
    }
    return lastCons(consArg(list_arg, "last"))
}

fun bi_read(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (tentative_stream, eof_error_p, eof_value) = args3(args)
    var input_stream: Stream? = null

    if (tentative_stream === Nil) {
        input_stream = stdin
    } else if (tentative_stream is Stream) {
        input_stream = tentative_stream
    } else if (tentative_stream is StringObject) {
        input_stream = StringStream(tentative_stream.value)
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

TODO
fun bi_flet(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    var args = args
    val bindings = pop(&args)
    var bound_symbols: [(Symbol, Function?)] = []
    defer {
        do {
            for (symbol, function) in bound_symbols {
                symbol.set_function(function)
            }
        } catch {}
    }
    for binding in bindings {
        var b = binding
        val sym = symbolArg(pop(&b), "fval function variable")
        bound_symbols.add((sym, sym.function))
        sym.set_function(functionArg(eval(Cons(intern("lambda"),
                                                           b)),
                                             "fval function"))
    }
    return evalProgn(args)
}

fun bi_symbol_name(args: LispObject, key_args: Map<Symbol, LispObject>
): LispObject {
    return makeString(symbolArg(arg1(args), "symbol-name").name)
}

fun bi_atom(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    return bool2ob(arg1(args).isAtom())
}

fun bi_equal(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (arg1, arg2) = args2(args)
    return bool2ob(arg1.equal(arg2))
}

fun bi_eval(args: LispObject, key_args: Map<Symbol, LispObject>): LispObject {
    val (expr, env) = args2(args)

    if env === Nil {
        return eval(expr)
    }
    return with_environment(env: envArg(env, "eval environment")) {
        eval(expr)
    }
}

