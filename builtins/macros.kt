// builtin functions related to macros

package org.w21.lyk

/// builtin defmacro
/// fun     bi_defmacro
/// std     name lambda-list
/// key     
/// opt     docstring
/// rest    bodyforms
/// ret     name
/// special yes
/// doc {
/// Create a macro with name `name`, parameters `lambda-list`, and `bodyforms`.
/// `params` is a list of parameter symbols for the function.
/// Optional `docstring` should describe what the function does.
/// Return the function symbol `name`.
/// 
/// On calling the function, `bodyforms` will be evaluated in an environment
/// with the parameters bound to the actual arguments. The value of the last
/// form evaluated will be returned.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_defmacro(args: LObject, kwArgs: Map<LSymbol, LObject> ,
                suppp: Map<LSymbol, Boolean>): LObject
{
    var (name, rest1) = args
    var (params, bodyForms) = rest1
    val sym = symbolArg(name, " name")
    sym.setFunction(makeMacro(params, bodyForms, sym,
                              location = lastTopLevelLocation))
    return name
}

/// builtin macroexpand
/// fun     bi_macroexpand
/// std     form
/// key     
/// opt     
/// rest    
/// ret     expanded-form
/// special no
/// doc {
/// Expand macros in `form` and return the expanded form.
/// macroexpand repeatedly expands form until it is no longer a macro form. 
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_macroexpand(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
    return macroExpandForm(arg1(args))
}

/// builtin macroexpand-1
/// fun     bi_macroexpand_1
/// std     form
/// key     
/// opt     
/// rest    
/// ret     expanded-form
/// special no
/// doc {
/// Do one step of macro expansion in `form` and return the expanded form.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_macroexpand_1(args: LObject, kwArgs: Map<LSymbol, LObject>,
                     suppp: Map<LSymbol, Boolean>): LObject {
    return macroExpandFormRecurse(arg1(args)).first
}

// Core of quasiquote, the internal recursion.
fun qq_recurse(form: LObject): LObject {
    if (form !is LCons) {
        return form
    }
    val (head, tail) = form
    if (head === unquoteSymbol) {
        if (tail is LCons && tail.cdr === Nil) {
            return eval(tail.car)            
        }
        throw ArgumentError("wrong number of args to unquote (takes 1)")
    }
    val tailResult = qq_recurse(tail)
    if (head !is LCons) {
        return LCons(head, tailResult)
    }
    val (headhead, cell2) = head
    if (headhead === unquoteSplicingSymbol) {
        if (!(cell2 is LCons && cell2.cdr === Nil)) {
            throw ArgumentError("wrong number of args to unquote-splicing"
                                + "(takes 1)")
        }
        val expandedArg = eval(cell2.car)
        if (expandedArg is LCons) {
            lastCons(expandedArg).cdr = tailResult
            return expandedArg
        }
        if (expandedArg == Nil) {
            return Nil
        }
        throw TypeError("unquote-splicing arg not a list: `$expandedArg`")
    } else {
        return LCons(qq_recurse(head), tailResult)
    }
}
    
/// builtin quasiquote
/// fun     bi_quasiquote
/// std     expr
/// key     
/// opt     
/// rest    
/// ret     expanded-form
/// special yes
/// doc {
/// In `expr`, replace unquoted items them with their values as appropriate.
/// Return the resulting form.
/// 
//// Unquoted items are those preceded by an »unquote« sign (»,«) or an
/// »unquote-splicing« (»,@«). In the latter case, if the value is a list,
/// splice it into the surrounding list.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_quasiquote(args: LObject, kwArgs: Map<LSymbol, LObject>,
                  suppp: Map<LSymbol, Boolean>): LObject {
    return qq_recurse(arg1(args))
}

/// builtin unquote
/// fun     bi_unquote
/// std     form
/// key     
/// opt     
/// rest    
/// ret     no-return
/// special no
/// doc {
/// Throw an error if called as a function outside of a quasiquote.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_unquote(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
    throw FunctionError("unquote outside of a quasiquote context")
}

/// builtin unquote-splicing
/// fun     bi_unquote_splicing
/// std     form
/// key     
/// opt     
/// rest    
/// ret     no-return
/// special no
/// doc {
/// Throw an error if called as a function outside of a quasiquote.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_unquote_splicing(args: LObject, kwArgs: Map<LSymbol, LObject>,
                        suppp: Map<LSymbol, Boolean>): LObject {
    throw FunctionError("unquote-splicing outside of a quasiquote context")
}

// EOF
