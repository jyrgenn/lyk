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
fun bi_defmacro(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject
{
    var (name, rest1) = args
    var (params, bodyForms) = rest1
    val sym = symbolArg(name, "defmacro name")
    sym.setFunction(makeMacro(params, bodyforms, currentEnv, name))
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
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_macroexpand(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    return macroExpandForm(arg1(args))
}

// Core of quasiquote, the internal recursion.
fun qq_recurse(form: LispObject): LispObject {
    if (form !is Cons) {
        return form
    }
    val (head, tail) = form
    if (head === unquoteSymbol) {
        if (tail is Cons && tail.cdr() === Nil) {
            return eval(tail.car())            
        }
        throw ArgumentError("wrong number of args to unquote (takes 1)")
    }
    val tailResult = qq_recurse(tail)
    if (head !is Cons) {
        return Cons(head, tailResult)
    }
    val (headhead, cell2) = head
    if (headhead === unquoteSplicingSymbol) {
        if (!(cell2 is Cons && cell2.cdr() === Nil)) {
            throw ArgumentError("wrong number of args to unquote-splicing"
                                + "(takes 1)")
        }
        val expandedArg = eval(cell2.car())
        if (expandedArg is Cons) {
            expandedArg.lastCons().rplacd(tailResult)
            return expandedArg
        }
        throw TypeError("unquote-splicing arg not a list: `$expandedArg`")
    } else {
        return Cons(qq_recurse(head), tailResult)
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
fun bi_quasiquote(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
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
fun bi_unquote(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
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
fun bi_unquote_splicing(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    throw FunctionError("unquote-splicing outside of a quasiquote context")
}

// EOF
