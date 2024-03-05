// c[ad]+r functions

package org.w21.lyk

/// builtin caar
/// fun     bi_caar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the caar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_caar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().car()
}

/// builtin cadr
/// fun     bi_cadr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cadr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cadr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().car()
}

/// builtin cdar
/// fun     bi_cdar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cdar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cdar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().cdr()
}

/// builtin cddr
/// fun     bi_cddr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cddr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cddr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().cdr()
}

/// builtin caaar
/// fun     bi_caaar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the caaar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_caaar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().car().car()
}

/// builtin caadr
/// fun     bi_caadr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the caadr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_caadr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().car().cdr()
}

/// builtin cadar
/// fun     bi_cadar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cadar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cadar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().cdr().car()
}

/// builtin caddr
/// fun     bi_caddr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the caddr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_caddr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().cdr().car()
}

/// builtin cdaar
/// fun     bi_cdaar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cdaar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cdaar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().car().cdr()
}

/// builtin cdadr
/// fun     bi_cdadr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cdadr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cdadr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().car().cdr()
}

/// builtin cddar
/// fun     bi_cddar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cddar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cddar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().cdr().cdr()
}

/// builtin cdddr
/// fun     bi_cdddr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cdddr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cdddr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().cdr().cdr()
}


/// builtin caaaar
/// fun     bi_caaaar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the caaaar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_caaaar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().car().car().car()
}

/// builtin caaadr
/// fun     bi_caaadr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the caaadr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_caaadr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().car().car().car()
}

/// builtin caadar
/// fun     bi_caadar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the caadar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_caadar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().cdr().car().car()
}

/// builtin caaddr
/// fun     bi_caaddr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the caaddr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_caaddr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().cdr().car().car()
}

/// builtin cadaar
/// fun     bi_cadaar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cadaar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cadaar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().car().cdr().car()
}

/// builtin cadadr
/// fun     bi_cadadr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cadadr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cadadr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().car().cdr().car()
}

/// builtin caddar
/// fun     bi_caddar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the caddar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_caddar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().cdr().cdr().car()
}

/// builtin cadddr
/// fun     bi_cadddr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cadddr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cadddr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().cdr().cdr().car()
}

/// builtin cdaaar
/// fun     bi_cdaaar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cdaaar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cdaaar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().car().car().cdr()
}

/// builtin cdaadr
/// fun     bi_cdaadr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cdaadr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cdaadr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().car().car().cdr()
}

/// builtin cdadar
/// fun     bi_cdadar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cdadar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cdadar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().cdr().car().cdr()
}

/// builtin cdaddr
/// fun     bi_cdaddr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cdaddr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cdaddr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().cdr().car().cdr()
}

/// builtin cddaar
/// fun     bi_cddaar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cddaar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cddaar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().car().cdr().cdr()
}

/// builtin cddadr
/// fun     bi_cddadr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cddadr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cddadr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().car().cdr().cdr()
}

/// builtin cdddar
/// fun     bi_cdddar
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cdddar of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cdddar(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).car().cdr().cdr().cdr()
}

/// builtin cddddr
/// fun     bi_cddddr
/// std     list
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Return the cddddr of `list`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_cddddr(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    return arg1(args).cdr().cdr().cdr().cdr()
}

