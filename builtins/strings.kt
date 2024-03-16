// builtins dealing with strings (except io)

package org.w21.lyk

val stringSeparatorKey = intern(":sep")
val defaultStringSep = LispString.makeString(" ")

fun string_from_list(items: LispObject, sep: String): LispObject {
    var strings = mutableListOf<String>()
    for (item in items) {
        strings.add(item.toString())
    }
    return LispString.makeString(strings.joinToString(sep))
}

/// builtin string
/// fun     bi_string
/// std     
/// key     ":sep" to defaultStringSep
/// opt     
/// rest    items
/// ret     string
/// special no
/// doc {
/// Make a string from all arguments and return it. Keyword :sep specifies
/// a separator string (default: " ").
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string(args: LispObject, kwArgs: Map<LSymbol , LispObject>): LispObject {
    val sep = kwArgs[stringSeparatorKey].toString()
    return string_from_list(args, sep)
}

/// builtin join
/// fun     bi_join
/// std     items
/// key     
/// opt     sep defaultStringSep
/// rest    
/// ret     string
/// special no
/// doc {
/// Make a string from all items, separated by `sep` and return it.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_join(args: LispObject, kwArgs: Map<LSymbol, LispObject>): LispObject {
    val (items, sep) = args2(args)
    return string_from_list(items, sep.toString())
}

/// builtin regexp
/// fun     bi_regexp
/// std     pattern
/// key     
/// opt     
/// rest    
/// ret     regexp-object
/// special no
/// doc {
/// Return a new regexp object built from `pattern` (a kind of string).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_regexp(args: LispObject, kwArgs: Map<LSymbol, LispObject>): LispObject {
    val re = arg1(args)
    if (re is Regexp) {
        return re
    }
    return Regexp(re.toString())
}

/// builtin regexp-match
/// fun     bi_regexp_match
/// std     re string
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if regexp `re` matches `string`, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_regexp_match(args: LispObject, kwArgs: Map<LSymbol, LispObject>
): LispObject {
    val (re, string) = args2(args)
    val regex = regexpArg(re, "regexp-match regepx")
    val s = string.toString()
    return regex.match(s)
}
