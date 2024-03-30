// builtins dealing with strings (except io)

package org.w21.lyk

val stringSeparatorKey = intern("sep")
val defaultStringSep = makeString(" ")

fun string_from_list(items: LObject, sep: String = ""): LObject {
    var strings = mutableListOf<String>()
    for (item in items) {
        strings.add(item.toString())
    }
    return makeString(strings.joinToString(sep))
}

/// builtin string
/// fun     bi_string
/// std     
/// key
/// opt     
/// rest    items
/// ret     string
/// special no
/// doc {
/// Make a string from all arguments and return it.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string(args: LObject, kwArgs: Map<LSymbol , LObject>): LObject {
    return string_from_list(args)
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
fun bi_join(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
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
fun bi_regexp(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val re = arg1(args)
    if (re is LRegexp) {
        return re
    }
    return LRegexp(re.toString())
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
fun bi_regexp_match(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (re, string) = args2(args)
    val regex = regexpArg(re, "regexp-match regepx")
    val s = string.toString()
    return regex.match(s)
}

/// builtin regexp-split
/// fun     bi_regexp_split
/// std     re string
/// key     
/// opt     limit makeNumber(0)
/// rest    
/// ret     string-list
/// special no
/// doc {
/// Split the string around regexp matches and return a list of the parts.
/// If `limit` is greater than (default) 0, only so many parts are split
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_regexp_split(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (re, s, limit) = args3(args)
    return collectedList {
        for (elem in regexpArg(re, "regexp-split regexp").split(
                 s.toString(), intArg(limit, "regexp-split limit"))) {
            it.add(makeString(elem))
        }
    }
}

/// builtin regexp-replace
/// fun     bi_regexp_replace
/// std     re string replacement
/// key     
/// opt     limit makeNumber(0)
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if regexp `re` replacees `string`, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_regexp_replace(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (re, s, replacement, limit) = args4(args)
    val regexp = regexpArg(re, "regexp-replace regexp")
    return regexp.replace(s.toString(), replacement.toString(),
                          intArg(limit, "regexp-replace limit"))
}

