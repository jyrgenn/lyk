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
/// std     regexp string
/// key     
/// opt     limit
/// rest    
/// ret     value
/// special no
/// doc {
/// Return a list of matches if regexp `regexp` matches `string`, nil else.
/// The returned value in case of a match is a list of the values for the
/// whole match and possible group matches. If an optional group (...)?
/// does not match, its value is "".
/// With optional third argument `limit`, a list of match lists for
/// (potentially) multiple matches is returned. If `limit` is t, all matches
/// are considered; otherwise, a number specifies the number of matches to
/// be considered.
///
/// Regular expression syntax is that of the Kotlin regexp package (RE2),
/// which is largely similar to that of the Perl and Python languages.
/// A \"(?flags)\" specification in the regexp can modify the behaviour
/// of the match in the current group. Possible flags are:
/// 
/// i  case-insensitive (default false)
/// m  multi-line mode: ^ and $ match begin/end line in addition to begin/end
///    text (default false)
/// s  let . match \\n (default false)
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_regexp_match(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (re, string, limit) = args3(args)
    val regexp = regexpArg(re, "regexp-match regexp")
    val s = string.toString()

    if (limit === Nil) {
        return regexp.match(s)
    }
    val matchlist = regexp.findAll(s)
    if (limit === T) {
        return matchlist
    }
    if (limit is LNumber) {
        return firstN(matchlist, limit.toInt())
    }
    throw ArgumentError(
        "regexp-match `limit` argument is not a number or t or nil: $limit")
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

