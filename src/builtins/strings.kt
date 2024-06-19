// builtins dealing with strings (except io)

package org.w21.lyk

val stringSeparatorKey = intern("sep")
val defaultStringSep = makeString(" ")


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
fun bi_string(args: LObject, kwArgs: Map<LSymbol , LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
    return makeString(args.joinToString(""))
}

/// builtin make-string
/// fun     bi_make_string
/// std     length
/// key     
/// opt     initial makeString("\u0020")
/// rest    
/// ret     string
/// special no
/// doc {
/// Return a string of `length` with the contents taken from `initial`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_make_string(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
    val (length, initial) = args2(args)
    val len = intArg(length, " length")
    val sb = StrBuf()
    
    // If we have a function argument as initial value, call it for every
    // element of the string to build, and use the first rune of the resulting
    // string value of the result.
    
    if (initial is LFunction) {
        for (i in 0..<len) {
            val elem = initial.call(Nil).toString()
            if (elem.length < 1) {
                throw ArgumentError("initializer function ${initial.desc(null)} "
                                    + "returns an empty string")
            }
            sb.add(elem[0])
        }
    } else {
        val init = initial.toString()
        for (i in 0..<len) {
            sb.add(init[i % init.length])
        }
        
    }
    return makeString(sb.toString())
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
/// Make a string from all `items` (a sequence), separated by `sep`
/// and return it.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_join(args: LObject, kwArgs: Map<LSymbol, LObject>,
            suppp: Map<LSymbol, Boolean>): LObject {
    val (items, sep) = args2(args)
    seqArg(items, " items")
    return makeString(items.joinToString(sep.toString()))
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
/// See `regexp-match` for more information about regular expressions.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_regexp(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
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
fun bi_regexp_match(args: LObject, kwArgs: Map<LSymbol, LObject> ,
                    suppp: Map<LSymbol, Boolean>): LObject {
    val (re, string, limit) = args3(args)
    val regexp = regexpArg(re, " regexp")
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

fun regexp_split(regexp: LRegexp, s: String, limit: Int): LObject {
    return collectedList {
        for (elem in regexp.split(s, limit)) {
            it.add(makeString(elem))
        }
    }
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
fun bi_regexp_split(args: LObject, kwArgs: Map<LSymbol, LObject>,
                    suppp: Map<LSymbol, Boolean>): LObject {
    val (re, s, limit) = args3(args)
    return regexp_split(regexpArg(re, " regexp"), s.toString(),
                        intArg(limit, " limit"))
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
fun bi_regexp_replace(args: LObject, kwArgs: Map<LSymbol, LObject>,
                      suppp: Map<LSymbol, Boolean>): LObject {
    val (re, s, replacement, limit) = args4(args)
    val regexp = regexpArg(re, " regexp")
    return regexp.replace(s.toString(), replacement.toString(),
                          intArg(limit, " limit"))
}

/// builtin string-split
/// fun     bi_string_split
/// std     string
/// key     
/// opt     separator, limit, keep-empty
/// rest    
/// ret     string-list
/// special no
/// doc {
/// Split `string` into parts separated by `separator`; return them as list.
/// If `separator` is a regexp object, a regexp match is done instead of a
/// string match. If it is nil or unspecified, it is assumed to be
/// whitespace. if `limit` is non-nil and positive, it is the maximum
/// number of parts into which the string is split.
/// If separator and keep-empty are both nil, don't keep empty parts at
/// the beginning or the end of the list.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string_split(args: LObject, kwArgs: Map<LSymbol, LObject>,
                    suppp: Map<LSymbol, Boolean>): LObject {
    val (string, separator, limit, keep_empty) = args4(args)
    var s = string.toString()
    var regexp: LRegexp
    var ilimit: Int

    if (separator === Nil) {
        if (keep_empty === Nil) {
            s = s.trim()
        }
        regexp = LRegexp("\\s+")
    } else if (separator is LRegexp) {
        regexp = separator
    } else {
        regexp = LRegexp(Regex.escape(separator.toString()))
    }
    if (limit === Nil) {
        ilimit = 0
    } else {
        ilimit = intArg(limit, " limit")
        if (ilimit < 0) {
            ilimit = 0
        }
    }
    return regexp_split(regexp, s, ilimit)
}

fun substring_limits(start: LObject, end: LObject, len: Int): Pair<Int, Int> {
    val pos1 = (if (start === Nil)
                    0
                else
                    min(len, max(0, intArg(start, " start"))))
    val pos2 = (if (end === Nil)
                    len
                else
                    max(0, min(len, intArg(end, " start"))))
    return Pair(pos1, pos2)
}


/// builtin string-upcase
/// fun     bi_string_upcase
/// std     string
/// key     
/// opt     start, end
/// rest    
/// ret     string
/// special no
/// doc {
/// Return `string` with all lowercase chars replaced by uppercase chars.
/// `start` and `end` may specify the region of the string to be treated;
/// defaults are 0 and the end of the string.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string_upcase(args: LObject, kwArgs: Map<LSymbol, LObject>,
                     suppp: Map<LSymbol, Boolean>): LObject {
    val (string, start, end) = args3(args)
    val s = string.toString()
    val len = s.length

    val (pos1, pos2) = substring_limits(start, end, len)

    if (pos1 >= pos2) {
        return makeString(s)
    }
    try {
        val prefix = s.substring(0, pos1)
        val selected = s.substring(pos1, pos2)
        val suffix = s.substring(pos2, len)
        return makeString(prefix + selected.uppercase() + suffix)
    } catch (e: java.lang.StringIndexOutOfBoundsException) {
        // cannot happen
        throw IndexError("range [$pos1, $pos2) out of bounds for "
                         + "length $len in string-upcase")
    }
}

/// builtin string-downcase
/// fun     bi_string_downcase
/// std     string
/// key     
/// opt     start, end
/// rest    
/// ret     string
/// special no
/// doc {
/// Return `string` with all lowercase chars replaced by uppercase chars.
/// `start` and `end` may specify the region of the string to be treated;
/// defaults are 0 and the end of the string.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string_downcase(args: LObject, kwArgs: Map<LSymbol, LObject>,
                       suppp: Map<LSymbol, Boolean>): LObject {
    val (string, start, end) = args3(args)
    val s = string.toString()
    val len = s.length

    val (pos1, pos2) = substring_limits(start, end, len)

    if (pos1 >= pos2) {
        return makeString(s)
    }
    try {
        val prefix = s.substring(0, pos1)
        val selected = s.substring(pos1, pos2)
        val suffix = s.substring(pos2, len)
        return makeString(prefix + selected.lowercase() + suffix)
    } catch (e: java.lang.StringIndexOutOfBoundsException) {
        // cannot happen
        throw IndexError("range [$pos1, $pos2) out of bounds for "
                         + "length $len in string-downcase")
    }
}

val word_chars =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

fun capitalize(s: String, is_in_word: Boolean): String {
    val sb = StrBuf()
    var in_word = is_in_word
    
    for (ch in s) {
        if (in_word) {
            if (!(ch in word_chars)) {
                in_word = false
            }
            sb.add(ch.lowercase())
        } else if (ch in word_chars) {
            sb.add(ch.uppercase())
            in_word = true
        } else {
            sb.add(ch)
        }
    }
    return sb.toString()
}


/// builtin string-capitalize
/// fun     bi_string_capitalize
/// std     string
/// key     
/// opt     start, end
/// rest    
/// ret     string
/// special no
/// doc {
/// Return `string` with all lowercase chars replaced by uppercase chars.
/// `start` and `end` may specify the region of the string to be treated;
/// defaults are 0 and the end of the string.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string_capitalize(args: LObject, kwArgs: Map<LSymbol, LObject>,
                         suppp: Map<LSymbol, Boolean>): LObject {
    val (string, start, end) = args3(args)
    val s = string.toString()
    val len = s.length

    val (pos1, pos2) = substring_limits(start, end, len)

    if (pos1 >= pos2) {
        return makeString(s)
    }
    try {
        val prefix = s.substring(0, pos1)
        val selected = s.substring(pos1, pos2)
        val suffix = s.substring(pos2, len)
        val is_in_word = pos1 > 0 && prefix[pos1 - 1] in word_chars
        return makeString(prefix
                          + capitalize(selected, is_in_word)
                          + suffix)
    } catch (e: java.lang.StringIndexOutOfBoundsException) {
        // cannot happen
        throw IndexError("range [$pos1, $pos2) out of bounds for "
                         + "length $len in string-capitalize")
    }
}


fun string_trim(s: String, pred: (Char) -> Boolean,
                left: Boolean, right: Boolean): String {
    val len = s.length
    var leftstart = 0
    var rightend = len

    if (left) {
        for (i in 0..len) {
            if (i == len) {
                return ""
            }
            if (!pred(s[i])) {
                leftstart = i
                break
            }
        }
    }
    if (right) {
        for (i in len - 1 downTo -1) {
            if (i == -1) {
                return ""
            }
            if (!pred(s[i])) {
                rightend = i + 1
                break
            }
        }
    }
    return s.substring(leftstart, rightend)
}

/// builtin string-trim
/// fun     bi_string_trim
/// std     char-bag string
/// key     
/// opt     
/// rest    
/// ret     trimmed-string
/// special no
/// doc {
/// Return a substring of `string`, with characters in `char-bag` stripped
/// off the beginning and end. `char-bag` may be t, in which case all
/// whitespace characters will be stripped, or a sequence of characters.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string_trim(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
    var (charbag, string) = args2(args)
    val pred: (it: Char) -> Boolean

    if (charbag === T) {
        pred = { it.isWhitespace() }
    } else {
        val chars = if (charbag is LSeq)
            charbag.elements().joinToString("")
        else
            charbag.toString()
        pred = { it in chars }
    }
    return makeString(string_trim(string.toString(), pred, true, true))
}

/// builtin string-left-trim
/// fun     bi_string_left_trim
/// std     char-bag string
/// key     
/// opt     
/// rest    
/// ret     trimmed-string
/// special no
/// doc {
/// Return a substring of `string`, with characters in `char-bag` stripped
/// off the beginning. `char-bag` may be t, in which case all whitespace
/// characters will be stripped, or a sequence of characters.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string_left_trim(args: LObject, kwArgs: Map<LSymbol, LObject>,
                        suppp: Map<LSymbol, Boolean>): LObject {
    var (charbag, string) = args2(args)
    val pred: (it: Char) -> Boolean

    if (charbag === T) {
        pred = { it.isWhitespace() }
    } else {
        val chars = charbag.toString()
        pred = { it in chars }
    }
    return makeString(string_trim(string.toString(), pred, true, false))
}

/// builtin string-right-trim
/// fun     bi_string_right_trim
/// std     char-bag string
/// key     
/// opt     
/// rest    
/// ret     trimmed-string
/// special no
/// doc {
/// Return a substring of `string`, with characters in `char-bag` stripped
/// off the end. `char-bag` may be t, in which case all whitespace characters
/// will be stripped, or a sequence of characters.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string_right_trim(args: LObject, kwArgs: Map<LSymbol, LObject>,
                         suppp: Map<LSymbol, Boolean>): LObject
{
    var (charbag, string) = args2(args)
    val pred: (it: Char) -> Boolean

    if (charbag === T) {
        pred = { it.isWhitespace() }
    } else {
        val chars = charbag.toString()
        pred = { it in chars }
    }
    return makeString(string_trim(string.toString(), pred, false, true))
}

/// builtin substring
/// fun     bi_substring
/// std     string start
/// key     
/// opt     end
/// rest    
/// ret     string
/// special no
/// doc {
/// Return a substring of `string`, bounded by indices `start` and `end`
/// (or the end of the original string).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_substring(args: LObject, kwArgs: Map<LSymbol, LObject>,
                 suppp: Map<LSymbol, Boolean>): LObject {
    val (string, start, end) = args3(args)
    val s = string.toString()
    val len = s.length
    val n_start = min(len, intArg(start, " start"))
    val n_end = if (end === Nil)
        len
    else
        min(len, intArg(end, " end"))

    if (n_start < 0) {
        throw IndexError("substring start index is negative: $n_start")
    }
    if (n_end < 0) {
        throw IndexError("substring end index is negative: $n_end")
    }
    return makeString(s.substring(n_start, n_end))
}

/// builtin string-contains-p
/// fun     bi_string_contains_p
/// std     haystack needle
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if string `haystack` contains `needle`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string_contains_p(args: LObject, kwArgs: Map<LSymbol, LObject>,
                         suppp: Map<LSymbol, Boolean>): LObject {
    val (haystack, needle) = args2(args)
    return bool2ob(needle.toString() in haystack.toString())
}

/// builtin string-starts-with
/// fun     bi_string_starts_with
/// std     string prefix
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return true iff `string` starts with `prefix`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string_starts_with(args: LObject, kwArgs: Map<LSymbol, LObject>,
                          suppp: Map<LSymbol, Boolean>): LObject {
    val (str, prefix) = args2(args)
    return bool2ob(str.toString().startsWith(prefix.toString()))
}

/// builtin string-ends-with
/// fun     bi_string_ends_with
/// std     string prefix
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return true iff `string` ends with `prefix`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string_ends_with(args: LObject, kwArgs: Map<LSymbol, LObject>,
                          suppp: Map<LSymbol, Boolean>): LObject {
    val (str, prefix) = args2(args)
    return bool2ob(str.toString().endsWith(prefix.toString()))
}

