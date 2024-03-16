// table functions

package org.w21.lyk


/// builtin table-get
/// fun     bi_table_get
/// std     table key
/// key     
/// opt     default
/// rest    
/// ret     value
/// special no
/// doc {
/// Return value associated in `table` with `key`.
/// If `key` is not present, return `default` (which defaults to nil).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_table_get(args: LispObject, kwArgs: Map<LSymbol, LispObject>
): LispObject {
    val (tbl, key, defaultvalue) = args3(args)
    val table = tableArg(tbl, "table-get table")
    return table.get(key, defaultvalue)
}

/// builtin table-put
/// fun     bi_table_put
/// std     table key value
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Make `table` associate `key` with `value`, return `value`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_table_put(args: LispObject, kwArgs: Map<LSymbol, LispObject>
): LispObject {
    val (tbl, key, value) = args3(args)
    val table = tableArg(tbl, "table-get table")
    table.put(key, value)
    return value
}

/// builtin make-table
/// fun     bi_make_table
/// std     
/// key     
/// opt     
/// rest    
/// ret     table
/// special no
/// doc {
/// Return a new, empty key-value table.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_make_table(args: LispObject, kwArgs: Map<LSymbol, LispObject>
): LispObject {
    return Table(Nil)
}

/// builtin table-count
/// fun     bi_table_count
/// std     table
/// key     
/// opt     
/// rest    
/// ret     count
/// special no
/// doc {
/// Return the number of key-value pairs in `table`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_table_count(args: LispObject, kwArgs: Map<LSymbol, LispObject>
): LispObject {
    val table = arg1(args)
    return makeNumber(tableArg(table, "table-count").the_table.size)
}

/// builtin table-exists
/// fun     bi_table_exists
/// std     table key
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `key` exists in `table`, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_table_exists(args: LispObject, kwArgs: Map<LSymbol, LispObject>
): LispObject {
    val (table, key) = args2(args)
    return tableArg(table, "table-exists").exists(key)
}

val initialKeySym = intern("initial")
val createKeySym = intern("create")

/// builtin table-inc
/// fun     bi_table_inc
/// std     table key
/// key     "create" to Nil, "initial" to makeNumber(0)
/// opt     increment makeNumber(1)
/// rest    
/// ret     value
/// special no
/// doc {
/// Increment (and return) the numeric value for `key` in `table` by `increment`.
/// If keyword argument `create` is non-nil and `key` does not exist in table,
/// create the key with the value `initial` before incrementing. Otherwise, it
/// is an error if `key` does not exists in `table`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_table_inc(args: LispObject, kwArgs: Map<LSymbol, LispObject>
): LispObject {
    val (tbl, key, inc) = args3(args)
    val table = tableArg(tbl, "table-inc table")
    val increment = numberArg(inc, "table-inc increment")

    val value = table.the_table[key]
    if (value != null) {
        if (value !is LNumber) {
            throw TypeError("key value $value in table $table not a number")
        }
        val new_value = makeNumber(value.value + increment)
        table.put(key, new_value)
        return new_value
    } else if (kwArgs[createKeySym] !== Nil) {
        val initial = numberArg(kwArgs[initialKeySym] ?: Nil,
                                "table-inc initial")
        val new_value = makeNumber(initial + increment)
        table.put(key, new_value)
        return new_value
    } else {
        throw ValueError("key $key does not exist in table $table")
    }
}

/// builtin table-keys
/// fun     bi_table_keys
/// std     table
/// key     
/// opt     
/// rest    
/// ret     keys
/// special no
/// doc {
/// Return a list of all keys in `table`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_table_keys(args: LispObject, kwArgs: Map<LSymbol, LispObject>
): LispObject {
    return tableArg(arg1(args), "table-keys table").keys()
    
}

/// builtin table-values
/// fun     bi_table_values
/// std     table 
/// key     
/// opt     
/// rest    
/// ret     values
/// special no
/// doc {
/// Return a list with all values in `table`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_table_values(args: LispObject, kwArgs: Map<LSymbol, LispObject>
): LispObject {
    return tableArg(arg1(args), "table-values table").values()
}

/// builtin table-pairs
/// fun     bi_table_pairs
/// std     table
/// key     
/// opt     
/// rest    
/// ret     pairs
/// special no
/// doc {
/// Return a list with all (key . value) pairs in `table`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_table_pairs(args: LispObject, kwArgs: Map<LSymbol, LispObject>
): LispObject {
    return tableArg(arg1(args), "table-pairs table").items()
    
}

/// builtin table-remove
/// fun     bi_table_remove
/// std     table key
/// key     
/// opt     
/// rest    
/// ret     table
/// special no
/// doc {
/// Remove `key` from `table` and return `table`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_table_remove(args: LispObject, kwArgs: Map<LSymbol, LispObject>
): LispObject {
    val (tbl, key) = args2(args)
    val table = tableArg(tbl, "table-remove table")

    table.remove(key)
    return table
}
