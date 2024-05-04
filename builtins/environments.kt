// builtins concerned with environments

package org.w21.lyk

/// builtin new-environment
/// fun     bi_new_environment
/// std     
/// key     
/// opt     parent-environment T, value-table
/// rest    
/// ret     environment
/// special no
/// doc {
/// Return a new environment. Optional `parent-environment` is the parent,
/// otherwise the current environment. If `parent-environment` is nil, there
/// is no parent environment, i.e. a top-level environment is created; if
/// `parent-environmenta is t (the default), the parent is the current
/// environment. If `value-table` is non-nil, it is a table with symbol/value
/// pairs to populate the new environment.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")

fun bi_new_environment(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (parent, value_table) = args2(args)
    
    val parent_env = when (parent) {
        is LEnv -> parent
        T -> currentEnv
        Nil -> null
        else ->
            throw ArgumentError("`parent-environment` argument is not t or nil"
                                + "or an environment: $parent")
    }
    
    val env = LEnv(parent_env)
    if (value_table !== Nil) {
        val vtable = tableArg(value_table, " value-table")

        for ((sym, value) in vtable.items()) {
            if (sym is LSymbol) {
                env.bind(sym, value)
            } else {
                throw ArgumentError("`value-table` argument has non-symbol"
                                      + "key: $sym")
            }
        }
    }
    return env
}

/// builtin the-environment
/// fun     bi_the_environment
/// std     
/// key     
/// opt     
/// rest    
/// ret     environment
/// special no
/// doc {
/// Return the current environment.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_the_environment(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return currentEnv
}

/// builtin with-environment
/// fun     bi_with_environment
/// std     env
/// key     
/// opt     
/// rest    bodyforms
/// ret     value
/// special yes
/// doc {
/// Eval `bodyforms` in environment `env` and return the last value.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_with_environment(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (env_arg, bodyforms) = args

    val savedEnv = currentEnv
    try {
        currentEnv = environmentArg(eval(env_arg), " env")
        return evalProgn(bodyforms)
    } finally {
        currentEnv = savedEnv
    }
}

/// builtin root-environment
/// fun     bi_root_environment
/// std     
/// key     
/// opt     
/// rest    
/// ret     root-environment
/// special no
/// doc {
/// Return the root environment.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_root_environment(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return rootEnv
}

