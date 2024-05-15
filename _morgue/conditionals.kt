/// builtin if
/// fun     bi_if
/// std     condition then-clause
/// key     
/// opt     
/// rest    else-clauses
/// ret     value
/// special yes
/// doc {
/// If `condition` evals to non-nil, eval `then-clause` and return the value.
/// Otherwise, evaluate `else-clauses` and return the last value.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_if(args: LObject, kwArgs: Map<LSymbol, LObject>,
          suppp: Map<LSymbol, Boolean>): LObject {
    val (condition, rest) = args
    val (when_clause, else_clauses) = rest
    if (eval(condition) !== Nil) {
        return eval(when_clause)
    }
    return evalProgn(else_clauses)
}

/// builtin when
/// fun     bi_when
/// std     condition
/// key     
/// opt     
/// rest    then-clauses
/// ret     value
/// special yes
/// doc {
/// If `condition` evaluates to non-nil, eval all `then-clauses` and return
/// the value of the last. Otherwise return nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_when(args: LObject, kwArgs: Map<LSymbol, LObject>,
            suppp: Map<LSymbol, Boolean>): LObject {
    val (condition, when_clauses) = args
    if (eval(condition) !== Nil) {
        return evalProgn(when_clauses)
    }
    return Nil
}

/// builtin unless
/// fun     bi_unless
/// std     condition
/// key     
/// opt     
/// rest    else-clauses
/// ret     value
/// special yes
/// doc {
/// If `condition` evaluates to nil, eval all `else-clauses` and return
/// the value of the last. Otherwise, return nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_unless(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
    val (condition, else_clauses) = args
    if (eval(condition) === Nil) {
        return evalProgn(else_clauses)
    }
    return Nil
}

