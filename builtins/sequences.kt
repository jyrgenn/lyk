// functions on sequences

package org.w21.lyk


/// builtin elt
/// fun     bi_elt
/// std     sequence index
/// key     
/// opt     
/// rest    
/// ret     element
/// special no
/// doc {
/// Return the value of `sequence` (list, vector) element at `index`.
//// Throw an error if the (zero-based) `index` is not in the sequence.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_elt(args: LObject, kwArgs: Map<LSymbol, LObject>,
           suppp: Map<LSymbol, Boolean>): LObject {
    val (seq, index) = args2(args)
    return seqArg(seq).getAt(indexArg(index, " index"))
}

/// builtin setelt
/// fun     bi_setelt
/// std     sequence index value
/// key     
/// opt     
/// rest    
/// ret     element
/// special no
/// doc {
/// Set element `index` of sequence `sequence` to `value`; return `value`.
/// The index is zero-based. It is an error if `index` is negative or
/// greater than the length of the sequence.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_setelt(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
    val (seq, index, value) = args3(args)
    seqArg(seq).setAt(intArg(index, " index"), value)
    return value
}

/// builtin elements
/// fun     bi_elements
/// std     sequence
/// key     
/// opt     
/// rest    
/// ret     elements-list
/// special no
/// doc {
/// Return a list with all elements of `sequence`, in order.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_elements(args: LObject, kwArgs: Map<LSymbol, LObject>,
                suppp: Map<LSymbol, Boolean>): LObject {
    return collectedList {
        for (elem in seqArg(arg1(args))) {
            it.add(elem)
        }
    }
}

/// builtin copy-seq
/// fun     bi_copy_seq
/// std     sequence
/// key     
/// opt     
/// rest    
/// ret     copied-sequence
/// special no
/// doc {
/// Return a copy of `sequence`.
/// The elements of the new sequence are the same as the corresponding
/// elements of the given sequence.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_copy_seq(args: LObject, kwArgs: Map<LSymbol, LObject>,
                suppp: Map<LSymbol, Boolean>): LObject {
    return seqArg(arg1(args)).copy()
}

/// builtin doseq
/// fun     bi_doseq
/// std     control-vars
/// key     
/// opt     
/// rest    bodyforms
/// ret     result
/// special yes
/// doc {
/// Bind `var` to every element of `seq`, execute body and return the result.
/// `control-vars` is a list of (var seq [result-form [start [end]]]])
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_doseq(args: LObject, kwArgs: Map<LSymbol, LObject>,
             suppp: Map<LSymbol, Boolean>): LObject {
    val (cvars, bodyforms) = args
    val (symbol, sequence, resultform, start, end) = args5(cvars)
    val sym = symbolArg(symbol, " var")
    val i_start = intValueOr(start, 0, " start")
    val i_end = intValueOr(end, null, " end")

    val seq = eval(sequence)
    if (seq !is LSeq) {
        throw TypeError(seq, "sequence", "doseq,")
    }
    val subseq = seq.subseq(i_start!!, i_end)
    return withNewEnvironment {
        // if (resultform is LSymbol) {
        //     resultform.setValue(Nil)
        // }
        for (elem in subseq) {
            sym.bind(elem)
            evalProgn(bodyforms)
        }
        sym.bind(Nil)
        eval(resultform)
    }
}

/// builtin subseq
/// fun     bi_subseq
/// std     sequence start
/// key     
/// opt     end
/// rest    
/// ret     subsequence
/// special no
/// doc {
/// Return a copy of the subsequence of `sequence` from `start` to `end`.
/// If `end` is omitted, the end of the sequence is assumed.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_subseq(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
    val (sequence, start, end) = args3(args)
    val i_start = if (start === Nil) 0 else intArg(eval(start), " start")
    val i_end = if (end === Nil) null else intArg(eval(end), " end")
    val seq = seqArg(sequence, " sequence")

    return seq.subseq(i_start, i_end)
}

/// builtin delete
/// fun     bi_delete
/// std     item sequence
/// key     
/// opt     
/// rest    
/// ret     result-sequence
/// special no
/// doc {
/// Return a sequence from which the occurences of `item` have been removed.
/// The sequence may be modified.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_delete(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
    val (item, seq) = args2(args)
    return seqArg(seq, " sequence").delete(item)
}

val from_endKeyw = intern(":from-end")
val testKeyw = intern(":test")
val test_notKeyw = intern(":test-not")
val startKeyw = intern(":start")
val endKeyw = intern(":end")
val keyKeyw = intern(":key")
val equalSym = intern("equal")
val identitySym = intern("identity")

/// builtin find
/// fun     bi_find
/// std     item sequence
/// key     "from-end" to Nil, "test" to Nil, "test-not" to Nil, "start" to numberZero, "end" to Nil, "key" to Nil
/// opt     
/// rest    
/// ret     element
/// special no
/// doc {
/// Return first `item` from `sequence` if it is in `sequence`, or nil.
/// If `from-end` is true, return the last `item` found instead.
/// If `test` is not nil, it is used as a function to check the equality
/// of the elements of the sequence with `item`. Per default, use `equal`.
/// If `test-not` is not nil, it is used as a function to check the
/// non-equality of the elements of the sequence with `item`.
/// Use `start` as the start index of the subsequence to search.
/// Use `end`, if non-nil, as the end index of the subsequence to search.
/// Use `key`, if non-nil, as a function applied to the elements of the
/// sequence before testing against the result.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_find(args: LObject, kwArgs: Map<LSymbol, LObject>,
            suppp: Map<LSymbol, Boolean>): LObject {
    val (item, seq) = args2(args)
    val sequence = seqArg(seq, " sequence")
    val last = kwArgs[from_endKeyw]?.toBoolean() ?: false
    val test_k = kwArgs[testKeyw] ?: Nil
    val test = if (test_k === Nil) {
        equalSym.function ?:
            throw FunctionError("symbol `$equalSym` has no function")
    } else {
        functionArg(test_k, " :test")
    }
    val test_not = if (test_k === Nil) {
        null
    } else {
        functionArg(test_k, " :test-not")
    }
    val start = indexArg(kwArgs[startKeyw] ?: numberZero, " :start")
    val end_k = kwArgs[endKeyw] ?: Nil
    val end = if (end_k === Nil) null else indexArg(end_k, ":end")
    val key_k = kwArgs[keyKeyw] ?: Nil
    val key = if (key_k === Nil) {
        identitySym.function ?:
            throw FunctionError("symbol `$identitySym` has no function")
    } else {
        functionArg(key_k, " :key")
    }

    return sequence.find(start, end, last) {
        val elem = key.call(list(it))
        val testargs = list(elem, item)
        val b1 = test.call(testargs).toBoolean()
        if (test_not == null) {
            b1
        } else {
            b1 && test_not.call(testargs).toBoolean()
        }
    }
}

/// builtin find-if
/// fun     bi_find_if
/// std     predicate sequence
/// key     "from-end" to Nil, "start" to numberZero, "end" to Nil, "key" to Nil
/// opt     
/// rest    
/// ret     element
/// special no
/// doc {
/// Return first element from `sequence` where `predicate` is true, or nil.
/// If `from-end` is true, return the last `item` found instead.
/// Use `start` as the start index of the subsequence to search.
/// Use `end`, if non-nil, as the end index of the subsequence to search.
/// Use `key`, if non-nil, as a function applied to the elements of the
/// sequence before testing against the result.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_find_if(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
    val (pred, seq) = args2(args)
    val predicate = functionArg(pred, " predicate")
    val sequence = seqArg(seq, " sequence")
    val last = kwArgs[from_endKeyw]?.toBoolean() ?: false

    val start = indexArg(kwArgs[startKeyw] ?: numberZero, " :start")
    val end_k = kwArgs[endKeyw] ?: Nil
    val end = if (end_k === Nil) null else indexArg(end_k, ":end")
    val key_k = kwArgs[keyKeyw] ?: Nil
    val key = if (key_k === Nil) {
        identitySym.function ?:
            throw FunctionError("symbol `$identitySym` has no function")
    } else {
        functionArg(key_k, " :key")
    }

    return sequence.find(start, end, last) {
        val elem = key.call(list(it))
        predicate.call(list(elem)).toBoolean()
    }
}

/// builtin find-if-not
/// fun     bi_find_if_not
/// std     predicate sequence
/// key     "from-end" to Nil, "start" to numberZero, "end" to Nil, "key" to Nil
/// opt     
/// rest    
/// ret     element
/// special no
/// doc {
/// Return first element from `sequence` where `predicate` is false, or nil.
/// If `from-end` is true, return the last `item` found instead.
/// Use `start` as the start index of the subsequence to search.
/// Use `end`, if non-nil, as the end index of the subsequence to search.
/// Use `key`, if non-nil, as a function applied to the elements of the
/// sequence before testing against the result.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_find_if_not(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
    val (pred, seq) = args2(args)
    val predicate = functionArg(pred, " predicate")
    val sequence = seqArg(seq, " sequence")
    val last = kwArgs[from_endKeyw]?.toBoolean() ?: false

    val start = indexArg(kwArgs[startKeyw] ?: numberZero, " :start")
    val end_k = kwArgs[endKeyw] ?: Nil
    val end = if (end_k === Nil) null else indexArg(end_k, ":end")
    val key_k = kwArgs[keyKeyw] ?: Nil
    val key = if (key_k === Nil) {
        identitySym.function ?:
            throw FunctionError("symbol `$identitySym` has no function")
    } else {
        functionArg(key_k, " :key")
    }

    return sequence.find(start, end, last) {
        val elem = key.call(list(it))
        !(predicate.call(list(elem)).toBoolean())
    }
}

fun indexOrNil(result: Int): LObject {
    if (result < 0) {
        return Nil
    }
    return makeNumber(result)
}

/// builtin position
/// fun     bi_position
/// std     item sequence
/// key     "from-end" to Nil, "test" to Nil, "test-not" to Nil, "start" to numberZero, "end" to Nil, "key" to Nil
/// opt     
/// rest    
/// ret     element
/// special no
/// doc {
/// Return first index of `item` from `sequence` if it is in `sequence`, or nil.
/// If `from-end` is true, return the last `item` found instead.
/// If `test` is not nil, it is used as a function to check the equality
/// of the elements of the sequence with `item`. Per default, use `equal`.
/// If `test-not` is not nil, it is used as a function to check the
/// non-equality of the elements of the sequence with `item`.
/// Use `start` as the start index of the subsequence to search.
/// Use `end`, if non-nil, as the end index of the subsequence to search.
/// Use `key`, if non-nil, as a function applied to the elements of the
/// sequence before testing against the result.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_position(args: LObject, kwArgs: Map<LSymbol, LObject>,
            suppp: Map<LSymbol, Boolean>): LObject {
    val (item, seq) = args2(args)
    val sequence = seqArg(seq, " sequence")
    val last = kwArgs[from_endKeyw]?.toBoolean() ?: false
    val test_k = kwArgs[testKeyw] ?: Nil
    val test = if (test_k === Nil) {
        equalSym.function ?:
            throw FunctionError("symbol `$equalSym` has no function")
    } else {
        functionArg(test_k, " :test")
    }
    val test_not = if (test_k === Nil) {
        null
    } else {
        functionArg(test_k, " :test-not")
    }
    val start = indexArg(kwArgs[startKeyw] ?: numberZero, " :start")
    val end_k = kwArgs[endKeyw] ?: Nil
    val end = if (end_k === Nil) null else indexArg(end_k, ":end")
    val key_k = kwArgs[keyKeyw] ?: Nil
    val key = if (key_k === Nil) {
        identitySym.function ?:
            throw FunctionError("symbol `$identitySym` has no function")
    } else {
        functionArg(key_k, " :key")
    }

    return indexOrNil(sequence.position(start, end, last) {
                          val elem = key.call(list(it))
                          val testargs = list(elem, item)
                          val b1 = test.call(testargs).toBoolean()
                          if (test_not == null) {
                              b1
                          } else {
                              b1 && test_not.call(testargs).toBoolean()
                          }
                      })
}

/// builtin position-if
/// fun     bi_position_if
/// std     predicate sequence
/// key     "from-end" to Nil, "start" to numberZero, "end" to Nil, "key" to Nil
/// opt     
/// rest    
/// ret     element
/// special no
/// doc {
/// Return first element from `sequence` where `predicate` is true, or nil.
/// If `from-end` is true, return the last `item` found instead.
/// Use `start` as the start index of the subsequence to search.
/// Use `end`, if non-nil, as the end index of the subsequence to search.
/// Use `key`, if non-nil, as a function applied to the elements of the
/// sequence before testing against the result.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_position_if(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
    val (pred, seq) = args2(args)
    val predicate = functionArg(pred, " predicate")
    val sequence = seqArg(seq, " sequence")
    val last = kwArgs[from_endKeyw]?.toBoolean() ?: false

    val start = indexArg(kwArgs[startKeyw] ?: numberZero, " :start")
    val end_k = kwArgs[endKeyw] ?: Nil
    val end = if (end_k === Nil) null else indexArg(end_k, ":end")
    val key_k = kwArgs[keyKeyw] ?: Nil
    val key = if (key_k === Nil) {
        identitySym.function ?:
            throw FunctionError("symbol `$identitySym` has no function")
    } else {
        functionArg(key_k, " :key")
    }

    return indexOrNil(sequence.position(start, end, last) {
                          val elem = key.call(list(it))
                          predicate.call(list(elem)).toBoolean()
                      })
}

/// builtin position-if-not
/// fun     bi_position_if_not
/// std     predicate sequence
/// key     "from-end" to Nil, "start" to numberZero, "end" to Nil, "key" to Nil
/// opt     
/// rest    
/// ret     element
/// special no
/// doc {
/// Return first element from `sequence` where `predicate` is false, or nil.
/// If `from-end` is true, return the last `item` found instead.
/// Use `start` as the start index of the subsequence to search.
/// Use `end`, if non-nil, as the end index of the subsequence to search.
/// Use `key`, if non-nil, as a function applied to the elements of the
/// sequence before testing against the result.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_position_if_not(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
    val (pred, seq) = args2(args)
    val predicate = functionArg(pred, " predicate")
    val sequence = seqArg(seq, " sequence")
    val last = kwArgs[from_endKeyw]?.toBoolean() ?: false

    val start = indexArg(kwArgs[startKeyw] ?: numberZero, " :start")
    val end_k = kwArgs[endKeyw] ?: Nil
    val end = if (end_k === Nil) null else indexArg(end_k, ":end")
    val key_k = kwArgs[keyKeyw] ?: Nil
    val key = if (key_k === Nil) {
        identitySym.function ?:
            throw FunctionError("symbol `$identitySym` has no function")
    } else {
        functionArg(key_k, " :key")
    }

    return indexOrNil(sequence.position(start, end, last) {
                          val elem = key.call(list(it))
                          !(predicate.call(list(elem)).toBoolean())
                      })
}

