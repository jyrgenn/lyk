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
fun bi_elt(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (seq, index) = args2(args)
    val iindex = intArg(index, "elt index")

    if (iindex < 0) {
        throw IndexError("index $index for ${seq.type} is negative")
    }
    return seqArg(seq, "elt").getAt(iindex)
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
fun bi_setelt(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (seq, index, value) = args3(args)
    val iindex = intArg(index, "setelt index")

    if (iindex < 0) {
        throw IndexError("index $index for ${seq.type} is negative")
    }
    seqArg(seq, "setelt").setAt(iindex, value)
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
fun bi_elements(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return collectedList {
        for (elem in seqArg(arg1(args), "elements")) {
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
fun bi_copy_seq(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return seqArg(arg1(args), "copy-seq").copy()
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
fun bi_doseq(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (cvars, bodyforms) = args
    val (symbol, sequence, resultform, start, end) = args5(cvars)
    val sym = symbolArg(symbol, "doseq var")
    val i_start = intValueOr(start, 0, "doseq start")
    val i_end = intValueOr(end, null, "doseq end")

    val seq = eval(sequence)
    if (seq !is LSeq) {
        throw TypeError(seq, "sequence")
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
fun bi_subseq(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (sequence, start, end) = args3(args)
    val i_start = if (start === Nil) 0 else intArg(eval(start), "subseq start")
    val i_end = if (start === Nil) null else intArg(eval(end), "subseq end")
    val seq = seqArg(sequence, "subseq sequence")

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
fun bi_delete(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (item, seq) = args2(args)
    return seqArg(seq, "delete sequence").delete(item)
}
