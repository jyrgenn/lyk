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
        throw IndexError("index $index for ${typeOf(seq)} is negative")
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
        throw IndexError("index $index for ${typeOf(seq)} is negative")
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

