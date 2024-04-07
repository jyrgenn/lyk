// functions on sequences

package org.w21.lyk


/// builtin elements
/// fun     bi_elements
/// std     sequence
/// key     
/// opt     
/// rest    
/// ret     list
/// special no
/// doc {
/// Return the elements of `sequence` as a list. If the argument is a list,
/// the resulting list is a shallow copy of it.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_elements(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val seq = arg1(args)

    if (seq === Nil) {
        return Nil
    }
    if (seq is LSymbol) {
        throw ArgumentError("elements argument is not a sequence: $seq")
    }
    return collectedList {
        for (elem in seq) {
            it.add(elem)
        }
    }
}


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
    val (seq, ind) = args2(args)
    val index = intArg(ind, "elt index")

    return seq.elementAt(index)
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
    seq.setAt(iindex, value)
    return value
}
