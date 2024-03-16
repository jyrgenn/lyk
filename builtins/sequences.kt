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
fun bi_elements(args: LispObject, kwArgs: Map<LSymbol, LispObject>
): LispObject {
    val seq = arg1(args)

    if (seq === Nil) {
        return Nil
    }
    if (seq is LSymbol) {
        throw ArgumentError("elements argument is not a sequence: $seq")
    }
    return collectedList() {
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
fun bi_elt(args: LispObject, kwArgs: Map<LSymbol, LispObject>
): LispObject {
    val (seq, ind) = args2(args)
    val index = longArg(ind, "elt index")

    return seq.elementAt(index.toInt())
}
