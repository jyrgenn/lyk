// vector functions

package org.w21.lyk


/// builtin aref
/// fun     bi_aref
/// std     vector index
/// key     
/// opt     
/// rest    
/// ret     element
/// special no
/// doc {
/// Return the element of `vector` at `index`.
/// }
/// end builtin
/// builtin vector-get
/// fun     bi_aref
/// std     vector index
/// key     
/// opt     
/// rest    
/// ret     element
/// special no
/// doc {
/// Return the element of `vector` at `index`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_aref(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (vector, index) = args2(args)
    return vectorArg(vector, "aref vector")
        .getAt(longArg(index, "aref index").toInt())
}

/// builtin make-vector
/// fun     bi_make_vector
/// std     length
/// key     
/// opt     initial
/// rest    
/// ret     vector
/// special no
/// doc {
/// Return a new vector of length `length` and each element set to `initial`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_make_vector(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (len, initial) = args2(args)
    val length = intArg(len, "make-vector length")
    if (length < 0) {
        throw ValueError("make-vector length argument < 0: $length")
    }

    if (initial is LFunction) {
        val lc = ListCollector()
        for (i in 0..<length) {
            lc.add(initial.call(Nil))
        }
        return LVector(lc.list)
    }
    return LVector(length, initial)
}

/// builtin vector-set
/// fun     bi_vector_set
/// std     vector index value
/// key     
/// opt     
/// rest    
/// ret     value
/// special no
/// doc {
/// Set the element of `vector` at `index` to `value`; return `value`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_vector_set(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (vector, index, value) = args3(args)
    vectorArg(vector, "aref vector")
        .setAt(intArg(index, "vector-set index"), value)
    return value
}

/// builtin vector
/// fun     bi_vector
/// std     
/// key     
/// opt     
/// rest    elements
/// ret     vector
/// special no
/// doc {
/// 
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_vector(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    return LVector(args)
}
