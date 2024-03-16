// functions on association lists

package org.w21.lyk


fun assoc_check_function(pred: LObject, what: String): Function {
    if (pred is Function) {
        return pred
    }
    throw ArgumentError("predicate arg to $what is not a function: $pred")
}

fun assoc_iter_elems(alist: LObject, what: String,
                     closure: (elem_car: LObject) -> Boolean): LObject {
    if (alist === Nil) {
        return Nil
    }
    if (alist !is LCons) {
        throw ArgumentError("alist argument to $what is not a alist: $alist")
    }
    for (elem in alist) {
        if (elem !is LCons) {
            throw ArgumentError("alist argument to $what is not a proper"
                                + " alist: $alist")
        }
        val result = closure(elem.car())
        if (result) {
            return elem
        }
    }
    return Nil
}

/// builtin assoc
/// fun     bi_assoc
/// std     item alist
/// key     
/// opt     
/// rest    
/// ret     cons/nil
/// special no
/// doc {
/// Look up `item` in `alist` and return the pair whose car is equal to `item`.
/// Return nil if `item` is not found as a car in one of the pairs in `alist`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_assoc(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (item, alist) = args2(args)

    return assoc_iter_elems(alist, "assoc") {
        it.equal(item)
    }
}

/// builtin assq
/// fun     bi_assq
/// std     item alist
/// key     
/// opt     
/// rest    
/// ret     cons/nil
/// special no
/// doc {
/// Look up `item` in `alist` and return the pair whose car is eq to `item`.
/// Return nil if `item` is not found as a car in one of the pairs in `alist`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_assq(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (item, alist) = args2(args)

    return assoc_iter_elems(alist, "assq") {
        it === item
    }
}

/// builtin assoc-if
/// fun     bi_assoc_if
/// std     predicate alist
/// key     
/// opt     
/// rest    
/// ret     cons/nil
/// special no
/// doc {
/// Return the first cons in `alist` for whose car `predicate` is true.
/// If there is no such cons, return nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_assoc_if(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (pred, alist) = args2(args)
    var predicate = assoc_check_function(pred, "assoc-if")

    return assoc_iter_elems(alist, "assoc-if") {
        ob2bool(predicate.call(LCons(it, Nil)))
    }
}

/// builtin assoc-if-not
/// fun     bi_assoc_if_not
/// std     predicate alist
/// key     
/// opt     
/// rest    
/// ret     cons/nil
/// special no
/// doc {
/// Return the first cons in `alist` for whose car `predicate` is false.
/// If there is no such cons, return nil.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_assoc_if_not(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    val (pred, alist) = args2(args)
    var predicate = assoc_check_function(pred, "assoc-if-not")

    return assoc_iter_elems(alist, "assoc-if-not") {
        !ob2bool(predicate.call(LCons(it, Nil)))
    }
}
