// functions on association lists

package org.w21.lyk


fun assoc_iter_elems(alist: LObject,
                     closure: (elem_car: LObject) -> Boolean): LObject {
    if (alist === Nil) {
        return Nil
    }
    if (alist !is LCons) {
        throw ArgumentError("alist argument to $calledFunctionName is not an alist: $alist")
    }
    for (elem in alist) {
        if (elem !is LCons) {
            throw ArgumentError("alist argument to $calledFunctionName is not a proper"
                                + " alist: $alist")
        }
        val result = closure(elem.car)
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
/// Return nil if `item` is not found.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_assoc(args: LObject, kwArgs: Map<LSymbol, LObject>,
             suppp: Map<LSymbol, Boolean>): LObject {
    val (item, alist) = args2(args)

    return assoc_iter_elems(alist) {
        it.equal(item)
    }
}

/// builtin sassoc
/// fun     bi_sassoc
/// std     item alist default
/// key     
/// opt     
/// rest    
/// ret     cons/nil
/// special no
/// doc {
/// Look up `item` in `alist`; return the pair whose car is equal to `item`.
/// If `item` is not in `alist`, return `default`, or if it is a function,
/// call it with no args and return the result.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_sassoc(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
    val (item, alist, default) = args3(args)

    val result = assoc_iter_elems(alist) {
        it.equal(item)
    }
    if (result === Nil) {
        if (default is LFunction) {
            return default.call(Nil)
        }
        return default
    }
    return result
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
fun bi_assq(args: LObject, kwArgs: Map<LSymbol, LObject>,
            suppp: Map<LSymbol, Boolean>): LObject {
    val (item, alist) = args2(args)

    return assoc_iter_elems(alist) {
        it === item
    }
}

/// builtin sassq
/// fun     bi_sassq
/// std     item alist default
/// key     
/// opt     
/// rest    
/// ret     cons/nil
/// special no
/// doc {
/// Look up `item` in `alist`; return the pair whose car is eq to `item`.
/// If `item` is not in `alist`, return `default`, or if it is a function,
/// call it with no args and return the result.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_sassq(args: LObject, kwArgs: Map<LSymbol, LObject>,
             suppp: Map<LSymbol, Boolean>): LObject {
    val (item, alist, default) = args3(args)

    val result = assoc_iter_elems(alist) {
        it === item
    }
    if (result === Nil) {
        if (default is LFunction) {
            return default.call(Nil)
        }
        return default
    }
    return result
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
fun bi_assoc_if(args: LObject, kwArgs: Map<LSymbol, LObject>,
                suppp: Map<LSymbol, Boolean>): LObject {
    val (pred, alist) = args2(args)
    var predicate = functionArg(pred)

    return assoc_iter_elems(alist) {
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
fun bi_assoc_if_not(args: LObject, kwArgs: Map<LSymbol, LObject>,
                    suppp: Map<LSymbol, Boolean>): LObject {
    val (pred, alist) = args2(args)
    var predicate = functionArg(pred)

    return assoc_iter_elems(alist) {
        !ob2bool(predicate.call(LCons(it, Nil)))
    }
}
