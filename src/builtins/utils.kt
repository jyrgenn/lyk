// Utility functions for Lisp

package org.w21.lyk

/// builtin list-collector
/// fun     bi_list_collector
/// std     
/// key     
/// opt     
/// rest    
/// ret     closure
/// special no
/// doc {
/// Create a list collector closure and return it as a function.
/// 
/// The returned function takes an arbitrary number of arguments, which
/// are then added to the end of the list, and returns the list. The
/// normal use case would be to call it a number of times to add items to
/// the list, and then call it once without arguments to return the
/// resulting list. This is more efficient than using append repeatedly.
/// 
/// Example:
///     (let ((lc (list-collector)))
///       (lc 'see)
///       (lc 0 'evil)
///       (lc 'hear \"no\" 'evil)
///       (lc))
///     => (see 0 evil hear \"no\" evil)
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_list_collector(args: LObject, kwArgs: Map<LSymbol, LObject>,
                      suppp: Map<LSymbol, Boolean>): LObject {
    var the_list: LObject = Nil
    var last_cons: LCons? = null

    fun list_collector(args: LObject, kwArgs: Map<LSymbol, LObject>,
                      suppp: Map<LSymbol, Boolean>): LObject {
        for (arg in args) {
            val newcons = LCons(arg, Nil)
            if (last_cons != null) {
                (last_cons as LCons).cdr = newcons
            } else {
                the_list = newcons
            }
            last_cons = newcons
        }
        return the_list
    }

    return LBuiltin("*lc-closure*", ::list_collector,
                   noStd, noKey, noOpt, "args", "the-list", false,
"""
This is a list collector closure. It takes an arbitrary number of arguments,
which are then added to the end of the list, and returns the list. The normal
use case would be to call it a number of times to add items to the list, and
then call it once without arguments to return the resulting list. This is more
efficient than using append repeatedly.

Example:
    (let ((lc (list-collector)))
      (lc 'see)
      (lc 0 'evil)
      (lc 'hear "no" 'evil)
      (lc))
    => (see 0 evil hear "no" evil)
""", lastTopLevelLocation)
}
