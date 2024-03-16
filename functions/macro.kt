// the dreaded Lisp macro

package org.w21.lyk

class Macro(
    macroName: LSymbol?,                      // present if non anonymous
    stdPars: List<LSymbol>,                   // normal parameters
    keyPars: Map<LSymbol, LispObject>,        // &key name => default
    optPars: List<Pair<LSymbol, LispObject>>, // &optional name, default
    restPar: LSymbol?,                        // &rest parameters
    bodyForms: LispObject,                   //
    docBody: LispString,                     // docstring sans signature
): Lambda(macroName, stdPars, keyPars, optPars, restPar, bodyForms,
          docBody, currentEnv, isSpecial = true)
{
    fun expand(arglist: LispObject) = call(arglist)
}

fun macroExpandList(form: LispObject): Pair<LispObject, Boolean> {
    var haveExpanded = false
    val lc = ListCollector()

    var elems = form
    while (elems is Cons) {
        val elem = elems.car()
        val (newelem, expanded) = macroExpandFormRecurse(elem)
        lc.add(newelem)
        haveExpanded = haveExpanded || expanded
        elems = elems.cdr()
    }
    if (elems !== Nil) {
        val (newrest, expanded) = macroExpandFormRecurse(elems)
        lc.lastcdr(newrest)
        haveExpanded = haveExpanded || expanded
    }
    return Pair(lc.list(), haveExpanded)
}

fun macroExpandFormRecurse(form: LispObject): Pair<LispObject, Boolean> {
    if (form is Cons) {
        val (head, args) = form
        if (head is LSymbol) {
            val maybeMacro = head.function
            if (maybeMacro is Macro) {
                return Pair(maybeMacro.expand(args), true)
            }
        }
        return macroExpandList(form)
    } else {
        return Pair(form, false)
    }
}

fun macroExpandForm(form: LispObject): LispObject {
    var needExpansion = true
    var formvar = form
    while (needExpansion) {
        debug(debugMacroSym) {
            "need expand $formvar"
        }
        val (newForm, stillNeedExpansion) = macroExpandFormRecurse(formvar)
        formvar = newForm
        needExpansion = stillNeedExpansion
    }
    return formvar
}

fun makeMacro(params: LispObject,
              body: LispObject,
              name: LSymbol? = null): Macro {
    return makeLambda(params, body, currentEnv, name, isMacro = true)
        as Macro
}
