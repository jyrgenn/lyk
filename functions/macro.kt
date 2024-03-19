// the dreaded Lisp macro

package org.w21.lyk

class LMacro(
    macroName: LSymbol?,                      // present if non anonymous
    stdPars: List<LSymbol>,                   // normal parameters
    keyPars: Map<LSymbol, LObject>,        // &key name => default
    optPars: List<Pair<LSymbol, LObject>>, // &optional name, default
    restPar: LSymbol?,                        // &rest parameters
    val bodyForms: LObject,                   //
    docBody: LString,                     // docstring sans signature
): LFunction(macroName, stdPars, keyPars, optPars, restPar, null, true,
             docBody)
{
    override val typeDesc = "macro"
    
    fun expand(arglist: LObject): LObject {
        return withNewEnvironment(currentEnv) {
            bindPars(arglist, this)
            evalProgn(bodyForms)
        }
    }

    override fun equal(other: LObject) = this === other
}

fun macroExpandList(form: LObject): Pair<LObject, Boolean> {
    var haveExpanded = false
    val lc = ListCollector()

    var elems = form
    while (elems is LCons) {
        val elem = elems.car
        val (newelem, expanded) = macroExpandFormRecurse(elem)
        lc.add(newelem)
        haveExpanded = haveExpanded || expanded
        elems = elems.cdr
    }
    if (elems !== Nil) {
        val (newrest, expanded) = macroExpandFormRecurse(elems)
        lc.lastcdr(newrest)
        haveExpanded = haveExpanded || expanded
    }
    return Pair(lc.list(), haveExpanded)
}

fun macroExpandFormRecurse(form: LObject): Pair<LObject, Boolean> {
    if (form is LCons) {
        val (head, args) = form
        if (head is LSymbol) {
            val maybeMacro = head.function
            if (maybeMacro is LMacro) {
                return Pair(maybeMacro.expand(args), true)
            }
        }
        return macroExpandList(form)
    } else {
        return Pair(form, false)
    }
}

fun macroExpandForm(form: LObject): LObject {
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

fun makeMacro(params: LObject,
              body: LObject,
              name: LSymbol? = null): LMacro {
    return makeLambdaOrMacro(params, body, currentEnv, name, isMacro = true)
        as LMacro
}
