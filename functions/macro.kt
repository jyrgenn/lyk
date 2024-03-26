// the dreaded Lisp macro

package org.w21.lyk

class LMacro(
    macroName: LSymbol?,                   // present if non anonymous
    stdPars: List<LSymbol>,                // normal parameters
    keyPars: Map<LSymbol, LObject>,        // &key name => default
    optPars: List<Pair<LSymbol, LObject>>, // &optional name, default
    restPar: LSymbol?,                     // &rest parameters
    val bodyForms: LObject,                //
    docBody: LString,                      // docstring sans signature
    location: LString,                     // where defined
): LFunction(macroName, stdPars, keyPars, optPars, restPar, null, true,
             docBody, location)
{
    override val typeDesc = "macro"
    
    init {
        debug(debugMacroSym) {
            "Macro($macroName) = $bodyForms"
        }
    }

    fun expand(arglist: LObject): LObject {
        return withNewEnvironment(currentEnv) {
            debug(debugMacroSym) {
                "expand $this with args $arglist"
            }
            bindPars(arglist, this)
            val result = evalProgn(bodyForms)
            debug(debugMacroSym) {
                "$this expanded to $result"
            }
            result
        }
    }

    override fun call(arglist: LObject): LObject {
        throw InternalError("calling $this as a function!")        
    }

    override fun body() = bodyForms
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
    return Pair(lc.list, haveExpanded)
}

fun macroExpandFormRecurse(form: LObject): Pair<LObject, Boolean> {
    debug(debugMacroSym) {
        "macroExpandFormRecurse $form"
    }
    if (form is LCons) {
        debug(debugMacroSym) {
            "is cons"
        }
        val (head, args) = form
        if (head is LSymbol) {
            debug(debugMacroSym) {
                "head of form is symbol $head"
            }
            val maybeMacro = head.function
            debug(debugMacroSym) {
                "$head is maybeMacro? function $maybeMacro"
            }
            if (maybeMacro != null && maybeMacro is LMacro) {
                val expanded = maybeMacro.expand(args)
                debug(debugMacroSym) {
                    "yes, $maybeMacro is! return $expanded, true"
                }
                return Pair(expanded, true)
            }
        }
        val result = macroExpandList(form)
        debug(debugMacroSym) {
            "not a call form, so expand list and return $result"
        }
        return result
    } else {
        debug(debugMacroSym) {
            "not a cons, nothing to expand"
        }
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
              name: LSymbol? = null,
              location: String): LMacro {
    return makeLambdaOrMacro(params, body, currentEnv, name, isMacro = true,
                             location = location)
        as LMacro
}
