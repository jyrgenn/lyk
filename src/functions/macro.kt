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
    val env: LEnv,                             // the environment
    location: LString?,                    // where defined
): LFunction(macroName, stdPars, keyPars, optPars, restPar, null, true,
             docBody, location)
{
    override val obtype = "macro"

    override val typeDesc = "macro"
    
    init {
        debug(debugMacroSym) {
            "Macro($macroName) = $bodyForms"
        }
    }

    fun expand(arglist: LObject): LObject {
        return withNewEnvironment(env) {
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
        callCounter++
        throw InternalError("calling $this as a function!")        
    }

    override fun body() = bodyForms
}

fun macroExpandList(form: LCons): Pair<LObject, Boolean> {
    var haveExpanded = false
    var lastcons = form                 // dummy, because it mmust be
                                        // initialised
    var elems: LObject = form

    while (elems is LCons) {
        val (newelem, expanded) = macroExpandFormRecurse(elems.car)
        elems.car = newelem
        if (expanded) {
            haveExpanded = true
        }
        lastcons = elems
        elems = elems.cdr
    }
    if (elems !== Nil) {                // last cdr of improper list
        val (newrest, expanded) = macroExpandFormRecurse(elems)
        lastcons.cdr = newrest
        if (expanded) {
            haveExpanded = true
        }
    }
    return Pair(form, haveExpanded)
}

fun macroExpandFormRecurse(form: LObject): Pair<LObject, Boolean> {
    if (form is LCons) {
        var head = form.car
        if (head is LSymbol) {
            var maybeMacro = head.function
            if (maybeMacro is LMacro) {
                return Pair(maybeMacro.expand(form.cdr), true)
            }
        }
        return macroExpandList(form)
    }
    return Pair(form, false)
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
    debug(debugMacroSym) {
        "finally expanded to $formvar"
    }
    return formvar
}

fun makeMacro(params: LObject,
              body: LObject,
              name: LSymbol? = null,
              location: String?): LMacro {
    return makeLambdaOrMacro(params, body, currentEnv, name, isMacro = true,
                             location = location)
        as LMacro
}
