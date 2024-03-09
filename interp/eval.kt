// the core piece of it all

package org.w21.lyk


// var breakSymbol = Symbol.uninterned("*break-eval*")

fun evalProgn(forms: LispObject): LispObject {
    var result: LispObject = Nil
    debug(traceEvalSym, "evalProgn $forms")

    for (form in forms) {
        result = eval(form)
    }
    return result
}

fun evalFun(obj: LispObject?,
            reclevel: Int = 0,
            show: LispObject? = null): Function
{
    debug(traceEvalFunSym, "evalFun(${obj?.dump() ?: "nil"}, $reclevel)")
    if (obj != null && reclevel <= 2) {
        // dump(obj)
        if (obj is Function) {
            return obj
        }
        if (obj is Symbol) {
            obj.dump()
            debug(traceEvalSym, "$obj is symbol, function ${obj.function}")
            return evalFun(obj.function ?: obj.getValueOptional(),
                           reclevel + 1, show ?: obj)
        }
        return evalFun(eval(obj), reclevel+1, show ?: obj)
    }
    val present = show ?: obj ?: Symbol.uninterned("WOT?:")
    throw FunctionError("object `$present` is not a function "
                        + present.dump())
}


fun evalArgs(arglist: LispObject): LispObject {
    val lc = ListCollector()

    for (arg in arglist) {
        lc.add(eval(arg))
    }
    return lc.list()
}

var current_eval_level: Int = 0
var maxEvalLevel: Int = 0
var maxRecursionDepth: Int = 1_000_000_000
var abortEval: Boolean = false
var stepEval: Boolean = false
var evalStack: LispObject = Nil



fun eval(form: LispObject /* , expandMacros: Boolean = false */): LispObject {
    evalCounter += 1
    val savedLevel: Int = current_eval_level
    val deferList = listOf({ current_eval_level = savedLevel })
    
    debug(traceEvalSym, "eval $form, ${typeOf(form)}")
    try {
        current_eval_level += 1
        debug(traceEvalSym) {
            println("TRC eval[$current_eval_level] $form")
        }
        if (current_eval_level > maxEvalLevel) {
            maxEvalLevel = current_eval_level
        }
        if (current_eval_level > maxRecursionDepth) {
            throw AbortEvalSignal("max recursion depth reached "
                                  + "($maxRecursionDepth)")
        }
        // if expandMacros {
        //     form = macroexpandForm(form)
        // }
        // break_if(breakSymbol, "eval[{}]: {:r}", current_eval_level, form)
        try {
            if (abortEval) {
                throw AbortEvalSignal("eval aborted")
            }
            // if (stepEval) {
            //     var done = false
            //     stdout.println("\n; eval[$current_eval_level] $form")
            //     while (!done) {
            //         stdout.print("**step eval :o off; :x exit; eval or [step]: ")
            //         val answer = Reader(StringStream(stdin.readline()
            //                                                     ?: ""),
            //                             "<stdin>").read()
            //         if val ob = answer {
            //             switch ob {
            //                 case intern(":o"):
            //                     stepEval = false
            //                 done = true
            //                 case intern(":x"):
            //                     print("abort")
            //                 throw AbortEvalSignal("abort step eval")
            //                 default:
            //                     try { () -> Void in
            //                                     val savedStepEval = stepEval
            //                                 deferList.add({
            //                                                   stepEval = savedStepEval })
            //                                 stepEval = false
            //                                 print(" => ", eval(ob))
            //                     }()
            //             }
            //         } else {
            //             done = true
            //         }
            //     }
            // }

            var value: LispObject
            if (form is Symbol) {
                value = form.getValue()
            } else if (form is Cons) {
                var func = form.car()
                var args = form.cdr()
                val function = evalFun(func)
                // print("function is", function)
                if (!function.isSpecial) {
                    args = evalArgs(args)
                }
                debug(traceCallSym, function, args)
                value = function.call(args)
            } else {
                value = form
            }
            if (stepEval) {
                print("[$current_eval_level] => $value")
            }
            debug(traceEvalSym) {
                println("TRC eval[$current_eval_level] $form => $value")
            }
            return value
        } catch (err: LispError) {
            err.pushFrame(current_eval_level, form, currentEnv)
            throw err
        }
    } finally {
        for (defer in deferList) {
            defer()
        }
    }
}
      
