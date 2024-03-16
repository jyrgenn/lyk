// the core piece of it all

package org.w21.lyk

import kotlin.system.exitProcess


// debug frame marker; if there is a second object, it is a return
fun markFrame(level: Int, ob1: LispObject, ob2: LispObject? = null): String {
    var s = "%3d: ${mulString("| ", level)}" + ob1.toString()
    if (ob2 != null) {
        s += " => $ob2"
    }
    return s.format(level)
}
    


fun evalProgn(forms: LispObject): LispObject {
    var result: LispObject = Nil
    debug(debugEvalPrognSym) {
         "evalProgn $forms"
    }

    for (form in forms) {
        result = eval(form)
    }
    return result
}


// to allow for a variable with a function *value* (not function!) in the
// function position, we recurse *once* at maximum
fun evalFun(obj: LispObject?,
            reclevel: Int = 0,
            original: LispObject? = null): Function
{
    debug(debugEvalFunSym) {
        "evalFun(${obj?.dump() ?: "nil"}, $reclevel)"
    }
    if (obj != null && reclevel <= 1) {
        if (obj is Function) {
            return obj
        }
        if (obj is LSymbol) {
            debug(debugEvalFunSym) {
                  "$obj is symbol, function ${obj.function}"
            }
            // recurse to 
            return evalFun(obj.function ?: obj.getValueOptional(),
                           reclevel + 1, obj)
        }
        return evalFun(eval(obj), reclevel+1, obj)
    }
    val present = original ?: obj ?: LSymbol.uninterned("WOT?:")
    throw FunctionError("object `$present` is not a function: "
                        + present.dump())
}


fun evalArgs(arglist: LispObject): LispObject {
    val lc = ListCollector()

    for (arg in arglist) {
        lc.add(eval(arg))
    }
    return lc.list()
}

var evalLevel: Int = 0
var maxEvalLevel: Int = 0
var maxRecursionDepth: Int = 1_000_000_000
var abortEval: Boolean = false
var stepEval: Boolean = false
var evalStack: LispObject = Nil



fun eval(form: LispObject): LispObject {
    evalCounter += 1
    
    val savedLevel: Int = evalLevel
    val deferList = listOf({ evalLevel = savedLevel })
    evalLevel += 1
    if (evalLevel > maxEvalLevel) {
        maxEvalLevel = evalLevel
    }
    
    debug(debugEvalSym) {
        markFrame(evalLevel, form)
    }
    debug(debugStepEval) {
        stderr.print("enter\n")
        stderr.print("${markFrame(evalLevel, form)}\n: ")
        val line = readLine()
        if (line == null) {
            exitProcess(0)
        }
        null
    }
    try {
        if (evalLevel > maxRecursionDepth) {
            throw AbortEvalSignal("max recursion depth reached "
                                  + "($maxRecursionDepth)")
        }
        if (abortEval) {
            throw AbortEvalSignal("eval aborted")
        }

        var value: LispObject
        if (form is LSymbol) {
            value = form.getValue()
        } else if (form is Cons) {
            var (func, args) = form
            val function = evalFun(func)

            if (!function.isSpecial) {
                args = evalArgs(args)
            }
            debug(debugCallSym) {
                val indent = mulString("| ", evalLevel)
                "%3d: $indent$function $args".format(evalLevel - 1)
            }
            value = function.call(args)
            // debug(debugCallSym) {
            //     val indent = mulString("| ", evalLevel)
            //     "%3d: $indent$function $args => $value".format(
            //         evalLevel - 1)
        } else {
            value = form
        }
        debug(debugEvalSym) {
            markFrame(evalLevel, form, value)
        }
        debug(debugStepEval) {
            stderr.print("return\n")
            stderr.print(markFrame(evalLevel, form, value) + "\n: ")
            val line = readLine()
            if (line == null) {
                exitProcess(0)
            }
            null
        }
        return value
    } catch (err: LispError) {
        err.pushFrame(evalLevel, form, currentEnv)
        throw err
    } finally {
        for (defer in deferList) {
            defer()
        }
    }
}
      
