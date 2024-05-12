// the core piece of it all

package org.w21.lyk


// debug frame marker; if there is a second object, it is a return
fun markFrame(level: Int, ob1: LObject, ob2: LObject? = null): String {
    var s = "%3d: ${mulString("| ", level)} %s%s%s"
    var separator = ""
    var ob2Str = ""
    if (ob2 != null) {
        separator = " => "
        ob2Str = ob2.toString()
    }
    return s.format(level, ob1.toString(), separator, ob2Str)
}
    


fun evalProgn(forms: LObject): LObject {
    debug(debugEvalPrognSym) {
         "evalProgn $forms"
    }

    var result: LObject = Nil
    for (form in forms) {
        result = eval(form)
    }
    return result
}


// to allow for a variable with a function *value* (not function!) in the
// function position, we recurse *once* at maximum
fun evalFun(obj: LObject?,
            reclevel: Int = 0,
            original: LObject? = null): LFunction
{
    debug(debugEvalFunSym) {
        "evalFun(${obj?.dump() ?: "nil"}, $reclevel)"
    }
    if (obj != null && reclevel <= 1) {
        if (obj is LFunction) {
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


fun evalArgs(arglist: LObject): LObject {
    val lc = ListCollector()

    for (arg in arglist) {
        lc.add(eval(arg))
    }
    return lc.list
}


fun eval(form: LObject): LObject {
    evalCounter += 1
    abortEval = false
    
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
            exitLyk()
        }
        null
    }
    try {
        if (evalLevel > maxRecursionDepth) {
            throw AbortEvalSignal("max recursion depth reached "
                                  + "($maxRecursionDepth)")
        }
        if (abortEval) {
            throw AbortEvalSignal("external signal")
        }

        var value: LObject
        if (form is LSymbol) {
            value = form.getValue()
        } else if (form is LCons) {
            var (func, args) = form
            val function = evalFun(func)

            if (function is LMacro) {
                val newform = function.expand(args)
                debug (debugMacroSym) {
                    "eval expanded $form => $newform"
                }
                return eval(newform)
            }

            if (!function.isSpecial) {
                args = evalArgs(args)
            }
            debug(debugCallSym) {
                val indent = mulString("| ", evalLevel)
                "${"%3d".format(evalLevel - 1)}: $indent$function $args"
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
                exitLyk()
            }
            null
        }
        return value
    } catch (err: LispError) {
        err.pushFrame(evalLevel, form, currentEnv, lastTopLevelLocation)
        throw err
    } catch (sig: LykSignal) {
        throw sig
    } catch (e: ArithmeticException) {
        throw LispError("integer divide by zero")
    } catch (e: StringIndexOutOfBoundsException) {
        throw IndexError("substring " + e.message)
    } catch (exc: Exception) {
        val err = makeLispError(exc)
        err.pushFrame(evalLevel, form, currentEnv, lastTopLevelLocation)
        throw err
    } finally {
        for (defer in deferList) {
            defer()
        }
    }
}
      
