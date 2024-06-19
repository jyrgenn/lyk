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


// The thing in the function position of a form may be a symbol with a
// function either in the function slot or as the bound value, a function
// itself (but how does it get there??), or something that evaluates to a
// function. (The latter would include the function bound as a value to the
// symbol, but I'd rather special-case that as the -- I guess -- more common
// case.) If it is neither, we don't have a function, and that is an error.
//
fun evalFun(obj: LObject): LFunction
{
    debug(debugEvalFunSym) {
        "evalFun(${obj.dump()})"
    }
    if (obj is LSymbol) {
        val func = obj.function
        if (func is LFunction) {
            return func
        }
        val value = obj.getValueOptional()
        if (value is LFunction) {
            return value
        }
        throw FunctionError("${obj.obtype} `$obj` not bound to function")
    }
    if (obj is LFunction) {
        return obj
    }
    val last_hope = eval(obj)
    if (last_hope is LFunction) {
        return last_hope
    }
    throw FunctionError("${obj.obtype} `$obj` is not a function")
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
      
