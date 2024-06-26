// the lambda function

package org.w21.lyk


open class Lambda(                         // Macro will inherit this
    functionName: LSymbol?,                // present if non anonymous
    stdPars: List<LSymbol>,                // normal parameters
    keyPars: Map<LSymbol, LObject>,        // &key name => default
    optPars: List<Pair<LSymbol, LObject>>, // &optional name, default
    restPar: LSymbol?,                     // &rest parameters
    val bodyForms: LObject,                //
    docBody: LString,                      // docstring sans signature
    val environment: LEnv,                 // environment of function
    location: LString?,                    // where defined
): LFunction(functionName, stdPars, keyPars, optPars, restPar,
             intern("value"), false, docBody, location)
{
    override val obtype = "lambda"

    override val typeDesc = "lambda function"
    
    override fun desc(seen: Set<Int>?): String {
        var body: String

        if (bodyForms === Nil) {
            body = Nil.toString()
        } else {
            val s = bodyForms.toString()
            body = s.substring(1, s.length - 1)
        }
        
        return "#<Lambda[$id](${parlist()}) $body>"
    }


    override open fun toString() = definition().toString()
    
    override fun call(arglist: LObject): LObject {
        callCounter++
        return with_called_function_name_return(name.name) {
            withNewEnvironment(environment) {
                bindPars(arglist, this)
                debug(debugLambdaParamsSym) {
                    currentEnv.desc(null) + "\nin " + bodyForms.toString()
                }
                evalProgn(bodyForms)
            }
        }
    }

    override fun body() = bodyForms
}


