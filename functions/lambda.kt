// the lambda function

package org.w21.lyk

val optionalPSym = intern("&optional")
val keyPSym = intern("&key")
val restPSym = intern("&rest")
val emptyString = makeString("")


open class Lambda(                           // Macro will inherit this
    functionName: LSymbol?,                   // present if non anonymous
    stdPars: List<LSymbol>,                   // normal parameters
    keyPars: Map<LSymbol, LObject>,        // &key name => default
    optPars: List<Pair<LSymbol, LObject>>, // &optional name, default
    restPar: LSymbol?,                       // &rest parameters
    val bodyForms: LObject,               //
    docBody: LString,                     // docstring sans signature
    val environment: LEnv,
    isSpecial: Boolean = false          // for Macros only
): LFunction(functionName, stdPars, keyPars, optPars, restPar,
            intern("value"), isSpecial, docBody)
{
    val lambdatype = "function"
    
    override fun desc(): String {
        var body: String

        if (bodyForms === Nil) {
            body = Nil.toString()
        } else {
            val s = bodyForms.toString()
            body = s.substring(1, s.length - 1)
        }
        
        return "#<${typeDesc()}[$id](${parlist()}) $body>"
    }




    override fun call(arglist: LObject): LObject {
        return withNewEnvironment(environment) {
            bindPars(arglist, this)
            debug(debugLambdaParamsSym) {
                currentEnv.desc() + "\nin " + bodyForms.toString()
            }
            evalProgn(bodyForms)
        }
    }

}


