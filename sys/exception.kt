package org.w21.lyk

import java.io.IOException

class ErrorObject(val error: LispError): LObject() {
    override fun toString() = error.toString()

    override fun desc(seen: Set<Int>?) =
        "#<${this.type}: ${error.toString()}>"

    override val type = "error-object"
}

open class LispError(message: String, val data: LObject = Nil
): Exception(message) {
    val evalStack = ListCollector()

    val name: String
        get () {
            val typename = this::class.simpleName.toString()
            if (typename == "LispError") {
                return "Error"
            }
            return typename
        }

    fun toObject() = ErrorObject(this)

    override fun toString(): String {
        var s = "${this.name}: $message"
        if (data !== Nil) {
            s += ": " + data.toString()
        }
        return s
    }

    fun pushFrame(level: Int, form: LObject, env: LEnv, location: String) {
        evalStack.add(LVector(makeNumber(level), form, env,
                              makeString(location)))
    }
}

class ProcessError(exit_status: Int, function: String,
                   command: String):
    LispError("$function: command exit status $exit_status: `$command`")

class NotImplementedError(what: String):
    LispError("function `$what` is not implemented")

class IndexError(message: String): LispError(message) {
    constructor(what: LObject, index: Int):
        this("invalid index $index for ${what.type} $what")
}

class WarningError(message: String): LispError(message)

open class IOError(message: String): LispError(message) {
    constructor(err: Exception, message: String? = null):
        this((if (message == null) "" else message + ": ")
             + "${err::class.simpleName}: ${err.message}")
}


open class InternalError(message: String): LispError(message)


class JavaError(val err: Exception): LispError(err.message ?: "Java error") {
    override fun toString() =
        "${this.name}: ${err::class.simpleName}: $message"
}

open class ParseError(message: String,
                      val lh: LocationHolder): LispError(message)
{
    override fun toString() = "${lh.location()}: $message"
}

class SyntaxError(message: String,
                  lh: LocationHolder): ParseError(message, lh)

class InternalReaderError(message: String,
                          val lh: LocationHolder): LispError(message) {
    override fun toString() = "${lh.location()}: $message"
}

class ImmutableError(val symbol: LSymbol, val function: Boolean): 
    LispError("symbol $symbol is immutable")

open class ValueError(message: String,
                      lh: LocationHolder?): LispError(message) {
    val loc: String?

    init {
        loc = lh?.location()
    }
    
    constructor(message: String) : this(message, null) {}
    
    override fun toString(): String {
        val location = if (loc == null) "" else " at $loc"
        return super.toString() + location
    }

}

// return the appropriate indefinite article for the passed noun, meaning
// "an" if the noun starts with a vowel, else "a"
fun indef_a(noun: String): String {
    if (Regex("^[aeiouAEIOU]").find(noun) != null) {
        return "an"
    }
    return "a"
}

class TypeError(message: String, lh: LocationHolder?): ValueError(message, lh) {
    constructor(message: String) : this(message, null)
    constructor(obj: LObject, whatnot: String, where: String): 
        this("$where, not ${indef_a(whatnot)} $whatnot: ${obj.type} $obj")
}

class CallError(message: String): LispError(message)

class ArgumentError(message: String): LispError(message)

class LambdaDefError(message: String): LispError(message)

class FunctionError(message: String): LispError(message)

class AbortEvalSignal(message: String): LispError(message)

class ThrowSignal(val tag: LObject, val value: LObject):
    LispError("uncaught; tag $tag value $value")

class ReturnSignal(val value: LObject): LispError("returns", value)

class EOFError(message: String): LispError(message) {
    constructor(stream: LStream): this("on stream $stream")
}

class OtherError(message: String, val e: Exception): LispError(message) {
    override fun toString(): String = "${super.toString()}: $e"
}

class NotProvidedError(val feature: LSymbol):
    LispError("Required feature `$feature` has not been provided") {
}

class AssertionFail(form: LObject, val moreInfo: LObject? = Nil):
    LispError(form.toString() + if (moreInfo === Nil) "" else ", $moreInfo")

class RegexpError(val e: java.util.regex.PatternSyntaxException
): LispError(e.pattern) {
    override fun toString(): String {
        var s = "${this.name}: " +
            "invalid regexp pattern '${e.pattern}', ${e.message}"
        return s
    }    
}

// make some LispError from any Java exception
fun makeLispError(exc: Exception) =
    when (exc) {
        is LispError -> exc
        is java.util.regex.PatternSyntaxException ->
            RegexpError(exc)
        is IOException -> IOError(exc)
        else -> JavaError(exc)
    }

