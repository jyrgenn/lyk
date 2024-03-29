package org.w21.lyk

import java.io.IOException

class ErrorObject(val error: LispError): LObject() {
    override fun toString() = error.toString()

    override fun desc() = "#<${typeOf(this)}: {error.toString()}>"
}

open class LispError(message: String): Exception(message) {

    fun toObject() = ErrorObject(this)

    open override fun toString() = "${typeOf(this)}: $message"

    fun pushFrame(level: Int, form: LObject, env: LEnv, location: String) {
        evalStack.add(LVector(makeNumber(level), form, env,
                              makeString(location)))
    }
}

open class IOError(err: Exception):
    LispError("${err::class.simpleName}: ${err.message}")


open class InternalError(message: String): LispError(message)


class JavaError(val err: Exception): LispError(err.message ?: "Java error") {
    override fun toString() =
        "${typeOf(this)}: ${err::class.simpleName}: $message"
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

class UserError(message: String, val data: LObject = Nil): LispError(message) {
    override fun toString(): String {
        var s = super.toString()
        if (data !== Nil) {
            s += ", " + data.toString()
        }
        return s
    }
}

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

class TypeError(message: String, lh: LocationHolder?): ValueError(message, lh) {
    constructor(message: String) : this(message, null) {}
}

class CallError(message: String): LispError(message)

class ArgumentError(message: String): LispError(message)

class LambdaDefError(message: String): LispError(message)

class FunctionError(message: String): LispError(message)

class AbortEvalSignal(message: String): LispError(message)

class ThrowSignal(val tag: LObject, val value: LObject):
    LispError("uncaught; tag $tag value $value")

class EOFError(message: String): LispError(message) {
    constructor(stream: LStream): this("on stream $stream")
}

class OtherError(message: String, val e: Exception): LispError(message) {
    override fun toString(): String = "${super.toString()}: $e"
}

class NotProvidedError(val feature: LSymbol):
    LispError("Required feature `$feature` has not been provided") {
}

class AssertionFail(form: LObject, val moreInfo: LObject):
    LispError(form.toString())
{
    override fun toString(): String =
        "${super.toString()}" + if (moreInfo === Nil) "" else ", $moreInfo"
}

// make some LispError from any Java exception
fun makeLispError(exc: Exception) =
    when (exc) {
        is IOException -> IOError(exc)
        else -> JavaError(exc)
    }

