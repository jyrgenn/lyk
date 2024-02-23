package org.w21.lyk


class ErrorObject(val error: LispError): LispObject() {
    fun toString() = error.toString()
}

open class LispError(message: String): Exception(message) {
    fun asObject() = ErrorObject(this)

    fun toString() = "#<${super.toString()}>"

    fun pushFrame(level: Int, form: LispObject, env: Environment) {
        evalStack = Cons(Vector(makeNumber(level), form, env), evalStack)
    }
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

open class ValueError(message: String,
                      lh: LocationHolder?): LispError(message) {
    val loc: String?

    init {
        loc = lh?.location()
    }
    
    constructor(message: String) : this(message, null) {}
    
    override fun toString(): String {
        val location = if (loc == null) "" else " at $loc"
        return "${typeOf(this)}: $message$location"
    }

}

class TypeError(message: String, lh: LocationHolder?): ValueError(message, lh) {
    constructor(message: String) : this(message, null) {}
}

class IOError(message: String): LispError(message)

class CallError(message: String): LispError(message)

class ArgumentError(message: String): LispError(message)

class LambdaDefError(message: String): LispError(message)

class FunctionError(message: String): LispError(message)

class AbortEvalSignal(message: String): LispError(message)

class ThrowSignal(val tag: LispObject, val value: LispObject): Exception()
