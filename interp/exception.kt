package org.w21.lyk


open class ParseError(message: String,
                       val lh: LocationHolder): Exception(message)
{
    override fun toString() = "${lh.location()}: $message"
}

class SyntaxError(message: String,
                  lh: LocationHolder): ParseError(message, lh)

class InternalReaderError(message: String,
                          val lh: LocationHolder): Exception(message) {
    override fun toString() = "${lh.location()}: $message"
}

open class ValueError(message: String,
                      lh: LocationHolder?): Exception(message) {
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

class IOError(message: String): Exception(message)

class CallError(message: String): Exception(message)

class ArgumentError(message: String): Exception(message)

class LambdaDefError(message: String): Exception(message)

class FunctionError(message: String): Exception(message)

class AbortEvalSignal(message: String): Exception(message)

class LispError(message: String, label: String = "Lisp Message"):
    Exception(label + ": " + message)
{
    fun pushFrame(level: Int, form: LispObject, env: Environment) {
        evalStack = Cons(Vector(makeNumber(level), form, env), evalStack)
    }
}
