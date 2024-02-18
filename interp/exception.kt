package org.w21.lyk


open class ParseError(val error: String,
                       val lh: LocationHolder): Exception(error)
{
    override fun toString() = "${lh.location()}: $error"
}

class SyntaxError(error: String,
                  lh: LocationHolder): ParseError(error, lh)

class InternalReaderError(val error: String,
                          val lh: LocationHolder): Exception(error) {
    override fun toString() = "${lh.location()}: $error"
}

open class ValueError(val error: String,
                      lh: LocationHolder?): Exception(error) {
    val loc: String?

    init {
        loc = lh?.location()
    }
    
    constructor(error: String) : this(error, null) {}
    
    override fun toString(): String {
        val location = if (loc == null) "" else " at $loc"
        return "${typeOf(this)}: $error$location"
    }

}

class TypeError(error: String, lh: LocationHolder?): ValueError(error, lh) {
    constructor(error: String) : this(error, null) {}
}

class IOError(error: String): Exception(error)

class CallError(error: String): Exception(error)

class ArgumentError(error: String): Exception(error)

class LambdaDefError(error: String): Exception(error)

class FunctionError(error: String): Exception(error)
