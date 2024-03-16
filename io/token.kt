// Token types for the Reader. Of the most, the function is fully described by
// their type; tokens for strings, symbols, numbers, and regexps also have some
// value.

package org.w21.lyk

open class ReaderToken(reader: Reader): LocationHolder {
    val readerLocation = reader.location()

    open override fun location() = readerLocation
    open fun desc() = typeOf(this)
    override fun toString() = desc()
}

// WARNING: don't let these inherit from another -- they must be disjoint types,
// or otherwise the type checks depend on order, which is not good
class OparenToken(reader: Reader): ReaderToken(reader)
class CparenToken(reader: Reader): ReaderToken(reader) {}
class PeriodToken(reader: Reader): ReaderToken(reader) {}
class QuoteToken(reader: Reader): ReaderToken(reader) {}
class FunctionToken(reader: Reader): ReaderToken(reader) {}
class UnquoteToken(reader: Reader): ReaderToken(reader) {}
class QuasiquoteToken(reader: Reader): ReaderToken(reader) {}
class UnquoteSplicingToken(reader: Reader): ReaderToken(reader) {}
class TableStartToken(reader: Reader): ReaderToken(reader) {}
class VectorStartToken(reader: Reader): ReaderToken(reader) {}
class StringToken(reader: Reader, val value: String): ReaderToken(reader) {
    override fun desc() = super.desc() + "($value)"
}
class SymbolToken(reader: Reader, val value: String): ReaderToken(reader) {
    override fun desc() = super.desc() + "($value)"
}
class NumberToken(reader: Reader, val value: Double): ReaderToken(reader) {
    override fun desc() = super.desc() + "($value)"
}
class RegexpToken(reader: Reader, val value: String): ReaderToken(reader) {
    override fun desc() = super.desc() + "($value)"
}
class EOFToken(reader: Reader): ReaderToken(reader) {}
class QuasiQuoteToken(reader: Reader): ReaderToken(reader) {}
