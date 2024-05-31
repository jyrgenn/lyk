// Parent class of all objects

package org.w21.lyk

import java.util.Formatter;
import java.util.Formattable;
import java.util.FormattableFlags.*;

private var objectCounter = 0


abstract class LObject: Iterable<LObject>, Comparable<LObject>, Formattable {
    val id: Int
    
    init {
        objectCounter += 1
        id = objectCounter
    }

    abstract val obtype: String

    open fun isAtom() = false

    open fun isList() = false

    open val length: Int
        get () {
            throw ValueError("${this.obtype} $this has no length")
        }

    open fun isKeyword() = false

    // This will be used by the system, e.g. for expansion in "bla
    // $value" String templates. It will not necessarily be the form
    // that can be read in again by the reader.
    override fun toString(): String {
        return dump()
    }

    override fun formatTo(fmtr: Formatter,
                          flags: Int,
                          width: Int,
                          prec: Int) {
        val s = if ((flags and ALTERNATE) == ALTERNATE) {
            this.desc(null)
        } else {
            this.toString()
        }

        val padsize = width - s.length
        debug(debugFormatSym) {
            ("$this[${this.obtype}].format(fmtr = '$fmtr', flags = $flags,"
             + " width = $width, prec = $prec)")
        }
        if (padsize > 0) {
            val pad = mulString(" ", padsize)
            if ((flags and LEFT_JUSTIFY) == LEFT_JUSTIFY) {
                fmtr.format(s + pad)
            } else {
                fmtr.format(pad + s)
            }
        } else {
            if (width >= 0) {
                fmtr.format(s.substring(0, width))
            } else {
                fmtr.format(s)
            }
        }
    }

    // Print as much information about the object as can be helpful debugging.
    open fun dump() = "#<${this.obtype}[$id]>"

    // The output of this shall, if at all possible, be sufficent to
    // be read by the reader to re-create the object.
    open fun desc(seen: Set<Int>?) = toString()

    open operator fun component1(): LObject {
        return (this as? LCons)?.car ?:
            throw TypeError("$this is not a pair")
    }

    open operator fun component2(): LObject {
        return (this as? LCons)?.cdr ?:
            throw TypeError("$this is not a pair")
    }

    open fun toBoolean() = true

    open fun equal(other: LObject) = this === other

    open var car: LObject
        get() { throw LispError("called car on non-list $this") }
        set(_) { throw LispError("set car on non-list $this") }

    open var cdr: LObject
        get() { throw LispError("called cdr on non-list $this") }
        set(_) { throw LispError("set cdr on non-list $this") }

    open override fun compareTo(other: LObject): Int {
        throw compareError(other)
    }

    fun compareError(other: LObject): Throwable {
        return TypeError("cannot compare ${this.obtype} `$this`"
                         +" to ${other.obtype} `$other`")
    }

    override fun iterator(): Iterator<LObject>
        = throw TypeError(this, "iterable", "iterator request")
}

