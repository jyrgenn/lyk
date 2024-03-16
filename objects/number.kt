// Numbers, the real ones (and others)

package org.w21.lyk

import java.util.WeakHashMap


// class Number: LispObject, Comparable {
class Number(val value: Double): LispObject() {
    // this is a rudimentary number implementation for now

    constructor(longValue: Long) : this(longValue.toDouble()) {}
    constructor(intValue: Int) : this(intValue.toDouble()) {}

    companion object {
        // val numberTable = mutableMapOf<Double, Number>()

        val numberTable = WeakHashMap(mutableMapOf<Double, Number>())

        fun mkNumber(value: Double): Number {
            var numob = numberTable.get(value)
            if (numob == null) {
                numob = Number(value)
                numberTable.put(value, numob)
            }
            return numob
        }

        fun numbers(): LispObject {
            // return list2lisp(numberTable.values)

            val lc = ListCollector()
            
            for (num in numberTable.values) {
                lc.add(num)
            }
            return lc.list()
        }
    }

    
    // Return true iff object is an atom
    override fun isAtom() = true 

    fun isLong() = value == value.toLong().toDouble()
    
    override fun desc(): String {
        if (value <= Long.MAX_VALUE.toDouble()
                && value >= Long.MIN_VALUE.toDouble()) {
            val longVal = value.toLong()
            if (value == longVal.toDouble()) {
                return longVal.toString()
            }
        }
        return value.toString()
    }

    override fun toString() = desc()

    override fun compareTo(other: LispObject): Int {
        if (other is Number) {
            if (value < other.value) {
                return -1
            } else if (value > other.value) {
                return 1
            } else {
                return 0
            }
        } else {
            throw compareError(other)
        }
    }
}

fun makeNumber(value: Long) = Number.mkNumber(value.toDouble())
fun makeNumber(value: Int) = Number.mkNumber(value.toDouble())
fun makeNumber(value: Double) = Number.mkNumber(value)


// EOF
