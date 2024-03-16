// Numbers, the real ones (and others)

package org.w21.lyk

import java.util.WeakHashMap


// class LNumber: LispObject, Comparable {
class LNumber(val value: Double): LispObject() {
    // this is a rudimentary number implementation for now

    constructor(longValue: Long) : this(longValue.toDouble()) {}
    constructor(intValue: Int) : this(intValue.toDouble()) {}

    companion object {
        // val numberTable = mutableMapOf<Double, LNumber>()

        val numberTable = WeakHashMap(mutableMapOf<Double, LNumber>())

        fun mkNumber(value: Double): LNumber {
            var numob = numberTable.get(value)
            if (numob == null) {
                numob = LNumber(value)
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
        if (other is LNumber) {
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

fun makeNumber(value: Long) = LNumber.mkNumber(value.toDouble())
fun makeNumber(value: Int) = LNumber.mkNumber(value.toDouble())
fun makeNumber(value: Double) = LNumber.mkNumber(value)


// EOF
