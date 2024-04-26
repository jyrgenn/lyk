// Numbers, the real ones (and others)

package org.w21.lyk

import java.util.WeakHashMap


// class LNumber: LObject, Comparable {
class LNumber(val the_number: Double): LObject() {
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

        fun numbers(): LObject {
            // return list2lisp(numberTable.values)

            val lc = ListCollector()
            
            for (num in numberTable.values) {
                lc.add(num)
            }
            return lc.list
        }
    }

    
    override val type = "number"

    // Return true iff object is an atom
    override fun isAtom() = true 

    fun isLong() = the_number == the_number.toLong().toDouble()
    fun isInt() = the_number == the_number.toInt().toDouble()

    fun toLong() = the_number.toLong()
    fun toInt() = the_number.toInt()
    
    override fun desc(seen: MutableSet<LObject>?): String {
        if (the_number <= Long.MAX_VALUE.toDouble()
                && the_number >= Long.MIN_VALUE.toDouble()) {
            val longVal = the_number.toLong()
            if (the_number == longVal.toDouble()) {
                return longVal.toString()
            }
        }
        return the_number.toString()
    }

    override fun toString() = desc(null)

    override fun compareTo(other: LObject): Int {
        if (other is LNumber) {
            if (the_number < other.the_number) {
                return -1
            } else if (the_number > other.the_number) {
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
