// Numbers, the real ones (and others)

package org.w21.lyk

// import java.util.WeakHashMap


// class Number: LispObject, Comparable {
class Number(val value: Double): LispObject() {
    // this is a rudimentary number implementation for now

    constructor(intValue: Int) : this(intValue.toDouble()) {}

    companion object {
        val numberTable = mutableMapOf<Double, Number>()

        fun makeNumber(value: Int) = makeNumber(value.toDouble())
        
        fun makeNumber(value: Double): Number {
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

    // deinit {
    //     numberTable[value] = nil
    // }

    // static fun make(_ value: Double): Number {
    //     if let number = numberTable[value]?.number {
    //         return number
    //     }
    //     let newnum = Number(value)
    //     numberTable[value] = WeakNumRef(newnum)
    //     return newnum
    // }

    // static fun make(_ value: Int): Number {
    //     return make(Double(value))
    // }

    // override fun isNumber(): Bool { return true }
    
    // Return true iff object is an atom
    override fun isAtom() = true 

    fun isInt() = value == value.toInt().toDouble()
    
    override fun desc(): String {
        if (value <= Int.MAX_VALUE.toDouble()
                && value >= Int.MIN_VALUE.toDouble()) {
            val intVal = value.toInt()
            if (value == intVal.toDouble()) {
                return intVal.toString()
            }
        }
        return value.toString()
    }

    override fun toString() = desc()

    // // return true iff self is less than other
    // fun cmp_lt(_ other: CompObject): Bool {
    //     guard let myOther = other as? Number else {
    //         throw TypeError("cannot compare `\(self)` type \(type(of: self)) "
    //                           + "to `\(other)` type \(type(of: other))")
    //     }
    //     // print("cmp_lt(\(self), \(other)) =>", self.value < myOther.value)
    //     return self.value < myOther.value
    // }

    // // return true iff self is greater than other
    // fun cmp_gt(_ other: CompObject): Bool {
    //     guard let myOther = other as? Number else {
    //         throw TypeError("cannot compare `\(self)` type \(type(of: self)) "
    //                           + "to `\(other)` type \(type(of: other))")
    //     }
    //     return self.value > myOther.value
    // }

    // // return true iff self is equal to other
    // fun cmp_eq(_ other: CompObject): Bool {
    //     // with objects of this type being interned, equal means identical
    //     return self === other as! LispObject
    // }

    // // return true iff self is not equal to other
    // fun cmp_ne(_ other: CompObject): Bool {
    //     // with objects of this type being interned, equal means identical
    //     return self !== other as! LispObject
    // }

    // // return true iff self is greater than or equal to other
    // fun cmp_ge(_ other: CompObject): Bool {
    //     guard let myOther = other as? Number else {
    //         throw TypeError("cannot compare `\(self)` type \(type(of: self)) "
    //                           + "to `\(other)` type \(type(of: other))")
    //     }
    //     return self.value >= myOther.value
    // }

    // // return true iff self is less than or equal to other
    // fun cmp_le(_ other: CompObject): Bool {
    //     guard let myOther = other as? Number else {
    //         throw TypeError("cannot compare `\(self)` type \(type(of: self)) "
    //                           + "to `\(other)` type \(type(of: other))")
    //     }
    //     return self.value <= myOther.value
    // }

    // // return -1 if self is less than other, 0 if equal to, 1 if greater than
    // fun cmp(_ other: CompObject): Int {
    //     guard let myOther = other as? Number else {
    //         throw TypeError("cannot compare `\(self)` type \(type(of: self)) "
    //                           + "to `\(other)` type \(type(of: other))")
    //     }
    //     if self === myOther {
    //         return 0
    //     } else if self.value > myOther.value {
    //         return 1
    //     } else {
    //         return -1
    //     }
    // }

}

// struct WeakNumRef {
//     weak var number: Number?
//     init(_ number: Number) { self.number = number }
// }



// EOF
