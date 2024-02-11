// Numbers, the real ones (and others)

package org.w21.lyk


// class Number: LispObject, Comparable {
class Number(val value: Double): LispObject() {
    // this is a rudimentary number implementation for now

    constructor(intValue: Int) : this(intValue.toDouble()) {}

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
    
    override fun description(): String {
        if (value <= Int.MAX_VALUE.toDouble()) {
            val intVal = value.toInt()
            if (value == intVal.toDouble()) {
                return intVal.toString()
            }
        }
        return value.toString()
    }

    // // return true iff self is less than other
    // fun cmp_lt(_ other: Comparable): Bool {
    //     guard let myOther = other as? Number else {
    //         throw TypeError("cannot compare `\(self)` type \(type(of: self)) "
    //                           + "to `\(other)` type \(type(of: other))")
    //     }
    //     // print("cmp_lt(\(self), \(other)) =>", self.value < myOther.value)
    //     return self.value < myOther.value
    // }

    // // return true iff self is greater than other
    // fun cmp_gt(_ other: Comparable): Bool {
    //     guard let myOther = other as? Number else {
    //         throw TypeError("cannot compare `\(self)` type \(type(of: self)) "
    //                           + "to `\(other)` type \(type(of: other))")
    //     }
    //     return self.value > myOther.value
    // }

    // // return true iff self is equal to other
    // fun cmp_eq(_ other: Comparable): Bool {
    //     // with objects of this type being interned, equal means identical
    //     return self === other as! LispObject
    // }

    // // return true iff self is not equal to other
    // fun cmp_ne(_ other: Comparable): Bool {
    //     // with objects of this type being interned, equal means identical
    //     return self !== other as! LispObject
    // }

    // // return true iff self is greater than or equal to other
    // fun cmp_ge(_ other: Comparable): Bool {
    //     guard let myOther = other as? Number else {
    //         throw TypeError("cannot compare `\(self)` type \(type(of: self)) "
    //                           + "to `\(other)` type \(type(of: other))")
    //     }
    //     return self.value >= myOther.value
    // }

    // // return true iff self is less than or equal to other
    // fun cmp_le(_ other: Comparable): Bool {
    //     guard let myOther = other as? Number else {
    //         throw TypeError("cannot compare `\(self)` type \(type(of: self)) "
    //                           + "to `\(other)` type \(type(of: other))")
    //     }
    //     return self.value <= myOther.value
    // }

    // // return -1 if self is less than other, 0 if equal to, 1 if greater than
    // fun cmp(_ other: Comparable): Int {
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

fun makeNumber(value: Int) = Number(value)
fun makeNumber(value: Double) = Number(value)

// struct WeakNumRef {
//     weak var number: Number?
//     init(_ number: Number) { self.number = number }
// }



// EOF
