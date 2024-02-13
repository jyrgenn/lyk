
package org.w21.lyk

// strings

// class LispString: LispObject, Comparable {
class LispString(val value: String): LispObject() {

    // deinit {
    //     stringTable[value] = nil
    // }

    // Return true iff object is an atom
    override fun isAtom() = true

    override fun toString() = value

    // with all quoting and stuff
    override fun desc(): String {
        var result = CharBuf('\"')
        for (ch in value) {
            if (ch in "\\\"") {
                result.add('\\')
            }
            result.add(ch)
        }
        result.add('\"')
        return result.toString()
    }

    // static fun make(_ value: String) -> LispString {
    //     let value = value.precomposedStringWithCanonicalMapping
    //     if let str = stringTable[value]?.string {
    //         return str
    //     }
    //     let newstr = LispString(value)
    //     stringTable[value] = WeakStrRef(newstr)
    //     return newstr
    // }

    // // return true iff self is less than other
    // fun cmp_lt(_ other: Comparable) throws -> Bool {
    //     guard let myOther = other as? LispString else {
    //         throw TypeError("cannot compare `\(self)` type \(type(of: self)) "
    //                           + "to `\(other)` type \(type(of: other))")
    //     }
    //     return self.value < myOther.value
    // }

    // // return true iff self is greater than other
    // fun cmp_gt(_ other: Comparable) throws -> Bool {
    //     guard let myOther = other as? LispString else {
    //         throw TypeError("cannot compare `\(self)` type \(type(of: self)) "
    //                           + "to `\(other)` type \(type(of: other))")
    //     }
    //     return self.value > myOther.value
    // }

    // // return true iff self is equal to other
    // fun cmp_eq(_ other: Comparable) throws -> Bool {
    //     // with objects of this type being interned, equal means identical
    //     return self === other as! LispObject
    // }

    // // return true iff self is not equal to other
    // fun cmp_ne(_ other: Comparable) throws -> Bool {
    //     // with objects of this type being interned, equal means identical
    //     return self !== other as! LispObject
    // }

    // // return true iff self is greater than or equal to other
    // fun cmp_ge(_ other: Comparable) throws -> Bool {
    //     guard let myOther = other as? LispString else {
    //         throw TypeError("cannot compare `\(self)` type \(type(of: self)) "
    //                           + "to `\(other)` type \(type(of: other))")
    //     }
    //     return self.value >= myOther.value
    // }

    // // return true iff self is less than or equal to other
    // fun cmp_le(_ other: Comparable) throws -> Bool {
    //     guard let myOther = other as? LispString else {
    //         throw TypeError("cannot compare `\(self)` type \(type(of: self)) "
    //                           + "to `\(other)` type \(type(of: other))")
    //     }
    //     return self.value <= myOther.value
    // }

    // // return -1 if self is less than other, 0 if equal to, 1 if greater than
    // fun cmp(_ other: Comparable) throws -> Int {
    //     guard let myOther = other as? LispString else {
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

fun makeString(value: String) = LispString(value)

// fun makeString(_ value: String) -> LispString {
//     return LispString.make(value)
// }

// struct WeakStrRef {
//     weak var string: LispString?
//     init(_ string: LispString) { self.string = string }
// }

// EOF

