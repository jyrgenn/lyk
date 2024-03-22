// strings

package org.w21.lyk

import java.util.WeakHashMap


class LString(val value: String): LObject() {

    // deinit {
    //     stringTable[value] = nil
    // }

    companion object {
        // val stringTable = mutableMapOf<String, LString>()
        val stringTable = WeakHashMap(mutableMapOf<String, LString>())

        fun mkString(value: String): LString {
            var strob = stringTable.get(value)
            if (strob == null) {
                strob = LString(value)
                stringTable.put(value, strob)
            }
            return strob
        }

        fun strings(): LObject {
            // return list2lisp(stringTable.values)

            val lc = ListCollector()
            
            for (num in stringTable.values) {
                lc.add(num)
            }
            return lc.list
        }
    }

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

    override fun compareTo(other: LObject): Int {
        if (other is LString) {
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

fun makeString(value: String) = LString.mkString(value)


// EOF

