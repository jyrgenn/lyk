
package org.w21.lyk

// strings

class LispString(val value: String): LispObject() {

    // deinit {
    //     stringTable[value] = nil
    // }

    companion object {
        val stringTable = mutableMapOf<String, LispString>()

        fun makeString(value: String): LispString {
            var strob = stringTable.get(value)
            if (strob == null) {
                strob = LispString(value)
                stringTable.put(value, strob)
            }
            return strob
        }

        fun strings(): LispObject {
            // return list2lisp(stringTable.values)

            val lc = ListCollector()
            
            for (num in stringTable.values) {
                lc.add(num)
            }
            return lc.list()
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

    override fun compareTo(other: LispObject): Int {
        if (other is LispString) {
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


// EOF

