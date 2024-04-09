// strings

package org.w21.lyk

import java.util.WeakHashMap


class LString(val value: String): LObject(), LSeq {

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
            
            for (string in stringTable.values) {
                lc.add(string)
            }
            return lc.list
        }
    }

    override fun equal(other: LObject): Boolean {
        if (this === other) {
            return true
        }
        if (other !is LString) {
            return false
        }
        if (value == other.value) {
            return true
        }
        return false
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

    override fun getAt(index: Int): LObject {
        if (index >= 0 && index < value.length) {
            return makeChar(value[index])
        }
        throw IndexError(this, index)
    }

    @Suppress("UNUSED_PARAMETER")
    override fun setAt(index: Int, value: LObject) {
        throw TypeError("string object is immutable: " + desc())
    }

    override fun elements(): LObject {
        return collectedList {
            for (ch in value) {
                it.add(makeChar(ch))
            }
        }
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

    // this is a null operation for a string, because identical content means
    // identical string object anyway
    override fun copy(): LObject {
        return this
    }

    override fun subseq(start: Int, end: Int?): LObject {
        return makeString(if (end == null) {
                              value.substring(start)
                          } else {
                              value.substring(start, end)
                          })
    }

    class StringIterator(val s: LString): Iterator<LObject> {
        var nextIndex = 0

        override fun hasNext(): Boolean {
            return nextIndex < s.value.length
        }

        override fun next(): LObject {
            return makeChar(s.value[nextIndex++])
        }
    }
}

fun makeString(value: String) = LString.mkString(value)


// EOF
