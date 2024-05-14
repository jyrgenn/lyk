// strings

package org.w21.lyk

import java.util.WeakHashMap


class LString(val the_string: String): LObject(), LSeq {

    // deinit {
    //     stringTable[the_string] = nil
    // }

    override val type = "string"

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
        if (the_string == other.the_string) {
            return true
        }
        return false
    }

    // Return true iff object is an atom
    override fun isAtom() = true

    override fun toString() = the_string

    // with all quoting and stuff
    override fun desc(seen: Set<Int>?): String {
        var result = CharBuf('\"')
        for (ch in the_string) {
            if (ch in "\\\"") {
                result.add('\\')
            }
            result.add(ch)
        }
        result.add('\"')
        return result.toString()
    }

    override fun getAt(index: Int): LObject {
        if (index >= 0 && index < the_string.length) {
            return makeChar(the_string[index])
        }
        throw IndexError(this, index)
    }

    @Suppress("UNUSED_PARAMETER")
    override fun setAt(index: Int, value: LObject) {
        throw TypeError("string object is immutable: " + desc(null))
    }

    override fun find(start: Int, end: Int?, last: Boolean,
                      predicate: (LObject) -> Boolean): LObject {
        var result: LObject = Nil
        var index = -1
        for (elem in this) {
            index++
            if (index < start) {
                continue
            }
            if (end != null && index >= end) {
                break
            }
            if (predicate(elem)) {
                if (last) {
                    result = elem
                } else {
                    return elem
                }
            }
        }
        return result
    }

    override fun position(start: Int, end: Int?, last: Boolean,
                      predicate: (LObject) -> Boolean): Int {
        var result = -1
        var index = -1
        for (elem in this) {
            index++
            if (index < start) {
                continue
            }
            if (end != null && index >= end) {
                break
            }
            if (predicate(elem)) {
                if (last) {
                    result = index
                } else {
                    return index
                }
            }
        }
        return result
    }

    override fun elements(): LObject {
        return collectedList {
            for (ch in the_string) {
                it.add(makeChar(ch))
            }
        }
    }

    override fun compareTo(other: LObject): Int {
        if (other is LString) {
            if (the_string < other.the_string) {
                return -1
            } else if (the_string > other.the_string) {
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
                              the_string.substring(start)
                          } else {
                              the_string.substring(start, end)
                          })
    }

    override fun reversed(): LObject {
        // val sb = StrBuf()
        // for (ch in the_string.length - 1 downTo 0) {
        //     sb.add(ch)
        // }
        // return makeString(sb.toString())
        return makeString(the_string.reversed())
    }

    override val length get() = the_string.length

    override fun delete(item: LObject): LObject {
        if (item is LChar) {
            val sb = StrBuf()
            val char = item.the_char
            for (ch in the_string) {
                if (ch != char) {
                    sb.add(ch)
                }
            }
            return makeString(sb.toString())
        }
        return this
    }

    class StringIterator(val s: LString): Iterator<LObject> {
        var nextIndex = 0

        override fun hasNext(): Boolean {
            return nextIndex < s.the_string.length
        }

        override fun next(): LObject {
            return makeChar(s.the_string[nextIndex++])
        }
    }

    override fun iterator() = StringIterator(this)
}

fun makeString(value: String) = LString.mkString(value)


// EOF
