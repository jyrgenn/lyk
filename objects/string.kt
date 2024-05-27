// strings

package org.w21.lyk

import java.util.WeakHashMap


class LString(val the_string: String): LObject(), LSeq {

    // deinit {
    //     stringTable[the_string] = nil
    // }

    override val obtype = "string"

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

    class StringIterator(val s: LString): Iterator<LObject> {
        var nextIndex = 0

        override fun hasNext(): Boolean {
            return nextIndex < s.the_string.length
        }

        override fun next(): LObject {
            return makeChar(s.the_string[nextIndex++])
        }
    }

    /////// Implementation of the LSeq interface ///////

    // Get the element of the sequence at the specified index;
    // return the default value if index isn't in the sequence,
    // or if the default value is null, raise an error.
    override fun getAt(index: Int, default: LObject?): LObject {
        if (index >= 0 && index < the_string.length) {
            return makeChar(the_string[index])
        }
        if (default == null) {
            throw IndexError(this, index)
        }
        return default
    }
    
    // Set the element of the sequence at the specified index to the
    // specified value; raise an error if the index isn't in the
    // sequence or if the sequence is immutable (meaning Strings, at
    // least for now).
    @Suppress("UNUSED_PARAMETER")
    override fun setAt(index: Int, value: LObject) {
        throw TypeError("string object is immutable: " + desc(null))
    }

    // Return an iterator for the sequence.
    override operator fun iterator(): Iterator<LObject> = StringIterator(this)

    // Return a (shallow) copy of the sequence.
    // this is a null operation for a string, because identical content means
    // identical string object anyway
    override fun copy() = this

    // Return a list with the elements of the sequence.
    override fun elements(): LObject {
        return collectedList {
            for (ch in the_string) {
                it.add(makeChar(ch))
            }
        }
    }

    // Return a subsequence of the sequence. If start is null, start
    // at index 0; if end is null, end at the end of the sequence.
    // If both are null, return the sequence itself; otherwise, the
    // subsequence is a copy of the original sequence structure.
    override fun subseq(start: Int?, end: Int?): LObject {
        if (start == null) {
            if (end == null) {
                return this
            } else {
                if (end > the_string.length) {
                    IndexError(this, end)
                }
            }
        } else {
            if (start == 0 && (end ?: 0) == 0) {
                return this
            }
            if (start < 0 || start >= the_string.length) {
                throw IndexError(this, start)
            }
        }
        return makeString(if (end == null) {
                              the_string.substring(start ?: 0)
                          } else {
                              the_string.substring(start ?: 0, end)
                          })
    }

    // Return a reversed copy of the sequence. The sequence is not
    // altered.
    override fun reversed(): LObject {
        return makeString(the_string.reversed())
    }

    // Return a copy of the sequence withe the specified item
    // deleted. The item is identified using equal().
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

    // Find the item in the sequence for which the predicate is
    // true. Return a Pair of the item and its index in the
    // sequence. If no such item is found, return a Pair of Nil and
    // -1.
    override fun find(start: Int, end: Int?, last: Boolean,
             predicate: (LObject) -> Boolean): Pair<LObject, Int> {
        var result: Pair<LObject, Int> = Pair(Nil, -1)
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
                    result = Pair(elem, index)
                } else {
                    return Pair(elem, index)
                }
            }
        }
        return result
    }

    // The length of the sequence, read only.
    override val length get () = the_string.length
    
}

fun makeString(value: String) = LString.mkString(value)


// EOF
