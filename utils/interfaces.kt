// dig. utilities

package org.w21.lyk


fun interface LocationHolder {
    fun location(): String
}

interface LSeq {
    // Get the element of the sequence at the specified index;
    // return the default value if index isn't in the sequence,
    // or if the default value is null, raise an error.
    fun getAt(index: Int, default: LObject? = null): LObject
    
    // Set the element of the sequence at the specified index to the
    // specified value; raise an error if the index isn't in the
    // sequence or if the sequence is immutable (meaning Strings, at
    // least for now).
    fun setAt(index: Int, value: LObject)

    // Return an iterator for the sequence.
    operator fun iterator(): Iterator<LObject>

    // Return a (shallow) copy of the sequence.
    fun copy(): LObject

    // Return a list with the elements of the sequence.
    fun elements(): LObject

    // Return a subsequence of the sequence. If start is null, start
    // at index 0; if end is null, end at the end of the sequence.
    // If both are null, return the sequence itself; otherwise, the
    // subsequence is a copy of the original sequence structure.
    fun subseq(start: Int?, end: Int?): LObject

    // Return a reversed copy of the sequence. The sequence is not
    // altered.
    fun reversed(): LObject

    // Return a copy of the sequence withe the specified item
    // deleted. The item is identified using equal().
    fun delete(item: LObject): LObject

    // Find the item in the sequence for which the predicate is
    // true. Return a Pair of the item and its index in the
    // sequence. If no such item is found, return a Pair of Nil and
    // -1.
    fun find(start: Int, end: Int?, last: Boolean,
             predicate: (LObject) -> Boolean): Pair<LObject, Int>

    // The length of the sequence, read only.
    val length: Int
}
