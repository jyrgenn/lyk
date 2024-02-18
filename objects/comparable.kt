// Things that are comparable (if of the same type)

interface CompObject {
    // return true iff self is less than other
    fun cmp_lt(other: CompObject): Boolean

    // return true iff self is greater than other
    fun cmp_gt(other: CompObject): Boolean

    // return true iff self is equal to other
    fun cmp_eq(other: CompObject): Boolean

    // return true iff self is not equal to other
    fun cmp_ne(other: CompObject): Boolean

    // return true iff self is greater than or equal to other
    fun cmp_ge(other: CompObject): Boolean

    // return true iff self is less than or equal to other
    fun cmp_le(other: CompObject): Boolean

    // return -1 if self is less than other, 0 if equal to, 1 if greater than
    fun cmp(other: CompObject): Int
}
