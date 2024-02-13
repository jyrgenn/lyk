// Regular expressions

package org.w21.lyk


class Regexp(pattern: String): LispObject() {
    val regex: Regex

    init {
        regex = Regex(pattern)
    }

    override fun desc() = "#/${regex.pattern}/"
    override fun toString() = desc()

    fun match(s: String): List {
        val lc = ListCollector()
        for (match in regex.findAll(s)) {
            lc.add(LispString(match.value))
        }
        return lc.list() as List
    }

    override fun equal(other: LispObject): Boolean {
        if (this === other) {
            return true
        }
        return (other is Regexp) && regex.pattern == other.regex.pattern
    }
}
