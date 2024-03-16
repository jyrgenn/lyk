// Regular expressions

package org.w21.lyk


class Regexp(pattern: String): LObject() {
    val regex: Regex

    init {
        regex = Regex(pattern)
    }

    override fun desc() = "#/${regex.pattern}/"
    override fun toString() = desc()

    fun match(s: String): LObject {
        val lc = ListCollector()
        for (match in regex.findAll(s)) {
            lc.add(LString(match.value))
        }
        return lc.list()
    }

    override fun equal(other: LObject): Boolean {
        if (this === other) {
            return true
        }
        return (other is Regexp) && regex.pattern == other.regex.pattern
    }
}

