// Regular expressions

package org.w21.lyk


class LRegexp(pattern: String): LObject() {
    var regexp = Regex("")

    init {
        try {
            regexp = Regex(pattern)
        } catch (e: java.util.regex.PatternSyntaxException) {
            throw RegexpError(e)
        }
    }

    override fun desc() = "#/${regexp.pattern}/"
    override fun toString() = desc()

    fun match(s: String): LObject {
        val lc = ListCollector()
        val match = regexp.find(s)
        if (match != null) {
            // lc.add(makeString(match.value))
            for (group in match.groups) {
                lc.add(if (group != null) makeString(group.value) else Nil)
            }
        }
        return lc.list
    }

    fun split(s: String, limit: Int) = regexp.split(s, limit)

    fun replace(s: String, replacement: String, limit: Int): LString {
        if (limit == 0) {
            var previou_s: String
            var new_s = s
            do {
                previou_s = new_s
                new_s = regexp.replaceFirst(previou_s, replacement)
            } while (new_s != previou_s)
            return makeString(new_s)
        }
        var new_s = s
        for (n in 0..<limit) {
            new_s = regexp.replaceFirst(new_s, replacement)
        }
        return makeString(new_s)
    }
    

    override fun equal(other: LObject): Boolean {
        if (this === other) {
            return true
        }
        return (other is LRegexp) && regexp.pattern == other.regexp.pattern
    }
}

