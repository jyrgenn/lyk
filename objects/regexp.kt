// Regular expressions

package org.w21.lyk


class LRegexp(pattern: String): LObject() {
    var the_regexp = Regex("")

    init {
        try {
            the_regexp = Regex(pattern)
        } catch (e: java.util.regex.PatternSyntaxException) {
            throw RegexpError(e)
        }
    }

    override val type = "regexp"

    override fun desc(seen: MutableSet<LObject>?) =
        "#/${the_regexp.pattern.replace("/", "\\/")}/"
    
    override fun toString() = desc(null)

    fun matchToList(match: MatchResult?): LObject {
        val lc = ListCollector()
        if (match != null) {
            // lc.add(makeString(match.value))
            for (group in match.groups) {
                lc.add(if (group != null)
                           makeString(group.value)
                       else
                           emptyString)
            }
        }
        return lc.list        
    }

    fun match(s: String): LObject {
        val match = the_regexp.find(s)
        return matchToList(match)
    }

    fun findAll(s: String): LObject {
        val lc = ListCollector()
        for (match in the_regexp.findAll(s)) {
            lc.add(matchToList(match))
        }
        return lc.list
    }

    fun split(s: String, limit: Int) = the_regexp.split(s, limit)

    fun replace(s: String, replacement: String, limit: Int): LString {
        if (limit == 0) {
            var previou_s: String
            var new_s = s
            do {
                previou_s = new_s
                new_s = the_regexp.replaceFirst(previou_s, replacement)
            } while (new_s != previou_s)
            return makeString(new_s)
        }
        var new_s = s
        for (n in 0..<limit) {
            new_s = the_regexp.replaceFirst(new_s, replacement)
        }
        return makeString(new_s)
    }
    

    override fun equal(other: LObject): Boolean {
        if (this === other) {
            return true
        }
        return (other is LRegexp)
            && the_regexp.pattern == other.the_regexp.pattern
    }
}

