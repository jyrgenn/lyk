package org.w21.lyk


class LTable(): LObject() {
    // key: value pairs, backed by a dictionary [LObject: LObject]
    val the_table = mutableMapOf<LObject, LObject>()
    
    override val obtype = "table"

    constructor(elems: LObject): this() {
        if (!elems.isList()) {
            throw TypeError("Table constructor argument is not a list")
        }
        for (elem in elems) {
            if (elem is LCons) {
                the_table[elem.car] = elem.cdr
            } else {
                the_table[elem] = Nil
            }
        }
    }

    constructor(map: Map<LSymbol, LObject>): this(Nil) {
        for (entry in map) {
            the_table[entry.key] = entry.value
        }
    }

    fun get(key: LObject, defaultvalue: LObject = Nil)
        = the_table[key] ?: defaultvalue

    fun exists(key: LObject): LObject =
        if (the_table[key] == null) Nil else T

    fun remove(key: LObject): LObject? {
        val value = the_table[key]
        the_table.remove(key)
        return value ?: Nil
    }

    override val length get() = the_table.size

    fun put(key: LObject, value: LObject) {
        the_table[key] = value
    }

    fun keys(): LObject {
        val lc = ListCollector()
        for (key in the_table.keys) {
            lc.add(key)
        }
        return lc.list
    }

    fun values(): LObject {
        val lc = ListCollector()
        for (value in the_table.values) {
            lc.add(value)
        }
        return lc.list
    }

    fun items(): LObject {
        val lc = ListCollector()
        for ((key, value) in the_table) {
            lc.add(LCons(key, value))
        }
        return lc.list
    }

    override fun equal(other: LObject): Boolean {
        if (this === other) {
            return true
        }
        if (other !is LTable) {
            return false
        }
        if (the_table.size != other.the_table.size) {
            return false
        }
        for ((key, value) in the_table) {
            val otherValue = other.the_table[key]
            if (otherValue == null) {
                return false
            }
            if (!value.equal(otherValue)){
                return false
            }
        }
        return true
    }

    override fun desc(seen: Set<Int>?): String {
        if (seen != null && this.id in seen) {
            return "..."
        }
        val seen1 = seen ?: setOf<Int>()

        val buf = StrBuf("#:(")
        for ((key, value) in the_table) {
            buf.add("(")
            buf.add(key.desc(seen1 + this.id))
            buf.add(" . ")
            buf.add(value.desc(seen1 + this.id))
            buf.add(")")
        }
        buf.add(")")
        return buf.toString()
    }

    override fun toString() = desc(null)

}
