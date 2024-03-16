package org.w21.lyk

class Table(elems: LObject): LObject(), Callable {
    // key: value pairs, backed by a dictionary [LObject: LObject]
    val the_table = mutableMapOf<LObject, LObject>()

    init {
        if (!elems.isList()) {
            throw TypeError("Table constructor argument is not a list")
        }
        for (elem in elems) {
            if (elem is LCons) {
                the_table[elem.car()] = elem.cdr()
            } else {
                throw InternalError("Table element is not Pair (Table.init)")
            }
        }
    }

    fun get(key: LObject, defaultvalue: LObject = Nil): LObject {
        val value = the_table[key]
        if (value != null) {
            return value
        }
        return defaultvalue
    }

    fun exists(key: LObject): LObject =
        if (the_table[key] == null) Nil else T

    fun remove(key: LObject): LObject? {
        val value = the_table[key]
        the_table.remove(key)
        return value ?: Nil
    }

    fun count() = the_table.size

    fun put(key: LObject, value: LObject) {
        the_table[key] = value
    }

    fun keys(): LObject {
        val lc = ListCollector()
        for (key in the_table.keys) {
            lc.add(key)
        }
        return lc.list()
    }

    fun values(): LObject {
        val lc = ListCollector()
        for (value in the_table.values) {
            lc.add(value)
        }
        return lc.list()
    }

    fun items(): LObject {
        val lc = ListCollector()
        for ((key, value) in the_table) {
            lc.add(LCons(key, value))
        }
        return lc.list()
    }

    override fun equal(other: LObject): Boolean {
        if (this === other) {
            return true
        }
        if (other !is Table) {
            return false
        }
        if (the_table.size != other.the_table.size) {
            return false
        }
        for ((key, value) in the_table) {
            if (value != other.the_table[key]) {
                return false
            }
        }
        return true
    }

    override fun call(arglist: LObject): LObject {
        return Nil
    }

    override fun desc() = toString()

    override fun toString(): String {
        val buf = StrBuf("#:(")
        for ((key, value) in the_table) {
            buf.add("(")
            buf.add(key.desc())
            buf.add(" . ")
            buf.add(value.desc())
            buf.add(")")
        }
        buf.add(")")
        return buf.toString()
    }
}
