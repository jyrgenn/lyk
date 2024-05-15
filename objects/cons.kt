// The cons cell, a.k.a. pair

package org.w21.lyk


class LCons(override var car: LObject,
            override var cdr: LObject): LObject(), LSeq {
    var mark: Boolean = false
    var hitcount: Byte = 0

    init {
        debug(debugConsSym) {
            "(cons $car $cdr)"
        }
        consCounter++
    }

    override val type = "cons"

    override fun toString() = desc(null)

    override fun isList() = true

    fun incHitcount() {
        if (hitcount >= 2) {
            hitcount = 0
        } else {
            hitcount++
        }
    }

    // This function is modeled after the algorithm in
    // https://git.w21.org/lisp/mnlisp/-/blob/master/pp/mod/sexprio.mod#L666
    // from a programming assignment I completed together with Bernd
    // Machenschalk in the early 90s -- the first Lisp interpreter
    // we made. I never really understood this algorithm called
    // "Wirbeltraversierung", but here it worked, apparently
    // flawlessly, nearly instantly after correcting a few typos.
    override fun desc(seen: Set<Int>?): String {
        if (seen != null && this.id in seen) {
            return "..."
        }
        val seen1 = (seen ?: setOf<Int>()) + this.id
        
        var sb = StrBuf()
        var current: LObject = this
        var prev: LObject = Nil
        var next: LObject //= this
        var lastWasNoLparen = false

        sb.add("(")
        do {
            if (current is LCons && current.mark) {
                sb.add(" ... )")
                lastWasNoLparen = true
                next = prev
                prev = current
                current = next
                if (current is LCons) {
                    current.mark = false
                }
            } else {
                if (current is LCons) {
                    current.incHitcount()
                    next = current.car
                    current.car = current.cdr
                    current.cdr = prev
                    if (current.hitcount == 1.toByte()) {
                        if (next !is LCons) {
                            if (lastWasNoLparen) {
                                sb.add(" ")
                            }
                            sb.add(next.desc(seen1))
                            lastWasNoLparen = true
                            prev = next
                        } else {
                            if (lastWasNoLparen) {
                                sb.add(" ")
                            }
                            sb.add("(")
                            lastWasNoLparen = false
                            current.mark = true
                            prev = current
                            current = next
                        }
                    } else if (current.hitcount == 2.toByte()) {
                        if (next === Nil) {
                            sb.add(")")
                            lastWasNoLparen = true
                            prev = next
                        } else if (next !is LCons) {
                            sb.add(" . ")
                            sb.add(next.desc(seen1))
                            sb.add(")")
                            lastWasNoLparen = true
                            prev = next
                        } else {
                            current.mark = true
                            prev = current
                            current = next
                        }
                    } else {
                        prev = current
                        current = next
                        if (current is LCons) {
                            current.mark = false
                        }
                    }
                }
            }
        } while (current !== Nil)
        
        return sb.toString()
    }

    override val length: Int
        get () {
            var len = 0
            var cell: LObject = this
            while (cell is LCons) {
                len++
                cell = cell.cdr
            }
            return len
        }
    
    override fun delete(item: LObject): LObject {
        val lc = ListCollector()
        
        for (elem in this) {
            if (!elem.equal(item)) {
                lc.add(elem)
            }
        }
        return lc.list
    }

    override fun getAt(index: Int, default: LObject?): LObject {
        var l: LObject = this
        var i = 0
        while (l is LCons) {
            if (i++ == index) {
                return l.car
            }
            l = l.cdr
        }
        if (default == null) {
            throw IndexError(this, index)
        }
        return default
    }
    override fun setAt(index: Int, value: LObject) {
        var l: LObject = this
        var i = 0
        while (l is LCons) {
            if (i++ == index) {
                l.car = value
                return
            }
            l = l.cdr
        }
        throw IndexError(this, index)
    }

    override fun elements(): LObject {
        var l: LObject = this
        return collectedList {
            while (l is LCons) {
                it.add(l.car)
                l = l.cdr
            }
            if (l !== Nil) {
                throw TypeError("not a proper list: $this")
            }
        }
    }

    override fun copy() = elements()

    override fun subseq(start: Int, end: Int?): LObject {
        if (start < 0) {
            IndexError(this, start)
        }
        if (end != null && end < 0) {
            IndexError(this, end)
        }
        var l: LObject = this
        var lc = ListCollector()
        var i = 0
        while (l is LCons) {
            if (end != null && i >= end) {
                break
            }
            if (i >= start) {
                lc.add(l.car)
            }
            i++
            l = l.cdr
        }
        if (i <= start) {
            IndexError(this, start)
        }
        if (end != null && i < end) {
            IndexError(this, end)
        }
        return lc.list
    }

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

    override fun reversed(): LObject {
        var l: LObject = Nil
        for (elem in this) {
            l = LCons(elem, l)
        }
        return l
    }

    override fun equal(other: LObject): Boolean {
        return other is LCons
            && car.equal(other.car)
            && cdr.equal(other.cdr)
    }

    fun toArray(): Array<LObject> {
        val valueArray = Array<LObject>(length) { Nil }

        var index = 0
        for (elem in this) {
            valueArray[index++] = elem
        }
        return valueArray
    }

    class ConsIterator(var l: LObject): Iterator<LObject> {
        val original = l                // keep the original list for an error

        override fun hasNext() =
            when (l) {
                is LCons -> true
                Nil -> false
                else -> throw TypeError("not a proper list: $original")

            }
        override fun next(): LObject {
            val obj = l.car
            l = l.cdr
            return obj
        }
    }

    override fun iterator(): Iterator<LObject> = ConsIterator(this)
}
