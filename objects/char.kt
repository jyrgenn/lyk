// the character object type

package org.w21.lyk

import java.util.WeakHashMap


class LChar(val ch: Char): LObject() {

    companion object {
        val charTable = WeakHashMap(mutableMapOf<Char, LChar>())

        val charName = mapOf(
            ' '      to "Space",
            '\n'     to "Newline",
            '\t'     to "Tab",
            '\u000c' to "Page",
            '\u007f' to "Rubout",
            '\r'     to "Return",
            '\u0008' to "Backspace",
            '\u0007' to "Bell",
        )            

        fun mkChar(ch: Char): LChar {
            var chob = charTable.get(ch)
            if (chob == null) {
                chob = LChar(ch)
                charTable.put(ch, chob)
            }
            return chob
        }

        fun chars(): LObject {
            return list2lisp(charTable.values)
        }
    }

    override fun equal(other: LObject): Boolean {
        if (this === other) {
            return true
        }
        if (other !is LChar) {
            return false
        }
        if (ch == other.ch) {
            return true
        }
        return false
    }
    
    override fun isAtom() = true

    override fun toString() = ch.toString()

    fun ascii_glyph() =
        if (ch.code in 33..126) {
            ch.toString()
        } else {
            null
        }

    override fun desc() = 
        "#\\" + (charName[ch] ?:
                     ascii_glyph() ?:
                     if (ch.code < 0x100) {
                         "x%02x".format(ch.code)
                     } else if (ch.code < 0x10000) {
                         "u%04x".format(ch.code)
                     } else {
                         "U%08x".format(ch.code)
                     })

    override fun compareTo(other: LObject): Int {
        if (other is LChar) {
            if (ch < other.ch) {
                return -1
            } else if (ch > other.ch) {
                return 1
            } else {
                return 0
            }
        } else {
            throw compareError(other)
        }
    }
    
}

fun makeChar(ch: Char) = LChar.mkChar(ch)
fun makeChar(s: String) = LChar.mkChar(s[0])
fun makeChar(code: Int): LObject = LChar.mkChar(code.toChar())
