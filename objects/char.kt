// the character object type

package org.w21.lyk

import java.util.WeakHashMap


class LChar(val the_char: Char): LObject() {

    override val type = "char"

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
        val nameChar = mapOf(
            "space"     to ' ',
            "newline"   to '\n',
            "linefeed"  to '\n',
            "tab"       to '\t',
            "page"      to '\u000c',
            "rubout"    to '\u007f',
            "return"    to '\r',
            "backspace" to '\u0008',
            "bell"      to '\u0007',
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
        if (the_char == other.the_char) {
            return true
        }
        return false
    }
    
    override fun isAtom() = true

    override fun toString() = the_char.toString()

    fun ascii_glyph() =
        if (the_char.code in 33..126) {
            the_char.toString()
        } else {
            null
        }

    val code: Int
        get () = the_char.code

    override fun desc(seen: Set<Int>?) = 
        "#\\" + (charName[the_char] ?:
                     ascii_glyph() ?:
                     if (isPrintable(the_char)) {
                         the_char
                     } else if (the_char.code < 0x100) {
                         "x%02x".format(the_char.code)
                     } else if (the_char.code < 0x10000) {
                         "u%04x".format(the_char.code)
                     } else {
                         "U%08x".format(the_char.code)
                     })

    override fun compareTo(other: LObject): Int {
        if (other is LChar) {
            if (the_char < other.the_char) {
                return -1
            } else if (the_char > other.the_char) {
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
// fun makeChar(s: String) = LChar.mkChar(s[0])
fun makeChar(code: Int): LObject = LChar.mkChar(code.toChar())
