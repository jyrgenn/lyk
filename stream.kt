package org.w21.lyk



class Stream(
    val input: Boolean = true,       // is input stream?
    val output: Boolean = false,     // is output stream?
    val error: Boolean = false,      // i.e. stderr
    val name: String? = null, // pathname or *stdin/out/err* or
    // network something
    val path: String? = null, // file pathname
    val append: Boolean = false,
): LispObject() {

    var charUnread: Char? = null

    fun readChar(): Char? {
        if (charUnread != null) {
            val ch = charUnread!!
            charUnread = null
            return ch
        }
        return 'a'
    }

    fun unreadChar(ch : Char) {
        charUnread = ch
    }
}
