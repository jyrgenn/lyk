package org.w21.lyk

import java.io.File

val stdinName   = "*stdin*"
val stdoutName  = "*stdout*"
val stderrName  = "*stderr*"
val consoleName = "*console*"
val stdinPath   = "/dev/stdin"
val stdoutPath  = "/dev/stdout"
val stderrPath  = "/dev/stderr"
val consolePath = "/dev/tty"
val newLine = 10


open class StringReaderStream(content: String, name: String? = ""):
    LStream(input = true, path = null, name = name)
{
    val chars = content.toCharArray()
    var nextpos = 0

    override fun read(): Char? {
        if (is_open && nextpos < chars.size) {
            return chars[nextpos++]
        }
        return null
    }
    
    override fun close(): Boolean {
        return super.close()
    }
}

open class FileReaderStream(path: String, name: String? = null,
                            error: Boolean = true):
    LStream(input = true, path = path, name = name ?: path, error = error)
{
    val fileReader = File(path).bufferedReader()

    override fun read(): Char? {
        try {
            val c = fileReader.read()       // returns an Int!
            if (c < 0) {                    // EOF as in C
                return null
            }
            return c.toChar()
        } catch (e: Exception) {
            throw IOError("reading $this", e)
        }
    }
    
    override fun close(): Boolean {
        fileReader.close()
        return super.close()
    }
}

open class FileWriterStream(path: String,
                            name: String? = null,
                            val flushln: Boolean = false,
                            val flushch: Boolean = false,
                            error: Boolean = false,
                            // append: Boolean = false,
                            // create: Boolean = true,
                            // exclusive: Boolean = false
): LStream(output = true, path = path, name = name ?: "'$path'", error = error)
{
    val fileWriter = File(path).printWriter().buffered()

    override fun read(): Char? {
        throw ArgumentError("read on output stream $this")
    }
    
    override fun write(code: Int) {
        try {
            fileWriter.write(code)
            if (flushch || (flushln && code == newLine)) {
                fileWriter.flush()
            }
        } catch (e: Exception) {
            throw IOError("writing $this", e)
        }
    }
    override fun write(s: String) {
        try {
            fileWriter.write(s)
            if (flushch) {
                fileWriter.flush()
            }
        } catch (e: Exception) {
            throw IOError("writing $this", e)
        }
    }

    fun flush() {
        try {
            fileWriter.flush()
        } catch (e: Exception) {
            throw IOError("flushing $this", e)
        }
    }

    override fun close(): Boolean {
        try {
            fileWriter.close()
            return super.close()
        } catch (e: Exception) {
            throw IOError("closing $this", e)
        }
    }
}

abstract class LStream(
    val input: Boolean = false,         // is input stream?
    val output: Boolean = false,        // is output stream?
    val error: Boolean = false,         // i.e. stderr
    val name: String? = null,           // pathname or *stdin/out/err* or
                                        // network something
    val path: String? = null,           // file pathname
    val append: Boolean = false,        // "a"
): LObject()
{
    var charUnread: Char? = null
    var is_open = true

    open fun read(): Char? {          // the actual reading
        throw ArgumentError("write on $this")
    }
    open fun write(code: Int) {
        throw ArgumentError("write on $this")
    }
    open fun write(s: String) {
        throw ArgumentError("write on $this")
    }
    open fun write(ch: Char) {
        write(ch.code)
    }
    
    open fun print(thing: Any) {
        write(thing.toString())
    }
    open fun println(vararg things: Any) {
        val last = things.size - 1
        var n = 0
        for (thing in things) {
            write(thing.toString())
            if (n++ == last) {
                write(newLine)
            }
        }
    }
    
    open fun println() {
        write(newLine)
    }
    
    fun readChar(): Char? {
        if (charUnread != null) {
            val ch = charUnread
            charUnread = null
            return ch
        }
        return read()
    }

    fun unreadChar(ch : Char) {
        charUnread = ch
    }

    open fun close(): Boolean {
        val result = is_open
        is_open = false
        return result
    }

    override fun toString(): String {
        val x = if (is_open) "" else "x"
        val i = if (input) "I" else ""
        val o = if (output) "O" else ""
        val e = if (error) "E" else ""
        return "#<${type()}[$i$o$e$x]$name>"
    }
    override fun desc() = toString()
}
