package org.w21.lyk

import java.io.File

val stdinName  = "*stdin*"
val stdoutName = "*stdout*"
val stderrName = "*stderr*"
val stdinPath  = "/dev/stdin"
val stdoutPath = "/dev/stdout"
val stderrPath = "/dev/stderr"
val newLine = 10


class StdinStream(name: String = stdinName):
    FileReaderStream(stdinPath, name)
class StdoutStream(name: String = stdoutName):
    FileWriterStream(stdoutPath, name, flushln = true)
class StderrStream(name: String = stderrName):
    FileWriterStream(stderrPath, name, flushch = true, error = true)

open class StringReaderStream(content: String, name: String? = null):
    Stream(input = true, path = null, name = name)
{
    val chars = content.toCharArray()
    var nextpos = 0

    override fun read(): Char? {
        if (nextpos < chars.size) {
            return chars[nextpos]++
        }
        return null
    }
    
    override fun write(ch: Char) {
        throw IOError("write on input stream $this")
    }
    override fun write(s: String) {
        throw IOError("write on input stream $this")
    }
    override fun println(s: String) {
        throw IOError("write on input stream $this")
    }
}

open class FileReaderStream(path: String, name: String? = null):
    Stream(input = true, path = path, name = name ?: path)
{
    val fileReader = File(path).bufferedReader()

    override fun read(): Char? {
        val c = fileReader.read()       // returns an Int!
        if (c < 0) {                    // EOF as in C
            return null
        }
        return c.toChar()
    }
    
    override fun write(ch: Char) {
        throw IOError("write on input stream $this")
    }
    override fun write(s: String) {
        throw IOError("write on input stream $this")
    }
    override fun println(s: String) {
        throw IOError("write on input stream $this")
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
): Stream(output = true, path = path, name = name ?: path, error = error)
{
    val fileWriter = File(path).bufferedWriter()

    override fun read(): Char? {
        throw IOError("read on output stream $this")
    }
    
    override fun write(ch: Char) {
        fileWriter.write(ch.code)
        if (flushch) {
            fileWriter.flush()
        }
    }
    override fun write(s: String) {
        fileWriter.write(s)
        if (flushch) {
            fileWriter.flush()
        }
    }

    override fun println(s: String) {
        fileWriter.write(s)
        fileWriter.write(newLine)
        if (flushch || flushln) {
            fileWriter.flush()
        }
    }
}

abstract class Stream(
    val input: Boolean = false,         // is input stream?
    val output: Boolean = false,        // is output stream?
    val error: Boolean = false,         // i.e. stderr
    val name: String? = null,           // pathname or *stdin/out/err* or
                                        // network something
    val path: String? = null,           // file pathname
    val append: Boolean = false,        // "a"
): LispObject()
{
    var charUnread: Char? = null

    abstract fun read(): Char?          // the actual reading
    abstract fun write(ch: Char)
    abstract fun write(s: String)
    abstract fun println(s: String)
    
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

    override fun toString(): String {
        val i = if (input) "I" else ""
        val o = if (output) "O" else ""
        val e = if (error) "E" else ""
        return "#<${type()}[$i$o$e]$name>"
    }
    override fun desc() = toString()
}
