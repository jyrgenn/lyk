package org.w21.lyk

import java.io.File
import java.lang.ref.WeakReference

val stdinName   = "*stdin*"
val stdoutName  = "*stdout*"
val stderrName  = "*stderr*"
val consoleName = "*console*"
val stdinPath   = "/dev/stdin"
val stdoutPath  = "/dev/stdout"
val stderrPath  = "/dev/stderr"
val consolePath = "/dev/tty"
val newLine = 10


val openStreams = mutableSetOf<WeakReference<LStream>>()


class StringWriterStream(name: String? = null
): LStream(output = true, name = name) {
    var content = StrBuf()
    
    override fun write(code: Int) {
        content.add(code.toChar())
    }

    override fun write(s: String) {
        content.add(s)
    }

    fun value_and_reset(): String {
        // This operation clears any characters on string-output-stream, so the
        // string contains only those characters which have been output since
        // the last call to get-output-stream-string or since the creation of
        // the string-output-stream, whichever occurred most recently.
        // [CLHS, Function GET-OUTPUT-STREAM-STRING]
        val s = content.toString()
        content = StrBuf()
        return s
    }

    override fun close_specific() {}
}


class StringReaderStream(content: String, name: String? = ""):
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
    
    override fun close_specific() {}
}

class FileReaderStream(file: File, name: String? = null):
    LStream(input = true, path = file.path, name = name ?: file.path,
            error = false)
{
    constructor(pathname: String, name: String? = null):
        this(File(pathname), name = name ?: pathname)

    constructor(dir: String, fname: String, name: String? = null):
        this(File(dir, fname), name = name ?: fname)

    val fileReader = file.bufferedReader()

    override fun read(): Char? {
        try {
            val c = fileReader.read()       // returns an Int!
            if (c < 0) {                    // EOF as in C
                return null
            }
            return c.toChar()
        } catch (e: Exception) {
            throw IOError(e)
        }
    }
    
    override fun close_specific() {
        fileReader.close()
    }
}

class FileWriterStream(path: String,
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

    override fun write(code: Int) {
        try {
            fileWriter.write(code)
            if (flushch || (flushln && code == newLine)) {
                fileWriter.flush()
            }
        } catch (e: Exception) {
            throw IOError(e)
        }
    }
    override fun write(s: String) {
        try {
            fileWriter.write(s)
            if (flushch) {
                fileWriter.flush()
            }
        } catch (e: Exception) {
            throw IOError(e)
        }
    }

    override fun flush() {
        try {
            fileWriter.flush()
        } catch (e: Exception) {
            throw IOError(e)
        }
    }

    override fun close_specific() {
        try {
            fileWriter.close()
        } catch (e: Exception) {
            throw IOError(e)
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
    
    init {
        openStreams.add(WeakReference(this))
    }

    open fun read(): Char? {          // the actual reading
        throw ArgumentError("read on $this")
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
    open fun flush() {}
    
    open fun readLine(trimNewline: Boolean): String? {
        val sb = StrBuf()
        while (true) {
            val ch = read()
            when (ch) {
                newLine.toChar() -> {
                    if (!trimNewline) {
                        sb.add(ch)
                    }
                    return sb.toString()
                }
                null -> return if (sb.size > 0) sb.toString() else null
                else -> sb.add(ch)
            }
        }
    }

    open fun print(vararg things: Any, separator: String = " ") {
        val last = things.size - 1
        var n = 0
        for (thing in things) {
            write(thing.toString())
            if (n++ < last) {
                write(separator)
            }
        }
    }
    
    open fun printf(format_string: String, vararg format_args: Any) {
        write(format_string.format(*format_args))
    }
    
    open fun println(vararg things: Any, separator: String = " ") {
        print(*things, separator = separator)
        write(newLine)
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

    abstract fun close_specific()

    open fun close(): Boolean {
        val result = is_open
        is_open = false
        close_specific()
        openStreams.minus(WeakReference(this))
        return result
    }

    override fun toString(): String {
        val x = if (is_open) "" else "x"
        val i = if (input) "I" else ""
        val o = if (output) "O" else ""
        val e = if (error) "E" else ""
        return "#<${typeOf(this)}[$i$o$e$x]$name>"
    }
    override fun desc() = toString()

}

fun closeAllStreams() {
    for (ref in openStreams) {
        try {
            val stream = ref.get()
            if (stream != null) {
                stream.close()
            }
        } catch (e: Exception) {}
    }
}

fun init_Streams() {
    atexit(::closeAllStreams)
}
