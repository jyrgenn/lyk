package org.w21.lyk

import java.io.File
import java.lang.ref.WeakReference

import java.util.TreeSet
import jline.console.ConsoleReader
import jline.console.history.FileHistory
import jline.console.completer.Completer


val stdinName   = "*standard-input*"
val stdoutName  = "*standard-output*"
val stderrName  = "*error-output*"
val consoleName = "*terminal-io*"
val stdinPath   = "/dev/stdin"
val stdoutPath  = "/dev/stdout"
val stderrPath  = "/dev/stderr"
val consolePath = "/dev/tty"
val devNullPath = "/dev/null"
val newLine = 10


val openStreams = mutableSetOf<WeakReference<LStream>>()


/**
 * Completer for Lisp symbols.
 *
 * @author <a href="mailto:ni@w21.org">Jürgen Nickelsen/a>
 * based on StringsCompleter by
 * <a href="mailto:jason@planet57.com">Jason Dillon</a>
 */
class SymbolsCompleter(symbol_names: Set<String>): Completer
{
    val names = TreeSet<String>();

    init {
        for (name in symbol_names) {
            names.add(name)
        }
    }

    override fun complete(buffer: String?, cursor: Int,
                 candidates: MutableList<CharSequence>): Int {
        if (buffer == null) {
            for (s in names) {
                candidates.add(s);
            }
        } else {
            var index = cursor
            debug(debugCompleterSym) {
                "buffer: \"$buffer\" len ${buffer.length} cursor $cursor"
            }
            while (index >= 0) {
                if (index < buffer.length && isDelimiter(buffer[index])) {
                    break
                }
                index--
            }
            val prefix = buffer.substring(0, min(cursor, index+1))
            val sym = buffer.substring(min(cursor, index+1), cursor)
            for (match in names.tailSet(sym)) {
                if (!match.startsWith(sym)) {
                    break;
                }
                candidates.add(prefix + match);
            }
        }

        return if (candidates.isEmpty()) -1 else 0;
    }
}


class ConsoleReaderStream(var prompt: LObject = Nil):
    ReaderStream(consoleName, null, true)
{
    val cr = ConsoleReader()
    var linebuf = StringReaderStream("")
    var linenum = 0
    var completer: SymbolsCompleter? = null
    val history_file = historyFilenameSym.getValueOptional()
    val history = if (history_file != null) {
        FileHistory(File(history_file.toString()))
    } else {
        null
    }

    init {
        cr.setExpandEvents(false)
        cr.setHistory(history)
    }

    override val obtype = "console-reader-stream"

    override fun read_location() = "$name:$linenum"

    override fun read(): Char? {
        try {
            if (!linebuf.hasNext()) {
                val currentPrompt = prompt
                val promptString = when(currentPrompt) {
                    Nil -> ""
                    is LFunction -> currentPrompt.call(Nil).toString()
                    else -> currentPrompt.toString()
                }
                if (completer != null) {
                    cr.removeCompleter(completer)
                }
                completer = SymbolsCompleter(symbolTable.keys)
                cr.addCompleter(completer)
                val line = cr.readLine(promptString)
                linenum++
                if (line == null) {
                    return null
                }
                history?.add(line)
                linebuf = StringReaderStream(line + "\n")
            }
            return linebuf.read()
        } catch (e: Exception) {
            throw IOError(e)
        }
    }

    override fun setPrompt(newPrompt: String) {
        prompt = makeString(newPrompt)
    }

    override fun close_specific() {
        history?.flush()
    }
}


class StringWriterStream(name: String? = null):
    WriterStream(name ?: "*string-writer*", null, false, false)
{
    var content = StrBuf()
    
    override val obtype = "string-writer-stream"

    override fun write(code: Int) {
        // stderr.println("added $code")
        lastChar = code.toChar()
        content.add(lastChar!!.toString())
    }

    override fun write(s: String) {
        // stderr.println("added $s")
        if (s.length > 0) {
            lastChar = s[s.length-1]
            content.add(s)
        }
    }

    fun value_and_reset(): String {
        // This operation clears any characters on string-output-stream, so the
        // string contains only those characters which have been output since
        // the last call to get-output-stream-string or since the creation of
        // the string-output-stream, whichever occurred most recently.
        // [CLHS, Function GET-OUTPUT-STREAM-STRING]
        val s = content.toString()
        content = StrBuf()
        // stderr.println("value and reset: $s")
        return s
    }

    override fun toString(): String {
        val x = if (is_open) "" else "x"
        val i = if (input) "I" else ""
        val o = if (output) "O" else ""
        val e = if (error) "E" else ""
        return "#<${this.obtype}[$i$o$e$x:${content.size}]$name>"
    }    

    override fun close_specific() {}
}


class StringReaderStream(val content: String, name: String? = "",
                         var lineno: Int = 1,
                         val debugline: Boolean = false):
    ReaderStream(name ?: "", null, false)
{
    var nextpos = 0
    var current_name = name
    // var linenum = lineno
    var linebegin = true

    override val obtype = "string-reader-stream"

    override fun read_location() = "$current_name:$lineno"

    override fun read(): Char? {
        if (hasNext()) {
            if (linebegin && debugline) {
                debug(debugLoadlineSym) {
                    val line = content.substring(nextpos,
                                                 content.indexOf('\n', nextpos))
                    "$name:$lineno \"$line\""
                }
            }
            linebegin = false
            val char = content[nextpos]
            if (char == '\n') {
                lineno++
                // ^;#file filename indicator in preload string
                if (lookingAt(content, nextpos + 1, loadFileNameInd)) {
                    current_name =
                        restOfLine(content,
                                   nextpos + 1 + loadFileNameInd.length).trim()
                    lineno = 0
                }
                linebegin = true
            }
            nextpos++
            return char
        }
        return null
    }

    fun hasNext() = is_open && nextpos < content.length
    
    override fun toString(): String {
        val x = if (is_open) "" else "x"
        val i = if (input) "I" else ""
        val o = if (output) "O" else ""
        val e = if (error) "E" else ""
        val cl = content.length
        val rem = cl - nextpos
        return "#<${this.obtype}[$i$o$e$x:$rem/$cl]$name>"
    }    override fun close_specific() {}
}

class FileReaderStream(file: File, name: String? = null,
                       val debugline: Boolean = false):
    ReaderStream(name ?: file.path, file.path, false)
{
    constructor(pathname: String, name: String? = null,
                debugline: Boolean = false):
        this(File(pathname), name = name ?: pathname, debugline = debugline)

    val fileReader = file.bufferedReader()
    var linebuf = StringReaderStream("", name = name)
    var linenum = 0

    override val obtype = "file-reader-stream"

    override fun read_location() = linebuf.read_location()

    override fun read(): Char? {
        try {
            if (!linebuf.hasNext()) {
                val line = fileReader.readLine()
                linenum++
                if (line == null) {
                    return null
                }
                if (debugline) {
                    debug(debugLoadlineSym) {
                        "$name:$linenum \"$line\""
                    }
                }
                linebuf = StringReaderStream(line + "\n", name = name)
            }
            return linebuf.read()
        } catch (e: Exception) {
            throw IOError(e)
        }
    }
    
    override fun close_specific() {
        fileReader.close()
    }
}

// class FileIOStream(path: String,
//                             name: String? = null,
//                             val flushln: Boolean = false,
//                             val flushch: Boolean = false,
//                             error: Boolean = false,
//                             // append: Boolean = false,
//                             // create: Boolean = true,
//                             // exclusive: Boolean = false
// ): LStream(input = true, output = true, path = path,
//            name = name ?: "'$path'", error = error)
// {
//     val file = File(path)
//     val fileWriter = file.bufferedWriter()
//     val fileReader = file.bufferedReader()
//     var linebuf = StringReaderStream("", name = name)

//     override val obtype = "file-io-stream"

//     override fun read_location() = linebuf.read_location()

//     override fun read(): Char? {
//         try {
//             if (!linebuf.hasNext()) {
//                 val line = fileReader.readLine()
//                 if (line == null) {
//                     return null
//                 }
//                 linebuf = StringReaderStream(line + "\n", name = name)
//             }
//             return linebuf.read()
//         } catch (e: Exception) {
//             throw IOError(e)
//         }
//     }
    
//     override fun write(code: Int) {
//         try {
//             fileWriter.write(code)
//             lastChar = code.toChar()
//             if (flushch || (flushln && code == newLine)) {
//                 fileWriter.flush()
//             }
//         } catch (e: Exception) {
//             throw IOError(e)
//         }
//     }
//     override fun write(s: String) {
//         try {
//             if (s.length > 0) {
//                 fileWriter.write(s)
//                 lastChar = s[s.length-1]
//                 if (flushch || (flushln && lastChar == '\n')) {
//                     fileWriter.flush()
//                 }
//             }
//         } catch (e: Exception) {
//             throw IOError(e)
//         }
//     }

//     override fun flush() {
//         try {
//             fileWriter.flush()
//         } catch (e: Exception) {
//             throw IOError(e)
//         }
//     }

//     override fun close_specific() {
//         try {
//             fileReader.close()
//             fileWriter.close()
//         } catch (e: Exception) {
//             throw IOError(e)
//         }
//     }

// }

class FileWriterStream(path: String,
                            name: String? = null,
                            val flushln: Boolean = false,
                            val flushch: Boolean = false,
                            error: Boolean = false,
                            // append: Boolean = false,
                            // create: Boolean = true,
                            // exclusive: Boolean = false
): WriterStream(name ?: "'$path'", path, error, false){
    val fileWriter = File(path).bufferedWriter()

    override val obtype = "file-writer-stream"

    override fun write(code: Int) {
        try {
            fileWriter.write(code)
            lastChar = code.toChar()
            if (flushch || (flushln && code == newLine)) {
                fileWriter.flush()
            }
        } catch (e: Exception) {
            throw IOError(e)
        }
    }
    override fun write(s: String) {
        try {
            if (s.length > 0) {
                fileWriter.write(s)
                lastChar = s[s.length-1]
                if (flushch || (flushln && lastChar == '\n')) {
                    fileWriter.flush()
                }
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

abstract class ReaderStream(name: String, path: String?, interactive: Boolean):
    LStream(input = true, output = false, error = false, name = name,
            path = path, append = false, interactive = interactive)
{
    open fun readLine(trimNewline: Boolean = false): String? {
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

    abstract fun read_location(): String

    open fun setPrompt(newPrompt: String) {}

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

    
}

abstract class WriterStream(name: String, path: String?,
                            is_error: Boolean, append: Boolean = false):
    LStream(false, true, is_error, name, path, append, false)
{
    open fun flush() {}
    
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
    
    open fun write(ch: Char) {
        write(ch.code)
    }
    open fun write(buf: CharArray, n: Int) {
        for (i in 0 ..< n) {
            write(buf[i])
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
    val interactive: Boolean = false,
): LObject()
{
    var charUnread: Char? = null
    var is_open = true
    var lastChar: Char? = null
    
    init {
        openStreams.add(WeakReference(this))
    }

    
    // return true if the last character written was a newline
    fun newlineLast(): Boolean {
        return lastChar != null && lastChar == '\n'
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
        return "#<${this.obtype}[$i$o$e$x]$name>"
    }
    override fun desc(seen: Set<Int>?) = toString()

    fun finalize() {
        debug(debugFinalizeSym) {
            "$this"
        }
        close()
    }
}

fun closeAllStreams() {
    for (ref in openStreams) {
        try {
            val stream = ref.get()
            if (stream != null) {
                debug(debugAtexitSym) {
                    "close $stream"
                }
                stream.close()
            }
        } catch (e: Exception) {}
    }
}

fun init_Streams() {
    atexit(::closeAllStreams)
}
