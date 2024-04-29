// load a file or string of Lisp source

package org.w21.lyk

import java.io.File


// load file, possibly from *load-path*, with suffixes
fun load(fname: String, throw_error: Boolean, print: Boolean, verbose: Boolean
): LObject {
    val suffixes = listOf("", ".l", ".lisp")
    val loadpath = loadPathSym.getValueOptional() ?: Nil

    fun trySuffixes(dir: String, fname: String): LObject? {
        for (suffix in suffixes) {
            val fullname = fname + suffix
            if (File(dir, fullname).exists()) {
                return load_file(dir, fullname, throw_error, !verbose, print,
                                 debugline = Options.debug[debugLoadlineSym]!!)
            }
        }
        return null
    }

    if (fname.contains("/")) {
        if (File(fname).exists()) {
            return load_file(fname, throw_error, !verbose, print,
                             debugline = Options.debug[debugLoadlineSym]!!)
        }
        throw IOError("could not find load file: $fname")
    }

    // no slash in name, so try the load path
    for (dir in loadpath) {
        val result = trySuffixes(dir.toString(), fname)
        if (result != null) {
            return result
        }
    }
    throw IOError("could not find load file: $fname")
}

// return the result value of the load, and a Boolean if the file was actually
// found
fun load_file(pathname: String, throw_error: Boolean = true,
              quiet: Boolean = false, print: Boolean = false,
              debugline: Boolean = false
): LObject {
    return load_stream(FileReaderStream(pathname, debugline = debugline),
                       pathname, throw_error, quiet, print)
}

// return the result value of the load, and a Boolean if the file was actually
// found
fun load_file(dir: String, fname: String, throw_error: Boolean = true,
              quiet: Boolean = false, print: Boolean = false,
              debugline: Boolean = false
): LObject {
    
    return load_stream(FileReaderStream(dir, fname, debugline = debugline),
                       dir + "/" + fname, throw_error, quiet, print)
}

fun load_string(code: String, name: String, throw_error: Boolean = true,
                quiet: Boolean = false, print: Boolean = false): LObject {
    return load_stream(StringReaderStream(code, name = name),
                       name, throw_error, quiet, print)
}

fun load_stream(load_stream: LStream, name: String,
                throw_error: Boolean, quiet: Boolean, print: Boolean): LObject {
    var success = Nil

    debug(debugLoadlineSym) { "loading $name" }
    withVariableAs(loadPathnameSym, makeString(name)) {
        try {
            var error: LispError? = null
            val perfdata = measurePerfdata {
                error = repl(Reader(load_stream, name), print = print)
            }
            if (error != null) {
                if (throw_error) {
                    throw error as LispError
                }
                success = Nil
            } else {
                success = T
                if (!quiet) {
                    info("load $name: " + perfdata)
                }
            }
        } catch (e: Exception) {
            debug(debugLoadlineSym) { "$name: $e" }
            throw e
        } finally {
            load_stream.close()
            debug(debugLoadlineSym) { "finished $name" }
        }
    }
    return success
}
