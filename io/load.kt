// load a file or string of Lisp source

package org.w21.lyk


// return the result value of the load, and a Boolean if the file was actually
// found
fun load_file(pathname: String, throw_error: Boolean = true,
              quiet: Boolean = false, print: Boolean = false
): Pair<LObject, Boolean> {
    try {
        var file_stream = FileReaderStream(pathname)
        return Pair(load_stream(file_stream, pathname,
                                throw_error, quiet, print),
                    true)
    } catch (e: java.io.FileNotFoundException) {
        return Pair(Nil, false)
    }
}

// return the result value of the load, and a Boolean if the file was actually
// found
fun load_file(dir: String, fname: String, throw_error: Boolean = true,
              quiet: Boolean = false, print: Boolean = false
): Pair<LObject, Boolean> {
    try {
        var file_stream = FileReaderStream(dir, fname)
        return Pair(load_stream(file_stream, dir + "/" + fname, throw_error,
                                quiet, print),
                    true)
    } catch (e: java.io.FileNotFoundException) {
        return Pair(Nil, false)
    }
}

fun load_string(code: String, name: String, throw_error: Boolean = true,
                quiet: Boolean = false, print: Boolean = false): LObject {
    var load_stream = StringReaderStream(code)
    return load_stream(load_stream, name, throw_error, quiet, print)
}

fun load_stream(load_stream: LStream, name: String,
                throw_error: Boolean, quiet: Boolean, print: Boolean): LObject {
    var success = Nil
    
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
        } finally {
            load_stream.close()
        }
    }
    return success
}
