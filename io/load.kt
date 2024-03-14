// load a file or string of Lisp source

package org.w21.lyk


fun load_file(fname: String, verbose: Boolean = false,
              throw_error: Boolean = true): LispObject {
    var load_stream = FileReaderStream(fname)
    return load(load_stream, fname, verbose, throw_error)
}

fun load(load_stream: Stream, name: String, verbose: Boolean = false,
         throw_error: Boolean = true): LispObject {
    var success = Nil
    
    withVariableAs(currentLoadFile, LispString.makeString(name)) {
        try {
            var error: LispError? = null
            val perfdata = measurePerfdata {
                error = repl(Reader(load_stream, name))
            }
            if (error != null) {
                if (verbose) {
                    stderr.println(error.toString())
                }
                if (throw_error) {
                    throw error as LispError
                }
                success = Nil
            } else {
                success = T
                if (verbose) {
                    stderr.println("; load $name: " + perfdata)
                }
            }
        } catch (e: Exception) {
            if (verbose) {
                stderr.println(e.toString())
            }
            if (throw_error) {
                throw e
            }
            success = Nil
        } finally {
            load_stream.close()
        }
    }
    return success
}
