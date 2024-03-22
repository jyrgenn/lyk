// load a file or string of Lisp source

package org.w21.lyk


fun load_file(fname: String, throw_error: Boolean = true,
              quiet: Boolean = false): LObject {
    var load_stream = FileReaderStream(fname)
    return load_stream(load_stream, fname, throw_error, quiet)
}

fun load_string(code: String, name: String, throw_error: Boolean = true,
                quiet: Boolean = false): LObject {
    var load_stream = StringReaderStream(code)
    return load_stream(load_stream, name, throw_error, quiet)
}

fun load_stream(load_stream: LStream, name: String,
         throw_error: Boolean = true, quiet: Boolean = false): LObject {
    var success = Nil
    
    withVariableAs(currentLoadFile, makeString(name)) {
        try {
            var error: LispError? = null
            val perfdata = measurePerfdata {
                error = repl(Reader(load_stream, name))
            }
            if (error != null) {
                if (throw_error) {
                    throw error as LispError
                } else {
                    printErr(error!!)
                }
                success = Nil
            } else {
                success = T
                if (!quiet) {
                    info("load $name: " + perfdata)
                }
            }
        } catch (e: Exception) {
            printErr(e)
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
