// load a file or string of Lisp source

package org.w21.lyk


fun load_file(fname: String, verbose: Boolean = false,
              throw_error: Boolean = true): LObject {
    var load_stream = FileReaderStream(fname)
    return load(load_stream, fname, verbose, throw_error)
}

fun load(load_stream: LStream, name: String, verbose: Boolean = false,
         throw_error: Boolean = true): LObject {
    var success = Nil
    
    withVariableAs(currentLoadFile, makeString(name)) {
        try {
            var error: LispError? = null
            val perfdata = measurePerfdata {
                error = repl(Reader(load_stream, name))
            }
            if (error != null) {
                if (verbose) {
                    printErr(error!!)
                }
                if (throw_error) {
                    throw error as LispError
                }
                success = Nil
            } else {
                success = T
                if (verbose) {
                    info("load $name: " + perfdata)
                }
            }
        } catch (e: Exception) {
            if (verbose) {
                printErr(e)
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
