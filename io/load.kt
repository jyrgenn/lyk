// load a file of Lips source

package org.w21.lyk


fun load_file(fname: String, verbose: Boolean = false,
              throw_error: Boolean = true): LispObject {
    var load_stream = FileReaderStream(fname)
    return load(load_stream, fname, verbose, throw_error)
}

fun load(load_stream: Stream, name: String, verbose: Boolean = false,
         throw_error: Boolean = true): LispObject {
    val pairsBefore = pairCounter
    val evalsBefore = evalCounter

    var success = Nil
    
    withVariableAs(currentLoadFile, LispString.makeString(name)) {
        try {
            val error = repl(Reader(load_stream, name))
            if (error != null) {
                if (verbose) {
                    stderr.println(error.toString())
                }
                if (throw_error) {
                    throw error
                }
                success = Nil
            } else {
                success = T
                if (verbose) {
                    val pairs = pairCounter - pairsBefore
                    val evals = evalCounter - evalsBefore
                    stderr.println("; load $name: $pairs pairs, $evals evals")
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
