// I/O functions

package org.w21.lyk

import java.io.File


val directionKeyw = intern(":direction")
val inputKeyw = intern(":input")
val outputKeyw = intern(":output")
val ioKeyw = intern(":io")
val if_existsKeyw = intern(":if-exists")
val new_versionKeyw = intern(":new-version")
val overwriteKeyw = intern(":overwrite")
val supersedeKeyw = intern(":supersede")
val errorKeyw = intern(":error")
val if_does_not_existKeyw = intern(":if-does-not-exist")
val createKeyw = intern(":create")
val sepKeyw = intern(":sep")
val printKeyw = intern(":print")


/// builtin println
/// fun     bi_println
/// std     
/// key     "sep" to makeString(" ")
/// opt     
/// rest    args
/// ret     *the-non-printing-object*
/// special no
/// doc {
/// Print all arguments, separated by :sep, terminated by a newline.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_println(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
    stdout.println(args.joinToString(kwArgs[sepKeyw].toString()))
    return theNonPrintingObject
}

/// builtin print
/// fun     bi_print
/// std     arg
/// key     
/// opt     output-stream
/// rest    
/// ret     arg
/// special no
/// doc {
/// Print `arg` to `stream` (or standard output) suitable for input to (read),
/// with quoting and escaping where necessary, preceded by a newline and
/// followed by a blank.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_print(args: LObject, kwArgs: Map<LSymbol, LObject>,
             suppp: Map<LSymbol, Boolean>): LObject {
    val (arg, stream) = args2(args)
    outputStreamArg(stream, " stream").write("\n$arg ")
    return arg
}

/// builtin prin1
/// fun     bi_prin1
/// std     arg
/// key     
/// opt     output-stream
/// rest    
/// ret     arg
/// special no
/// doc {
/// Print `arg` to `stream` (or standard output) suitable for input to (read),
/// with quoting and escaping where necessary.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_prin1(args: LObject, kwArgs: Map<LSymbol, LObject>,
             suppp: Map<LSymbol, Boolean>): LObject {
    val (arg, stream) = args2(args)
    outputStreamArg(stream, " stream").write(arg.desc(null))
    return arg
}

/// builtin prin1-to-string
/// fun     bi_prin1_to_string
/// std     arg
/// key     
/// opt     
/// rest    
/// ret     string
/// special no
/// doc {
/// Print `arg` to a string as with prin1 and return the string.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_prin1_to_string(args: LObject, kwArgs: Map<LSymbol, LObject>,
                       suppp: Map<LSymbol, Boolean>): LObject {
    return makeString(arg1(args).desc(null))
}

/// builtin princ
/// fun     bi_princ
/// std     arg
/// key     
/// opt     output-stream
/// rest    
/// ret     arg
/// special no
/// doc {
/// Print `arg` to `stream` (or standard output) without quoting or escaping.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_princ(args: LObject, kwArgs: Map<LSymbol, LObject>,
             suppp: Map<LSymbol, Boolean>): LObject {
    val (arg, stream) = args2(args)
    outputStreamArg(stream, " stream").write(arg.toString())
    return arg
}

/// builtin princ-to-string
/// fun     bi_princs
/// std     arg
/// key     
/// opt     
/// rest    
/// ret     string
/// special no
/// doc {
/// Print `arg` to a string without quoting or escaping and return the string.
/// Also known as princs.
/// }
/// end builtin

/// builtin princs
/// fun     bi_princs
/// std     arg
/// key     
/// opt     
/// rest    
/// ret     string
/// special no
/// doc {
/// Print `arg` to a string without quoting or escaping and return the string.
/// Also known as princ-to-string.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_princs(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
    return makeString(arg1(args).toString())
}

/// builtin terpri
/// fun     bi_terpri
/// std     
/// key     
/// opt     output-stream
/// rest    
/// ret     nil
/// special no
/// doc {
/// Terminate a print line by sending a newline to the output-stream
/// (or standard output).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_terpri(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
    val stream = arg1(args)
    outputStreamArg(stream, " stream").println()
    return Nil
}

/// builtin load
/// fun     bi_load
/// std     filename
/// key     "verbose" to T, "print" to Nil, "error" to T
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Load specified file; return t if the contents was evaluated without error.
/// If the filename contains a slash, it is used exactly as given. Otherwise,
/// try to find the load file in the directories named in *load-path*. Try the
/// filename as given, then with a ".l" suffix, then with  a ".lisp" suffix.
///
/// If keyword verbose is nil (the default is true), do not print an
/// informational message after loading.
/// If keyword `error` is nil (the default is true), do not raise an error for
/// an unfound file, but return nil instead.
/// If keyword `print` is true, (the default is nil), the load progress is
/// shown by printing the values of the top-level forms evaluated. Otherwise,
/// the value of the variable *load-print* is used to determine printing.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_load(args: LObject, kwArgs: Map<LSymbol, LObject>,
            suppp: Map<LSymbol, Boolean>): LObject {
    val fname = arg1(args).toString()
    val verbose = kwArgs[verboseSym] !== Nil
    val throw_error = (kwArgs[errorSym] ?: T).toBoolean()
    val print = (kwArgs[printKeyw] ?: Nil).toBoolean()
        || (loadPrintSym.getValueOptional() ?: Nil).toBoolean()

    return load(fname, throw_error, print, verbose)
}

/// builtin make-string-input-stream
/// fun     bi_make_string_input_stream
/// std     string
/// key     
/// opt     
/// rest    
/// ret     stream
/// special no
/// doc {
/// Return a string input stream. This stream will supply, in order, the
/// characters in the string.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_make_string_input_stream(args: LObject, kwArgs: Map<LSymbol, LObject>,
                                suppp: Map<LSymbol, Boolean>): LObject {
    val string = stringArg(arg1(args))
    return StringReaderStream(string)
}

/// builtin stream
/// fun     bi_stream
/// std     object
/// key     
/// opt     
/// rest    
/// ret     stream
/// special no
/// doc {
/// Return an input stream from `object`.
/// If `object` is a stream, return it. Otherwise take the string
/// representation of `object` and make a stream from that.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_stream(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
    val arg = arg1(args)
    if (arg is LStream) {
        return arg
    }
    return StringReaderStream(arg.toString())
}

/// builtin format
/// fun     bi_format
/// std     destination format-string
/// key     
/// opt     
/// rest    args
/// ret     nil-or-string
/// special no
/// doc {
/// Format `args` according to `format-string` and write to `dest` (stream,
/// t, or nil). nil means return the result as a string, t means write to
/// standard output.
///
/// The format string is modeled after Common Lisp's, but lacks many of its
/// properties. Currently the following format directives are implemented:
///   ~A : Aesthetic
///   ~S : Standard
///   ~C : Character
///   ~F : Fixed format floating point
///   ~% : Newline
///   ~& : Fresh line
///   ~| : Page separator
///   ~~ : Tilde
///   ~#\Newline : Ignored newline
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_format(args: LObject, kwArgs: Map<LSymbol, LObject>,
              suppp: Map<LSymbol, Boolean>): LObject {
    val (dest, rest) = args
    val (fstring, f_args) = rest
    val stream = when (dest) {
        T -> stdout
        Nil -> StringWriterStream()
        is LStream -> dest
        else ->
            throw ArgumentError("format `dest` not nil or t or stream: $dest")
    }
    formatArgs(stream, stringArg(fstring, " format-string"), f_args)
    if (dest === Nil) {
        return makeString((stream as StringWriterStream).value_and_reset())
    }
    return Nil
}


/// builtin open
/// fun     bi_open
/// std     fname
/// key     "if-does-not-exist" to intern(":error"), "direction" to intern(":input"), "if-exists" to intern(":overwrite")
/// opt     
/// rest    
/// ret     stream
/// special no
/// doc {
/// Open a file (or reopen a stream) and return the connected stream.
/// Options: :direction followed by :input or :output or :io,
/// :if-exists followed by :new-version or :append or :overwrite
/// or :supersede or :error or nil
/// :if-does-not-exist followed by :error or :create or nil
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_open(args: LObject, kwArgs: Map<LSymbol, LObject>,
            suppp: Map<LSymbol, Boolean>): LObject {
    val fname = arg1(args).toString()    
    val direction = kwArgs[directionKeyw] ?: Nil
    val if_exists = kwArgs[if_existsKeyw] ?: Nil
    val if_does_not_exist = kwArgs[if_does_not_existKeyw] ?: Nil

    var inp = false
    var outp = false
    var return_nil = false

    when (direction) {
    inputKeyw ->  inp = true
    outputKeyw -> outp = true
    ioKeyw ->     { inp = true; outp = true }
    else ->
        throw ArgumentError("open :direction not :input or :output or :io : "
                            + direction.desc(null))
    }
    if (!inp && !outp) {
        throw ArgumentError("open :direction not :input or :output or :io : "
                            + direction.desc(null))
    }
    when (if_exists) {
        new_versionKeyw, overwriteKeyw, supersedeKeyw -> {}
        errorKeyw -> {}
        else -> {}
    }
    when (if_does_not_exist) {
        errorKeyw -> {}
        Nil -> return_nil = true
        else -> {}
    }
    try {
        if (inp) {
            return FileReaderStream(fname)
        }
        if (outp) {
            return FileWriterStream(fname)
        }
    } catch (e: Exception) {
        if (return_nil) {
            return Nil
        }
        throw IOError(e)
    }
    return Nil
}

/// builtin close
/// fun     bi_close
/// std     stream
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Close an open stream. Return t if the stream was open, nil else
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_close(args: LObject, kwArgs: Map<LSymbol, LObject>,
             suppp: Map<LSymbol, Boolean>): LObject {
    val stream = streamArg(arg1(args))
    try {
        return bool2ob(stream.close())
    } catch (e: Exception) {
        throw IOError(e)
    }
}

/// builtin read-line
/// fun     bi_read_line
/// std     
/// key     
/// opt     input-stream, eof-error-p T, eof-value, trim-nl
/// rest    
/// ret     line
/// special no
/// doc {
/// Read a line from *stdin* (or `input-stream`) and return it as a string.
/// If `eof-error-p` is true (which is the default), raise an error on EOF.
/// Otherwise, return `eof-value` instead.
/// If `trim-nl` is true, trim a trailin newline character from the line.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_read_line(args: LObject, kwArgs: Map<LSymbol, LObject>,
                 suppp: Map<LSymbol, Boolean>): LObject {
    val (input_stream, eof_error_p, eof_value, trim_nl) = args4(args)

    var stream = if (input_stream === Nil) {
        stdin
    } else {
        inputStreamArg(input_stream, " input-stream")
    }
    val line = stream.readLine(trim_nl.toBoolean())
    if (line == null) {
        if (eof_error_p.toBoolean()) {
            throw EOFError(stream)
        }
        return eof_value
    }
    return makeString(line)
}

/// builtin make-string-output-stream
/// fun     bi_make_string_output_stream
/// std     
/// key     
/// opt     
/// rest    
/// ret     string-stream
/// special no
/// doc {
/// Return a output string stream that stores the text that is output to it.
/// See get-output-stream-string for returning the contents of that stream.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_make_string_output_stream(args: LObject, kwArgs: Map<LSymbol, LObject>,
                                 suppp: Map<LSymbol, Boolean>): LObject {
    return StringWriterStream()
}

/// builtin get-output-stream-string
/// fun     bi_get_output_stream_string
/// std     string-output-stream
/// key     
/// opt     
/// rest    
/// ret     string
/// special no
/// doc {
/// Return a string with the contents written to `string-output-stream`.
/// This operation clears any characters on `string-output-stream`, so
/// the string contains only those characters which have been output since
/// the last call to get-output-stream-string or since the creation of the
/// string-output-stream, whichever occurred most recently. 
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_get_output_stream_string(args: LObject, kwArgs: Map<LSymbol, LObject>,
                                suppp: Map<LSymbol, Boolean>): LObject {
           val arg = arg1(args)
           if (arg !is StringWriterStream) {
               throw TypeError("get-output-stream-string argument is not an "
                               + "output stream string")
           }
           return makeString(arg.value_and_reset())
}

/// builtin console-reader-stream-p
/// fun     bi_console_reader_stream_p
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t iff `object` is a console reader stream, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_console_reader_stream_p(args: LObject, kwArgs: Map<LSymbol, LObject>,
                               suppp: Map<LSymbol, Boolean>): LObject {
    return bool2ob(arg1(args) is ConsoleReaderStream)
}


/// builtin file-reader-stream-p
/// fun     bi_file_reader_stream_p
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t iff `object` is a file reader stream, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_file_reader_stream_p(args: LObject, kwArgs: Map<LSymbol, LObject>,
                            suppp: Map<LSymbol, Boolean>): LObject {
    return bool2ob(arg1(args) is FileReaderStream)
}


/// builtin string-reader-stream-p
/// fun     bi_string_reader_stream_p
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t iff `object` is a string reader stream, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string_reader_stream_p(args: LObject, kwArgs: Map<LSymbol, LObject>,
                              suppp: Map<LSymbol, Boolean>): LObject {
    return bool2ob(arg1(args) is StringReaderStream)
}


/// builtin string-writer-stream-p
/// fun     bi_string_writer_stream_p
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t iff `object` is a string writer stream, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_string_writer_stream_p(args: LObject, kwArgs: Map<LSymbol, LObject>,
                              suppp: Map<LSymbol, Boolean>): LObject {
    return bool2ob(arg1(args) is StringWriterStream)
}


/// builtin file-writer-stream-p
/// fun     bi_file_writer_stream_p
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t iff `object` is a file writer stream, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_file_writer_stream_p(args: LObject, kwArgs: Map<LSymbol, LObject>,
                            suppp: Map<LSymbol, Boolean>): LObject {
    return bool2ob(arg1(args) is FileWriterStream)
}

// /// builtin file-io-stream-p
// /// fun     bi_file_io_stream_p
// /// std     object
// /// key     
// /// opt     
// /// rest    
// /// ret     t/nil
// /// special no
// /// doc {
// /// Return t iff `object` is a file io stream, nil else.
// /// }
// /// end builtin
// @Suppress("UNUSED_PARAMETER")
// fun bi_file_io_stream_p(args: LObject, kwArgs: Map<LSymbol, LObject>,
//                         suppp: Map<LSymbol, Boolean>): LObject {
//     return bool2ob(arg1(args) is FileIOStream)
// }

/// builtin streamp
/// fun     bi_streamp
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t iff `object` is a stream, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_streamp(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
    return bool2ob(arg1(args) is LStream)
}

/// builtin finish-output
/// fun     bi_finish_output
/// std     
/// key     
/// opt     output-stream
/// rest    
/// ret     nil
/// special no
/// doc {
/// Flush pending output to `output-stream` and then return.
/// If `output-stream` is not specified, use *stdout*.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_finish_output(args: LObject, kwArgs: Map<LSymbol, LObject>,
                     suppp: Map<LSymbol, Boolean>): LObject {
    val output_stream = arg1(args)
    val stream =
        if (output_stream === Nil) {
            stdout
        } else {
            outputStreamArg(output_stream)
        }
    stream.flush()
    return Nil
}

/// builtin open-interactive-stream
/// fun     bi_open_interactive_stream
/// std     
/// key     
/// opt     prompt makeString("")
/// rest    
/// ret     interactive-stream
/// special no
/// doc {
/// Return an interactive input stream.
/// This is a stream from which can be read using the interactive line
/// editor with `prompt`. If `prompt` is a function, it will be called
/// with no arguments for every line, and its return value will be used
/// as the line editor prompt.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_open_interactive_stream(args: LObject, kwArgs: Map<LSymbol, LObject>,
                               suppp: Map<LSymbol, Boolean>): LObject {
    return ConsoleReaderStream(prompt = arg1(args))
}

/// builtin input-stream-p
/// fun     bi_input_stream_p
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `object` is an input stream, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_input_stream_p(args: LObject, kwArgs: Map<LSymbol, LObject>,
                      suppp: Map<LSymbol, Boolean>): LObject {
    val obj = arg1(args)
    if (obj is LStream && obj.input) {
        return T
    }
    return Nil
}

/// builtin output-stream-p
/// fun     bi_output_stream_p
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `object` is an output stream, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_output_stream_p(args: LObject, kwArgs: Map<LSymbol, LObject>,
                      suppp: Map<LSymbol, Boolean>): LObject {
    val obj = arg1(args)
    if (obj is LStream && obj.output) {
        return T
    }
    return Nil
}

/// builtin interactive-stream-p
/// fun     bi_interactive_stream_p
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `object` is an interactive stream, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_interactive_stream_p(args: LObject, kwArgs: Map<LSymbol, LObject>,
                      suppp: Map<LSymbol, Boolean>): LObject {
    val obj = arg1(args)
    if (obj is ConsoleReaderStream) {
        return T
    }
    return Nil
}

/// builtin open-stream-p
/// fun     bi_open_stream_p
/// std     object
/// key     
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Return t if `object` is an open stream, nil else.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_open_stream_p(args: LObject, kwArgs: Map<LSymbol, LObject>,
                     suppp: Map<LSymbol, Boolean>): LObject {
    val obj = arg1(args)
    if (obj is LStream && obj.is_open) {
        return T
    }
    return Nil
}


val startKWSym = intern(":start")
val endKWSym = intern(":end")

fun write_string_line(args: LObject, kwArgs: Map<LSymbol, LObject>,
                      newline: Boolean): LString {
    val (obj, str) = args2(args)
    val start = kwArgs[startKWSym] ?: Nil
    val end = kwArgs[endKWSym] ?: Nil
    
    val string = if (obj is LString) {
        obj.the_string
    } else {
        obj.toString()
    }
    var print_string: String
    val stream = if (str === Nil) stdout else outputStreamArg(str, " stream")
    if (start !== Nil || end !== Nil) {
        val the_start = if (start === Nil) 0 else intArg(start,
                                                         " start keyword")
        if (end === Nil) {
            print_string = string.substring(the_start)
        } else {
            val the_end = min(string.length, intArg(end, " end keyword"))
            print_string = string.substring(the_start, the_end)
        }
    } else {
        print_string = string
    }
    stream.write(print_string)
    if (newline) {
        stream.println()
    }
    return makeString(string)
}

/// builtin write-string
/// fun     bi_write_string
/// std     string
/// key     
/// opt     output-stream
/// rest    
/// ret     string
/// special no
/// doc {
/// Write the `string` to `output-stream`.
/// Keyword parameters `start` and `end`, if specified, denote the start
/// and end positions of the portion of `string` being written.
/// Return `string`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_write_string(args: LObject, kwArgs: Map<LSymbol, LObject>,
                    suppp: Map<LSymbol, Boolean>): LObject {
    return write_string_line(args, kwArgs, newline = false)
}

/// builtin write-line
/// fun     bi_write_line
/// std     string
/// key     "start" to Nil, "end" to Nil
/// opt     output-stream
/// rest    
/// ret     string
/// special no
/// doc {
/// Write the `string` to `output-stream`, then output a newline afterwards. 
/// Keyword parameters `start` and `end`, if specified, denote the start
/// and end positions of the portion of `string` being written.
/// Return `string`.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_write_line(args: LObject, kwArgs: Map<LSymbol, LObject>,
                    suppp: Map<LSymbol, Boolean>): LObject {
    return write_string_line(args, kwArgs, newline = true)
}

