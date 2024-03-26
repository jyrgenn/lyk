// I/O functions

package org.w21.lyk

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
fun bi_println(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
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
fun bi_print(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (arg, stream) = args2(args)
    outputStreamArg(stream, "print stream").write("\n${arg.desc()} ")
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
fun bi_prin1(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (arg, stream) = args2(args)
    outputStreamArg(stream, "prin1 stream").write(arg.desc())
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
fun bi_prin1_to_string(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    return makeString(arg1(args).desc())
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
fun bi_princ(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (arg, stream) = args2(args)
    outputStreamArg(stream, "princ stream").write(arg.toString())
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
fun bi_princs(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
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
fun bi_terpri(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val stream = arg1(args)
    outputStreamArg(stream, "terpri stream").println()
    return Nil
}

/// builtin warning
/// fun     bi_warning
/// std     format-string
/// key     
/// opt     
/// rest    format-args
/// ret     nil
/// special no
/// doc {
/// Raise a warning message with `format string` and format arguments.
/// If warnings are treated as errors (i.e. *warnings-as-errors* is true), the
/// warning exits all active calls immediately, except for errset. Otherwise,
/// only the message is printed as a warning, formatted as specified.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_warning(args: LObject, kwArgs: Map<LSymbol, LObject>
): LObject {
    // TODO: warnings as errors
    val (msg1, rest) = args
    val fields = mutableListOf(msg1.toString())
    for (arg in rest) {
        fields.add(arg.toString())
    }
    warn(fields.joinToString(" "))
    return theNonPrintingObject
}

/// builtin load
/// fun     bi_load
/// std     filename
/// key     "verbose" to T, "error" to T
/// opt     
/// rest    
/// ret     t/nil
/// special no
/// doc {
/// Load specified file; return t if the contents was evaluated without error.
/// If keyword ERROR is nil (the default is true), do not raise an error for
/// an unfound file. If keyword verbose is nil (the default is true), do not
/// print an informational message after loading.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_load(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val fname = arg1(args).toString()
    val verbose = kwArgs[verboseSym] !== Nil
    val throw_error = ob2bool(kwArgs[errorSym] ?: T)
    
    return load_file(fname, throw_error, !verbose)
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
fun bi_make_string_input_stream(args: LObject,
                                kwArgs: Map<LSymbol, LObject>): LObject {
    val string = stringArg(arg1(args), "make-string-input-stream")
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
fun bi_stream(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val arg = arg1(args)
    if (arg is LStream) {
        return arg
    }
    return StringReaderStream(arg.toString())
}

/// builtin format
/// fun     bi_format
/// std     format destination
/// key     
/// opt     
/// rest    args
/// ret     nil-or-string
/// special no
/// doc {
/// Format `args` according to `format-string` and write to `dest` (stream,
/// t, or nil). Nil means return the result as a string, t means write to
/// standard output.
/// The format string is interpreted by the Kotlin library, meaning it is
/// mostly like the format string in the related C functions.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_format(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val (dest, rest) = args
    val (f, f_args) = rest
    val format = stringArg(f, "format string")
    val formatted: String

    formatted = format.format(*valueArray(f_args))
    when (dest) {
        T -> stdout.write(formatted)
        Nil -> return makeString(formatted)
        is LStream ->
            dest.write(formatted)
        else ->
            throw ArgumentError("format `dest` not nil or t or stream: $dest")
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
fun bi_open(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
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
                            + direction.desc())
    }
    if (!inp && !outp) {
        throw ArgumentError("open :direction not :input or :output or :io : "
                            + direction.desc())
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
fun bi_close(args: LObject, kwArgs: Map<LSymbol, LObject>): LObject {
    val stream = streamArg(arg1(args), "close")
    try {
        return bool2ob(stream.close())
    } catch (e: Exception) {
        throw IOError(e)
    }
}
