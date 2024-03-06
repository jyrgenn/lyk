// I/O functions

package org.w21.lyk

val directionKeyw = Symbol.intern(":direction")
val inputKeyw = Symbol.intern(":input")
val outputKeyw = Symbol.intern(":output")
val ioKeyw = Symbol.intern(":io")
val if_existsKeyw = Symbol.intern(":if-exists")
val new_versionKeyw = Symbol.intern(":new-version")
val appendKeyw = Symbol.intern(":append")
val overwriteKeyw = Symbol.intern(":overwrite")
val supersedeKeyw = Symbol.intern(":supersede")
val errorKeyw = Symbol.intern(":error")
val if_does_not_existKeyw = Symbol.intern(":if-does-not-exist")
val createKeyw = Symbol.intern(":create")


fun bi_print(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    // TODO: &optional port
    print("\n${arg1(args).desc()} ")
    return arg
}

fun bi_print_to_string(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    return LispString.makeString("\n${arg1(args).desc()} ")
}

fun bi_prin1(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    // TODO: &optional port
    print(arg1(args).desc())
    return arg
}

fun bi_prin1_to_string(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    return makeString(arg1(args).desc())
}

fun bi_princ(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    // TODO: &optional port
    print(arg1(args).desc())
    return arg
}

fun bi_princs(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    return makeString(arg1(args).desc())
}

fun bi_terpri(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    // TODO: &optional port
    println()
    return Nil
}

fun bi_warning(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    // TODO: warnings as errors
    val (msg1, rest) = args
    var message = msg.desc()
    for (arg in rest) {
        message += " "
        message += arg.desc()
    }
    warn(message)
    return Nil
}

fun bi_load(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val fname = arg1(args).valueString
    val verbose = key_args[Symbol.intern("verbose")] !== Nil
    val throw_error = key_args[Symbol.intern("error")] ?? T
    val pairsBefore = pairCounter
    val evalsBefore = evalCounter
    val savedLoadFileValue = try currentLoadFile.getValue()
    defer { do { try currentLoadFile.bindValue(savedLoadFileValue) } catch {} }
    do {
        val load_file = try Stream.readFile(path: fname, name: fname)
        try currentLoadFile.bindValue(makeString(fname))
        defer { do { try load_file.close(); } catch {} }
        if val error = try repl(Reader(input: load_file, source: fname)) {
            if verbose {
                stderr.println("\(error)")
            }
            if throw_error !== Nil {
                throw error
            }
            return Nil
        }
        if verbose {
            val pairs = pairCounter - pairsBefore
            val evals = evalCounter - evalsBefore
            stderr.println("; load \(fname): \(pairs) pairs, \(evals) evals")
        }
    } catch {
            if verbose {
                stderr.println("\(error)")
            }
            if throw_error !== Nil {
                throw error
            }
            return Nil
    }
    return T
}

fun bi_make_string_input_stream(args: LispObject, kwArgs: Map<Symbol, LispObject>
): LispObject {
    val string = try stringArg(arg1(args), "make-string-input-stream")
    return try bi_stream(args: Pair(makeString(string), Nil),
                              key_args: [:])
}

fun bi_stream(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    val arg = arg1(args)
    if val stream = arg as? Stream {
        return stream
    }
    return try StringStream(String(arg.valueString))
}

fun bi_format(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    var args = args
    val dest = pop(&args)
    val format = try stringArg(pop(&args), "format string")
    val varargs = list2varargs(args)
    val result = String(format: format, arguments: varargs)
    switch dest {
    case T:
        stdout.print(result)
    case Nil:
        return makeString(result)
    case val s as Stream:
        s.print(result)
    default:
        throw ArgumentError("format `dest` is not nil or t or stream: \(dest)")
    }
    return Nil
}

fun bi_open(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    // TODO complete
    print("open \(args) \(key_args)")
    val fname = arg1(args).valueString
    val direction = key_args[key2var(directionKeyw)] ?? Nil
    val if_exists = key_args[key2var(if_existsKeyw)] ?? Nil
    val if_does_not_exist = key_args[key2var(if_does_not_existKeyw)] ?? Nil

    var inp = false
    var outp = false
    var append = false
    var return_nil = false

    switch direction {
    case inputKeyw:  inp = true
    case outputKeyw: outp = true
    case ioKeyw:     inp = true; outp = true
    default:
        throw ArgumentError("open :direction not :input or :output or :io : "
                              + direction.description)
    }
    switch if_exists {
    case new_versionKeyw, overwriteKeyw, supersedeKeyw:
        break
    case appendKeyw:
        append = true
    case errorKeyw:
        break
    default:
        break
    }
    switch if_does_not_exist {
    case errorKeyw:
        break
    case Nil:
        return_nil = true
    default:
        break
    }
    do {
        val s = try Stream(inp: inp, outp: outp, path: fname, append: append)
        // print(">>\(s)<<")
        return s
    } catch {
        if return_nil {
            return Nil
        }
        throw error
    }
}

fun bi_close(args: LispObject, kwArgs: Map<Symbol, LispObject>): LispObject {
    do {
        try (try streamArg(arg1(args), "close")).close()
        return T
    } catch {
        return Nil
    }
}
