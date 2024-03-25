// the REPL -- to be

package org.w21.lyk


fun repl(reader: Reader, prompt: String? = null): LispError? {
    // If we have a prompt, we assume this repl is interactive and print eval
    // results to stdout.

    val interactive = prompt != null
    val promptString = prompt ?: ""     // just so we don't have to check all
                                        // the time; we rely on interactive
                                        // anyway
    fun iprint(s: String, flush: Boolean = false) {
        if (interactive) {
            stdout.write(s)
            if (flush) {
                stdout.flush()
            }
        }
    }

    fun iprintln(s: String? = null) {
        // print only when interactive
        if (s != null) {
            iprint(s)
        }
        iprint("\n", flush = true)
    }

    while (true) {
        iprint(promptString, true)
        try {
            // Read, 
            var expr = reader.read()
            if (expr == null) {
                break
            }

            // Expand macros (just not macro definitions),
            if (expr is LCons && expr.car === defmacroSym) {
                // println("skip expanding (${expr.car} ${expr.cdr.car} ...)")
            } else {
                // if (expr is LCons) {
                //     println("(${expr.car} ${expr.cdr.car} ...)")
                // }
                expr = macroExpandForm(expr)
            }

            // Eval,
            val (perfdata, value) = measurePerfdataValue {
                eval(macroExpandForm(expr))
            }

            // Print,
            if (value !== theNonPrintingObject) {
                iprintln(value.desc())
            }
            iprintln()
            if (interactive) {
                info(perfdata)
            }
        } catch (e: JavaError) {
            e.err.printStackTrace()
            throw e
        } catch (e: LispError) {
            reader.skipRestOfLine()
            if (Options.print_estack) {
                e.printStackTrace()
            }
            printErr(e)
            for (frame in evalStack) {
                // stderr.println(frame)
                val (level, expr, env) = frame as LVector
                val frameno = "#%d".format((level as LNumber).toInt())
                val pad = mulString(" ", frameno.length)
                // using format() here lead to spurious %s argument missing
                // errors, so I work around it
                stderr.println(frameno, env.desc())
                stderr.println(pad, expr)
            }
            if (evalStack.size > 10) {
                printErr(e)
            }
            debug(debugErrorSym) {
                e.toObject().desc()
            }
            evalStack = ListCollector()
            if (!interactive) {
                return e
            }
        } catch (e: Exception) {
            printErr("unexpected exception:", e)
            e.printStackTrace()
            return OtherError("unexpected exception in REPL", e)
        }
    }                           // and Loop
    return null
}
