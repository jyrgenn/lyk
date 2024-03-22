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
            iprintln()
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
                eval(expr)
            }

            // Print,
            if (value !== theNonPrintingObject) {
                iprintln(value.desc())
            }
            if (interactive) {
                info(perfdata)
            }
        } catch (e: LispError) {
            reader.skipRestOfLine()
            if (Options.print_estack) {
                e.printStackTrace()
            } else {
                printErr(e)
                for (frame in evalStack) {
                    stderr.println(frame)
                }
                evalStack = Nil
                debug(debugErrorSym) {
                    e.toObject().desc()
                }
                if (!interactive) {
                    return e
                }
            }
        } catch (e: Exception) {
            printErr("unexpected exception:", e)
            e.printStackTrace()
            return OtherError("unexpected exception in REPL", e)
        }
    }                           // and Loop
    return null
}
