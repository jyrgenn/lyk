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
            val expr = reader.read()
            iprintln()
            if (expr == null) {
                break
            }

            // Expand macros,
            val expanded = macroExpandForm(expr)

            // Eval,
            val (perfdata, value) = measurePerfdataValue {
                eval(expanded)
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
                debug(debugErrorSym) {
                    e.toObject().desc()
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
