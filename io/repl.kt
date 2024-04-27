// the REPL -- to be

package org.w21.lyk


fun repl(reader: Reader, prompt: String? = null, print: Boolean = false
): LispError? {
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
        if (interactive) {
            reader.input.setPrompt(promptString)
        }
        try {
            // Read, 
            var (expr, loc) = reader.read()
            if (expr == null) {
                break
            }
            lastTopLevelLocation = loc
            
            // Expand macros (just not macro definitions (or do I?)),
            // expr = macroExpandForm(expr)
            if (expr is LCons && expr.car !== defmacroSym) {
                expr = macroExpandForm(expr)
            }
            debug(debugReplSym) {
                expr
            }

            // Eval,
            val (perfdata, value) = measurePerfdataValue {
                eval(expr)
            }
            if (print) {
                println(value)
            }

            // Print,
            if (value !== theNonPrintingObject) {
                iprintln(value.desc(null))
            }
            iprintln()
            if (interactive) {
                lastValueSym.setValue(value, true)
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
            printEvalStack(e)
            if (e.evalStack.size > 10) {
                printErr(e)
            }
            debug(debugErrorSym) {
                e.toObject().desc(null)
            }
            if (!interactive) {
                return e
            }
        } catch (e: Exception) {
            printErr("unexpected exception:", e)
            e.printStackTrace()
            return OtherError("unexpected exception in REPL", e)
        }
    }                           // end Loop
    if (interactive) {
        console.println()
    }
    return null
}
