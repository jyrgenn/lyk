// the REPL -- to be

package org.w21.lyk

val starSym = LSymbol.makeGlobal("*", Nil)
val star2Sym = LSymbol.makeGlobal("**", Nil)
val star3Sym = LSymbol.makeGlobal("***", Nil)
val plusSym = LSymbol.makeGlobal("+", Nil)
val plus2Sym = LSymbol.makeGlobal("++", Nil)
val plus3Sym = LSymbol.makeGlobal("+++", Nil)
val slashSym = LSymbol.makeGlobal("/", Nil)
val slash2Sym = LSymbol.makeGlobal("//", Nil)
val slash3Sym = LSymbol.makeGlobal("///", Nil)


fun repl(reader: Reader, interactive: Boolean = false, print: Boolean = false
): LispError? {
    // If we have a prompt, we assume this repl is interactive and print eval
    // results to stdout.

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
        try {
            // Read, 
            var (expr, loc) = reader.read()
            if (expr == null) {
                break
            }
            lastTopLevelLocation = loc
            
            debug(debugReplSym) {
                expr
            }

            // may be a short command
            if (expr.isKeyword()) {
                val table = replShortCommands.getValueOptional()
                if (table is LTable) {
                    val cmd = table.get(expr)
                    if (cmd !== Nil) {
                        if (cmd is LFunction) {
                            cmd.call(Nil)
                        }
                        println()
                        continue
                    }
                } else {
                    warn("$replShortCommands is not a table:"
                         + " ${replShortCommands.desc(null)}")
                }
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
                plus3Sym.setValue(plus2Sym.getValue())
                plus2Sym.setValue(plusSym.getValue())
                plusSym.setValue(expr, true)
                star3Sym.setValue(star2Sym.getValue())
                star2Sym.setValue(starSym.getValue())
                starSym.setValue(value, true)
                slash3Sym.setValue(slash2Sym.getValue())
                slash2Sym.setValue(slashSym.getValue())
                slashSym.setValue(list(value), true)
                info(perfdata.desc())
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
        } catch (sig: LykSignal) {
            printErr(sig)
        } catch (e: Exception) {
            printErr("unexpected exception:", e)
            e.printStackTrace()
            return OtherError("unexpected exception in REPL", e)
        }
    }                           // end Loop
    if (interactive) {
        stderr.println()
    }
    return null
}
