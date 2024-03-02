// the REPL -- to be

package org.w21.lyk


fun repl(prompt: String = "\n> ") {
    val reader = Reader(stdinStream, "*repl*")
    println("read on ${reader.desc()}")

    while (true) {
        print(prompt)
        try {
            val obj = reader.read()
            if (obj == null) {
                println()
                break
            }
            val result = eval(obj)
            println(result.desc())
        } catch (e: LispError) {
            reader.skipRestOfLine()
            if (Options.print_estack) {
                e.printStackTrace()
            } else {
                println(e.toString())
                debug(debugErrorSym, e.asObject().desc())
            }
        } catch (e: Exception) {
            
        }
    }
}
