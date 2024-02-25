// the REPL -- to be

package org.w21.lyk


fun repl(prompt: String = "> ") {
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
            // println("${typeOf(obj)} ${obj.desc()} => $result")
            println(result.desc())
        } catch (e: Exception) {
            reader.skipRestOfLine()
            throw e
            // println(e)
        }
    }
}
