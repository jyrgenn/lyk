// the REPL -- to be

package org.w21.lyk


fun repl(prompt: String = "> ") {
    val reader = Reader(StdinStream(name = "*repl*"))
    println("read on ${reader.description()}")

    while (true) {
        print(prompt)
        try {
            val obj = reader.read()
            if (obj == null) {
                println()
                break
            }
            println("${typeOf(obj)} ${obj.description()}")
        } catch (e: Exception) {
            reader.skipRestOfLine()
            println(e)
        }
    }
}
