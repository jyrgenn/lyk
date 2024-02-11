// the REPL -- to be

package org.w21.lyk


fun repl(prompt: String = "> ") {
    val reader = Reader(StdinStream())

    while (true) {
        print(prompt)
        val obj = reader.read()
        if (obj == null) {
            break
        }
        println("${typeOf(obj)} ${obj.description()}")
    }
    println()
}
