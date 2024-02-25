
package org.w21.lyk

val rootEnv = Environment()
var currentEnv = rootEnv

val Nil = Symbol.intern("nil", true)
val T = Symbol.intern("t", true)

var inErrset = false            // are we in an (errset ...) context?
val last_error = makeGlobal("*last-error*")

var gensymCounter = 4711

var stdinStream = StdinStream()
var stdoutStream = StdoutStream()
var stderrStream = StderrStream()

var warnings = true             // print warnings
var errors = true               // print errors


fun warn(message: String) {
    if (warnings) {
        System.err.println(";; $message")
    }
}

fun error(message: String) {
    if (errors) {
        System.err.println("ERROR $message")
    }
}
