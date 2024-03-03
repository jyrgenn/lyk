
package org.w21.lyk

val rootEnv = Environment()
var currentEnv = rootEnv

val Nil = Symbol.intern("nil", true)
val T = Symbol.intern("t", true)

// This is meant for interactive functions like #'doc, so their
// printed output is not cluttered by the display of a (irrelevant)
// return value. The REPL will print any return value but this one.
// Idea taken from TI's PC Scheme.
//
val theNonPrintingObject = Symbol.intern("*the-non-printing-object*", true)

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
        printErr(";; $message")
    }
}

fun error(message: String) {
    if (errors) {
        printErr("ERROR $message")
    }
}
