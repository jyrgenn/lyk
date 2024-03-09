// global variables controlling system behaviour

package org.w21.lyk

val rootEnv = Environment()

val warningsAsErrors = Symbol.makeGlobal("*warnings-as-errors*")
val last_error = Symbol.makeGlobal("*last-error*")

var inErrset = false            // are we in an (errset ...) context?

var gensymCounter = 4711

// This is meant for interactive functions like #'doc, so their
// printed output is not cluttered by the display of a (irrelevant)
// return value. The REPL will print any return value but this one.
// Idea taken from TI's PC Scheme.
//
val theNonPrintingObject = Symbol.intern("*the-non-printing-object*", true)

var stdin = FileReaderStream(stdinPath, stdinName, error = true)
var stdout = FileWriterStream(stdoutPath, stdoutName, flushln = true)
var stderr = FileWriterStream(stderrPath, stderrName, flushch = true,
                              error = true)



val verboseSym = Symbol.intern("verbose")
val errorSym = Symbol.intern("error")

var pairCounter = 0
var evalCounter = 0

val currentLoadFile = Symbol.makeGlobal("*current-load-file*")

