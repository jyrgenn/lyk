// global variables controlling system behaviour

package org.w21.lyk

val rootEnv = Environment()
var currentEnv = rootEnv

val Nil = Symbol.intern("nil", true)
val T = Symbol.intern("t", true)

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
var console = FileWriterStream(consolePath, consoleName, flushch = true)

var debug_out = console

val verboseSym = Symbol.intern("verbose")
val errorSym = Symbol.intern("error")

var pairCounter = 0
var evalCounter = 0

// debug and trace topic names
val traceEvalSym = Symbol.intern("eval")
val traceEvalFunSym = Symbol.intern("evalfun")
val traceCallSym = Symbol.intern("call")
val debugErrorSym = Symbol.intern("error")
val debugIOSym = Symbol.intern("io")
val debugBindParSym = Symbol.intern("bindpar")
val debugReaderSym = Symbol.intern("reader")


val currentLoadFile = Symbol.makeGlobal("*current-load-file*")

// set of features provided
val featureSet = mutableSetOf<Symbol>()
