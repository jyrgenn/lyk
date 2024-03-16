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

var consCounter = 0
var evalCounter = 0

// debug and trace topic names
val debugEvalSym = Symbol.intern("eval")
val debugEvalPrognSym = Symbol.intern("evalprogn")
val debugEvalFunSym = Symbol.intern("evalfun")
val debugCallSym = Symbol.intern("call")
val debugErrorSym = Symbol.intern("error")
val debugIOSym = Symbol.intern("io")
val debugBindParSym = Symbol.intern("bindpar")
val debugReaderSym = Symbol.intern("reader")
val debugConsSym = Symbol.intern("cons")
val debugCatchThrowSym = Symbol.intern("catch-throw")
val debugLetBindSym = Symbol.intern("let")
val debugBindSymSym = Symbol.intern("bind")
val debugStepEval = Symbol.intern("stepeval")
val debugMacroSym = Symbol.intern("macro")
val debugLambdaParamsSym = Symbol.intern("params")

val debugOffSym = Symbol.intern(":off")
val debugListSym = Symbol.intern(":list")
val debugDebugSym = Symbol.intern("debug")


val unquoteSymbol = Symbol.intern("unquote")
val unquoteSplicingSymbol = Symbol.intern("unquote-splicing")


var debugOn = false             // set iff any debug topic is "on"

val commandLineArgs = Symbol.makeGlobal("*command-line-args*")

val currentLoadFile = Symbol.makeGlobal("*current-load-file*")

// set of features provided
val featureSet = mutableSetOf<Symbol>()
