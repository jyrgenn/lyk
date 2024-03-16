// global variables controlling system behaviour

package org.w21.lyk

val rootEnv = Environment()
var currentEnv = rootEnv

val Nil = LSymbol.intern("nil", true)
val T = LSymbol.intern("t", true)

val warningsAsErrors = LSymbol.makeGlobal("*warnings-as-errors*")
val last_error = LSymbol.makeGlobal("*last-error*")

var inErrset = false            // are we in an (errset ...) context?

var gensymCounter = 4711

// This is meant for interactive functions like #'doc, so their
// printed output is not cluttered by the display of a (irrelevant)
// return value. The REPL will print any return value but this one.
// Idea taken from TI's PC Scheme.
//
val theNonPrintingObject = LSymbol.intern("*the-non-printing-object*", true)

var stdin = FileReaderStream(stdinPath, stdinName, error = true)
var stdout = FileWriterStream(stdoutPath, stdoutName, flushln = true)
var stderr = FileWriterStream(stderrPath, stderrName, flushch = true,
                              error = true)
var console = FileWriterStream(consolePath, consoleName, flushch = true)

var debug_out = console

val verboseSym = LSymbol.intern("verbose")
val errorSym = LSymbol.intern("error")

var consCounter = 0
var evalCounter = 0

// debug and trace topic names
val debugEvalSym = LSymbol.intern("eval")
val debugEvalPrognSym = LSymbol.intern("evalprogn")
val debugEvalFunSym = LSymbol.intern("evalfun")
val debugCallSym = LSymbol.intern("call")
val debugErrorSym = LSymbol.intern("error")
val debugIOSym = LSymbol.intern("io")
val debugBindParSym = LSymbol.intern("bindpar")
val debugReaderSym = LSymbol.intern("reader")
val debugConsSym = LSymbol.intern("cons")
val debugCatchThrowSym = LSymbol.intern("catch-throw")
val debugLetBindSym = LSymbol.intern("let")
val debugBindSymSym = LSymbol.intern("bind")
val debugStepEval = LSymbol.intern("stepeval")
val debugMacroSym = LSymbol.intern("macro")
val debugLambdaParamsSym = LSymbol.intern("params")

val debugOffSym = LSymbol.intern(":off")
val debugListSym = LSymbol.intern(":list")
val debugDebugSym = LSymbol.intern("debug")


val unquoteSymbol = LSymbol.intern("unquote")
val unquoteSplicingSymbol = LSymbol.intern("unquote-splicing")


var debugOn = false             // set iff any debug topic is "on"

val commandLineArgs = LSymbol.makeGlobal("*command-line-args*")

val currentLoadFile = LSymbol.makeGlobal("*current-load-file*")

// set of features provided
val featureSet = mutableSetOf<LSymbol>()
