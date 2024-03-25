// global variables controlling system behaviour

package org.w21.lyk

val rootEnv = LEnv()
var currentEnv = rootEnv

val Nil = intern("nil", true)
val T = intern("t", true)

val warningsAsErrors = LSymbol.makeGlobal("*warnings-as-errors*")
val last_error = LSymbol.makeGlobal("*last-error*")

var inErrset = false            // are we in an (errset ...) context?

var gensymCounter = 4711

// This is meant for interactive functions like #'doc, so their
// printed output is not cluttered by the display of a (irrelevant)
// return value. The REPL will print any return value but this one.
// Idea taken from TI's PC Scheme.
//
val theNonPrintingObject = intern("*the-non-printing-object*", true)

var stdin = FileReaderStream(stdinPath, stdinName, error = true)
var stdout = FileWriterStream(stdoutPath, stdoutName, flushln = true)
var stderr = FileWriterStream(stderrPath, stderrName, flushch = true,
                              error = true)
var console = FileWriterStream(consolePath, consoleName, flushch = true)

var debug_out = console

val verboseSym = intern("verbose")
val errorSym = intern("error")

var consCounter = 0
var evalCounter = 0

// function parameters
val optionalPSym = intern("&optional")
val keyPSym = intern("&key")
val restPSym = intern("&rest")
val emptyString = makeString("")
val returnSym = intern("=>")
val no_returnSym = intern("no-return")


// debug and trace topic names
val debugEvalSym = intern("eval")
val debugEvalPrognSym = intern("evalprogn")
val debugEvalFunSym = intern("evalfun")
val debugCallSym = intern("call")
val debugErrorSym = intern("error")
val debugIOSym = intern("io")
val debugBindParSym = intern("bindpar")
val debugReaderSym = intern("reader")
val debugConsSym = intern("cons")
val debugCatchThrowSym = intern("catch-throw")
val debugLetBindSym = intern("let")
val debugBindSymSym = intern("bind")
val debugStepEval = intern("stepeval")
val debugMacroSym = intern("macro")
val debugLambdaParamsSym = intern("params")
val debugPreloadSym = intern("preload")
val debugDebugSym = intern("debug")
val debugFormatSym = intern("format")
val debugDefunSym = intern("defun")

val debugOffSym = intern("=off")
val debugListSym = intern("=list")


val unquoteSymbol = intern("unquote")
val unquoteSplicingSymbol = intern("unquote-splicing")
val defmacroSym = intern("defmacro")

var debugOn = false             // set iff any debug topic is "on"

val commandLineArgs = LSymbol.makeGlobal("*command-line-args*")
val lastError = LSymbol.makeGlobal("*last-error*")
val currentLoadFile = LSymbol.makeGlobal("*current-load-file*")

// set of features provided
val featureSet = mutableSetOf<LSymbol>()

// eval stuff
var evalLevel: Int = 0
var maxEvalLevel: Int = 0
var maxRecursionDepth: Int = 1_000_000_000
var abortEval: Boolean = false
var stepEval: Boolean = false
var evalStack = ListCollector()

