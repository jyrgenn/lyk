// global variables controlling system behaviour

package org.w21.lyk

val rootEnv = LEnv()
var currentEnv = rootEnv

val Nil = intern("nil", true)
val T = intern("t", true)
val LambdaSym = intern("lambda", true)
val greekLambdaSym = intern("λ", true)

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

val stdin = FileReaderStream(stdinPath, name = stdinName)
val stdout = FileWriterStream(stdoutPath, name = stdoutName, flushln = true)
val stderr = FileWriterStream(stderrPath, name = stderrName, flushch = true,
                              error = true)

var console = FileIOStream(consolePath, name = consoleName, flushch = true)

var debug_out = console

val verboseSym = intern("verbose")
val errorSym = intern("error")

var consCounter = 0L
var evalCounter = 0L

// location of the last top level expression read in a repl
var lastTopLevelLocation = ""

// property to store a defined-in location for functions, macros
var definedInPropSym = intern("defined-in")

// property to store a defined-in location for defvar, defparameter
var varDefinedInPropSym = intern("var-defined-in")

// function parameters
val optionalPSym = intern("&optional")
val keyPSym = intern("&key")
val restPSym = intern("&rest")
val emptyString = makeString("")
val returnSym = intern("=>")
val no_returnSym = intern("no-return")

val lastValueSym = intern("_")


// debug and trace topic names
val debugEvalSym = intern("eval")
val debugEvalPrognSym = intern("evalprogn")
val debugEvalFunSym = intern("evalfun")
val debugCallSym = intern("call")
val debugErrorSym = intern("error")
val debugIOSym = intern("io")
val debugBindParSym = intern("bindpar")
val debugReaderSym = intern("reader")
val debugReadSymSym = intern("readsym")
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
val debugAtexitSym = intern("atexit")
val debugFinalizeSym = intern("finalize")
val debugLoadlineSym = intern("loadline")
val debugReadCharSym = intern("readchar")
val debugStartupSym = intern("startup")
val debugReplSym = intern("repl")

val debugOffSym = intern("=off")
val debugListSym = intern("=list")

val defunSym = debugDefunSym
val quoteSymbol = intern("quote")
val unquoteSymbol = intern("unquote")
val unquoteSplicingSymbol = intern("unquote-splicing")
val defmacroSym = intern("defmacro")

var debugOn = false             // set iff any debug topic is "on"

val commandLineArgs = LSymbol.makeGlobal("*command-line-args*")
val lastError = LSymbol.makeGlobal("*last-error*")
val terminalWidth = LSymbol.makeGlobal("*terminal-width*")
val terminalHeight = LSymbol.makeGlobal("*terminal-height*")
val loadPrintSym = LSymbol.makeGlobal("*load-print*")
val loadPathSym = LSymbol.makeGlobal("*load-path*")
val loadPathnameSym = LSymbol.makeGlobal("*load-pathname*")

// indicates the name of the load file that should be shown in error messages
val loadFileNameInd = ";#file"

val evalStackAbbrLines = LSymbol.makeGlobal("*eval-stack-abbr-lines*", T)

// set of features provided
val featureSet = mutableSetOf<LSymbol>()

// eval stuff
var evalLevel: Int = 0
var maxEvalLevel: Int = 0
var maxRecursionDepth: Int = 1_000_000_000
var abortEval: Boolean = false
var stepEval: Boolean = false

fun getTermWidth(): Int {
    val w = terminalWidth.getValueOptional()
    if (w is LNumber) {
        return w.toInt()
    }
    return 80
}

fun init_Variables() {
    val columns = System.getenv("COLUMNS")
    val width = Reader(StringReaderStream(columns ?: "")).read().first ?: Nil
    terminalWidth.setValue(width)
    val lines = System.getenv("LINES")
    val height = Reader(StringReaderStream(lines ?: "")).read().first ?: Nil
    terminalHeight.setValue(height)

    val lykpath = System.getenv("LYKPATH")?.split(":") ?: listOf(".")
    val loadpath = collectedList {
        for (dir in lykpath) {
            it.add(makeString(dir.trimEnd('/') + "/l"))
        }
    }
    loadPathSym.setValue(loadpath)

    LSymbol.makeGlobal(stdinName, stdin)
    LSymbol.makeGlobal(stdoutName, stdout)
    LSymbol.makeGlobal(stderrName, stderr)
    LSymbol.makeGlobal(consoleName, console)
}
