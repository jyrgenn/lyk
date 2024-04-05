// exit orderly, with everything cared for

package org.w21.lyk

import kotlin.system.exitProcess

val cleanupRoutines = mutableListOf<() -> Unit>()


fun atexit(closure: () -> Unit) {
    cleanupRoutines.add(closure)
}

fun exitLyk(status: Int = 0) {
    for (closure in cleanupRoutines) {
        closure()
    }
    exitProcess(status)
}