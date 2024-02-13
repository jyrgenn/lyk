
package org.w21.lyk

val Nil = intern("nil", true)
val T = intern("t", true)

val rootEnv = Environment()
var currentEnv = rootEnv
