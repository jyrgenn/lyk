#!/bin/sh
# generate the source file containing the build tag

program=${1??usage: $(basename $0) TARGET}
version=$(git describe --dirty)
builtat=$(date "+%F %R")
builtby=$USER@$HOST
kotlin=$(kotlinc -version 2>&1 |
             perl -nE '/^info: (\S+) (\S+)/ && say "$1 $2"')

cat <<EOB
// Build tag -- generated file, do not edit

package org.w21.lyk

val build_info = mapOf(
    "program" to "$program",
    "version" to "$version",
    "kotlin" to "$kotlin",
    "built-at" to "$builtat",
    "built-by" to "$builtby",
)

fun buildtag(): String {
    return "$program $version $kotlin $builtat $builtby"
}
EOB

