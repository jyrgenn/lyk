#!/bin/sh
# generate the source file containing the build tag

program=${1?usage: $(basename $0) TARGET}
commit=$(git describe --dirty)
builtat=$(date "+%Y%m%d:%H%M%S")
builtby=$USER@$HOST
kotlin=$(kotlinc -version 2>&1 |
             perl -nE '/^info: (\S+) (\S+)/ && say "$1 $2"')

cat <<EOB
// Build tag -- generated file, do not edit

package org.w21.lyk

val build_info = mapOf(
    "program" to "$program",
    "commit" to "$commit",
    "kotlin" to "$kotlin",
    "built-at" to "$builtat",
    "built-by" to "$builtby",
)

fun buildtag(): String {
    return "$program $commit $kotlin $builtat $builtby"
}
EOB

