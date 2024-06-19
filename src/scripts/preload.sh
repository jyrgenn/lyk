#!/bin/sh

cat <<EOF
// generated preload string -- DO NOT EDIT!

package org.w21.lyk

val preload_code = """
EOF

for file; do
    echo ";#file $file"
    printf %s "(debug preload \"$file\") "
    cat $file
    echo
done

cat <<EOF
"""
// EOF
EOF
