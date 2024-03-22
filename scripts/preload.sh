#!/bin/sh

cat <<EOF
// generated preload string -- DO NOT EDIT!

package org.w21.lyk

val preload_code = """
EOF

for file; do
    echo ";; from $file"
    echo "(debug preload \"$file\")"
    echo
    cat $file
    echo ";; end $file"
    echo
done

cat <<EOF
"""
// EOF
EOF
