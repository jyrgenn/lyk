#!/bin/sh

cd src &&
    echo "make[1]: Entering directory \`$PWD'" &&
    make "$@"
