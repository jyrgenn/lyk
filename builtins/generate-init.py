#!/usr/bin/env python

# create the Builtin register calls for the described builtins

import sys
import jpylib as y

print("""\
// initialise the builtin functions; should be generated one day

package org.w21.lyk

val noStd = arrayOf<String>()
val noKey = mapOf<String, LispObject>()
val noOpt = arrayOf<Pair<String, LispObject>>()
val noRest = null
val noRet = "nil"


fun init_Builtins() {
    println("Initialise Builtins")
""")

builtin = None                          # keyword => value, also, in one?
lineno = 0
in_doc = False
docstring = None


def prin(*args, **kwargs):
    print(*args, end="", **kwargs)


def print_init(data):
    errors = False
    for key in data:
        if data.get(key) == None:
            err(f"builtin {data.name}: missing {key}")
            errors = True
    if errors:
        sys.exit(1)

    prin(f'    Builtin("{data.name}", ::{data.fun},\n            ')
    if data.std:
        content = "\", \"".join(data.std)
        prin(f'arrayOf<String>("{content}")')
    else:
        prin("noStd")
    prin(",\n            ")

    if data.key:
        prin("mapOf<String, LispObject>(")
        prin(" ".join(data.key))
        prin(")")
    else:
        prin("noKey")
    prin(",\n            ")

    if data.opt:
        prin("arrayOf<Pair<String, LispObject>>(")
        prin(" ".join(data.opt))
        prin(")")
    else:
        prin("noOpt")
    prin(",\n            ")

    prin(f'"{data.rest or "noRest"}"')
    prin(",\n            ")
    prin(f'"{data.ret or "noRet"}"')
    prin(",\n            ")
    
    prin(repr(y.boolish(data.special[0])).lower())
    prin(",\n            ")
    if data.doc:
        prin('"""' + repr(data.doc)[1:-2] + '"""')
    print(")")


def errx(message):
    sys.exit(f"line {lineno}: " + message)

def err(message):
    print("line {lineno}: " + message, file=sys.stderr)

try:
    for line in y.all_input_lines(sys.argv[1:]):
        lineno += 1
        line = line.rstrip()
        # print("I read:", line, file=sys.stderr)

        if in_doc:
            if line.lstrip() == "}":
                builtin["doc"] = "\n".join(docstring)
                in_doc = False
            else:
                docstring.append(line)
            continue

        if not line or line.startswith("#"):
            continue

        parts = line.split()
        cmd = parts[0]

        if builtin:
            if cmd in ("std", "key", "opt", "rest"):
                print(f"builtin[{cmd}] = {parts[1:]}", file=sys.stderr)
                builtin[cmd] = parts[1:]
            elif cmd in ("fun", "ret", "special"):
                if len(parts) != 2:
                    print(line)
                    errx(f"just one parameter needed for '{cmd}'")
                print(f"builtin[{cmd}] = {parts[1]}", file=sys.stderr)
                builtin[cmd] = parts[1]
            elif cmd == "doc":
                if len(parts) != 2 or parts[1] != "{":
                    errx("missing opening brace after 'doc'")
                in_doc = True
                docstring = []
            elif cmd == "end":
                if len(parts) != 2 or parts[1] != "builtin":
                    errx("missing 'builtin' after 'end'")
                print_init(builtin)
                builtin = None
            else:
                errx(f"unexpected command '{cmd}'")
            continue

        if cmd == "builtin":
            if len(parts) != 2:
                sys.exit(f"line {lineno}: builtin without name")
            builtin = y.Namespace(name=parts[1], fun=None, std=None, key=None,
                                  opt=None, rest=None, ret=None, doc=None,
                                  special=False)
        else:
            errx(f"unexpected command '{cmd}', expected 'builtin <name>'")

    print("""\
    }
    // EOF
    """)
except FileNotFoundError as e:
    sys.exit(e)
