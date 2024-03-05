#!/usr/bin/env python

# create the Builtin register calls for the described builtins

import re
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

""")

builtin = None                          # keyword => value, also, in one?
in_doc = False
docstring = None
filename = None
lineno = 0


def prin(*args, **kwargs):
    print(*args, sep="", end="", **kwargs)


def print_init(builtin):
    errors = False
    for key in builtin:
        if builtin.get(key) == None:
            err(f"builtin {builtin.name}: missing {key}")
            errors = True
    if errors:
        sys.exit(1)

    prin(f'    Builtin("{builtin.name}", ::{builtin.fun},\n            ')
    if builtin.std:
        prin("/* std */ ")
        content = "\", \"".join(builtin.std)
        prin(f'arrayOf<String>("{content}")')
    else:
        prin("noStd")
    prin(",\n            ")

    if builtin.key:
        prin("/* key */ ")
        prin("mapOf<String, LispObject>(")
        prin(" ".join(builtin.key))
        prin(")")
    else:
        prin("noKey")
    prin(",\n            ")

    if builtin.opt:
        prin("/* opt */ ")
        prin("arrayOf<Pair<String, LispObject>>(")
        for vardef in builtin.opt.split(","):
            parts = vardef.split()
            var = parts[0]
            if len(parts) == 1:
                value = "Nil"
            elif len(parts) == 2:
                value = parts[1]
            else:
                errx(f"botched opt in {builtin}")
            prin(f"Pair(\"{var}\", {value}), ")
        prin(")")
    else:
        prin("noOpt")
    prin(",\n            ")

    if builtin.rest:
        prin("/* rest */ ")
        prin('"', builtin.rest[0], '"')
    else:
       prin("noRest")
    prin(",\n            ")
    if builtin.ret:
        prin("/* ret */ ")
        prin('"', builtin.ret, '"')
    else:
        prin("noRet")
    prin(",\n            ")
    prin("/* special */ ")
    prin(repr(y.boolish(builtin.special[0])).lower())
    prin(",\n            ")
    prin('"""\n' + builtin.doc + '"""')
    print(")")
    print()


def errx(message):
    sys.exit(f"builtins/{filename}:{lineno}: " + message)

def err(message):
    print("builtins/{filename}:{lineno}: " + message, file=sys.stderr)

    
def all_lines(files):
    global filename
    global lineno
    for file in files:
        with open(file) as f:
            filename = file
            lineno = 0
            for line in f:
                lineno += 1
                if line.startswith("///"):
                    yield re.sub("^/// ?", "", line)
                

try:
    for line in all_lines(sys.argv[1:]):
        line = line.rstrip()
        # print("I read:", line, file=sys.stderr)

        if in_doc:
            if line.lstrip() == "}":
                builtin["doc"] = "\n".join(docstring)
                in_doc = False
            else:
                if builtin.name == "defun":
                    print(f">>{line}<<", file=sys.stderr)
                docstring.append(line)
            continue

        if not line or line.startswith("#"):
            continue

        parts = line.split()
        cmd = parts[0]

        if builtin:
            if cmd in ("std", "key", "rest"):
                print(f"builtin[{cmd}] = {parts[1:]}", file=sys.stderr)
                builtin[cmd] = parts[1:]
            elif cmd in ("opt"):
                arg = " ".join(parts[1:])
                print(f"builtin[{cmd}] = {arg}", file=sys.stderr)
                builtin[cmd] = arg
            elif cmd in ("fun", "ret", "special"):
                if len(parts) != 2:
                    print(line)
                    errx(f"exactly one parameter needed for '{cmd}'")
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
                sys.exit(f"builtins/{filename}:{lineno}: builtin without name")
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
