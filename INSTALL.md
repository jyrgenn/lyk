To build and install lyk, I currently use OpenJDK 21.0.3 (no idea
which other versions would work), Kotlin JVM compiler version 1.9.22
or 2.0.0, and etags from a current GNU Emacs installation (29.3).
The OSes that work for me are macOS 14.5 (Sonoma) and Ubuntu 22.04.
The lyk start script needs a Korn Shell; ksh93 works.

Build and install lyk like this:

    $ ./make new install

You may want to edit `src/Makefile` to set INSTALLBASE, which
defaults to `/opt/w21`. The executable programs `lyk` (the shell
script starting the interpreter) and `lyc` (the lyk calculator) are
installed in `$(INSTALLBASE)/bin/`; `lyk.jar`, some documentation
and Lisp sources in `$(INSTALLBASE)/lib/lyk/`. Make sure you have
permission to create and/or write into these directories.

Have fun!

[2024-07-11 ni@w21.org]
