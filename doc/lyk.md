;;; lyk doc

Lyk — a Lisp yn Kotlin
======================

Like my previous attempts, mostly, this Lisp interpreter implements
a somewhat traditional form of Lisp, closer to Maclisp and Common
Lisp than to Scheme in spirit. Mainly it is a Lisp-2 -- somehow I
seem to feel close to separate function and value properties of a
symbol than to just one (as in Scheme, which is a Lisp-1), and the
aesthetics of older Lisps has always appealed to me.

While my attempt is far from the vast comprehensiveness and
sophistication of Common Lisp, I try to keep the names of and
functionality of builtin functions somewhat similar, although often
simpler. As the reference for Common Lisp I have used the CLHS, and
for its behaviour mostly SBCL. In other words, if a function has the
same name as in Common Lisp, it should at least do something
similar.

Common Lisp things missing in Lyk:
  - lots of builtin functions
  - packages
  - multiple values
  - complete number system
  - the more complex format directives
  - compilation
  - classes
  - structures (maybe later)
  - arrays beyond vector
  and many many more (hey, even this list in incomplete!)

In its inner workings, Lyk is a rather simple Lisp, firmly in the My
Favourite Toy Language category. It is a fun project after all, and
hammering out the details of, for instance, a sophisticated numerics
subsystem does not sound like much fun to me. Not without users
crying for better number support anyway. Same goes for much of the
above.


Documentation
-------------

You are reading it. In the `doc/` directory of the source repository
and in the installation directory are a few more short documents
describing parts of lyk. It isn't really comprehensive, though.


Explore
-------

There are a few tools to help explore lyk:

  - `apropos` prints all known symbols that match a substring or a
    regular expression, together with the information if the symbol
    is bound to a function (builtin, lambda, or macro), has a
    variable binding, or properties. Call it like this:

        (apropos "substring")

    or

        (apropos #/regexp/)

  - `doc` prints a function documentation for a symbol (or a
    function object) with a synopsis, description of the function,
    and the place where it is defined. Example:

        (doc 'apropos)

  - `describe` return a alist with an object's (the sole argument)
    attributes. This isn't as far developed as I'd wish, but for a
    symbol it shows all of its attributes, including the properties
    in an alist.

  - `DOCSTRINGS.md` is built in the `generated/` subdirectory of the
    source repository and installed in the `doc/` subdirectory of
    the installation directory. It contains the docstrings of all
    functions, same as those printed interactively with

        (doc 'function-name)


Installation
------------

lyk is built and installed using the `Makefile` at the root of the
source respository, which is used when you call `make install` (must
be GNU Make). The places where lyk is installed are controlled by
these Makefile variables:

    INSTALLBASE = /opt/w21
    INSTALLDIR =  $(INSTALLBASE)/lib/lyk
    INSTALLBIN =  $(INSTALLBASE)/bin

These are the contents of the INSTALLDIR directory after
installation:

  - `README.md` : the README file from the root of the repository
  - `doc/`      : the documentation, including this text
  - `l`         : some Lisp code that comes with lyl
  - `lyk.jar`   : contains the compiled lyk classes and the Kotlin and
                  JLine runtime libraries

These files are installed in the INSTALLBIN:

  - `lyk`       : the command to run lyk (a wrapper shell script)
  - `lyc`       : the lyk calculator, need `lyk` in $PATH


Have fun!
