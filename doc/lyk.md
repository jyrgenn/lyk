;;; lyk doc

Lyk — a Lisp yn Kotlin
======================

Like my previous attempts, mostly, this Lisp interpreter implements
a somewhat traditional form of Lisp, closer to Maclisp and Common
Lisp than to Scheme. Somehow I seem to feel close to separate
function and value properties of a symbol than to just one, and the
aesthetics of older Lisps has always appealed to me.

While my attempt is far from the vast comprehensiveness and
sophistication of Common Lisp, I try to keep the names of and
functionality of builtin functions somewhat similar, although often
simpler. As the reference for Common Lisp behaviour I have used
SBCL. In other words, if a function has the same name as in Common
Lisp, it should bahave at least similar.

Things in Common Lisp missing in Lyk:
  - lots of builtin functions
  - packages
  - multiple values
  - complete number system
  - a real format
  - compilation
  - classes
  - structures (maybe later)
  - arrays beyond vector
  and many many more (even this list in incomplete)

So, overall Lyk is a rather simple Lisp, firmly in the My Favourite
Toy Language category. It is a fun project after all, and hammering
out the details of a sophisticated numerics subsystem does not sound
like much fun to me. Not without users crying for better number
support anyway. Same goes for much of the above.
