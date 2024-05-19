About `format`
==============

BASIC OUTPUT
------------

directive ::= tilde [prefix_parameters] [flags] type

prefix_parameters ::= [parameter] [, [parameter]]

parameter ::= /([+-][0-9]+|v|#|'.)/

type ::= "C" | "%"| "&" | "|" | "~"


[x] Tilde C: Character
 ~C   : the character itself
 ~:C  : like ~C for printing characters, spelled out for others (e.g. #\Rubout)
 ~@C  : spelled out for all, for the Lisp reader to read
 ~:@C : like ~:C, "and then if the character requires unusual shift keys on the
        keyboard to type it, this fact is mentioned." Fsck that.
        Will be (more or less) gracefully ignored.

[x] Tilde %: print a #\Newline; with ~n%, print n newlines.

[x] Tilde &: fresh line; print a newline if we aren't already at the start of a
          line.

[x] Tilde |: Print a page separator; with ~n|, print n page separators

[x] Tilde ~: print a tilde; with ~n~, print n tildes.


Radix Control
-------------

[ ] Tilde R: Radix

[ ] Tilde D: Decimal

[ ] Tilde B: Binary

[ ] Tilde O: Octal

[ ] Tilde X: Hexadecimal


Floating Point
--------------

[ ] Tilde F: Fixed-Format Floating-Point

[ ] Tilde E: Exponential Floating-Point

[ ] Tilde G: General Floating-Point

[ ] Tilde $: Monetary Floating-Point


Printer Operations
------------------

[x] Tilde A: Aesthetic

[x] Tilde S: Standard

[-] Tilde W: Write


Pretty Printer Operations
-------------------------

[-] Tilde _: Conditional Newline

[-] Tilde <: Logical Block

[-] Tilde I: Indent

[-] Tilde /: Call Function


Layout Control
--------------

[-] Tilde T: Tabulate

[-] Tilde <: Justification

[-] Tilde >: End of Justification


Control-Flow Operations
-----------------------

[-] Tilde *: Go-To

[-] Tilde [: Conditional Expression

[-] Tilde ]: End of Conditional Expression

[-] Tilde {: Iteration

[-] Tilde }: End of Iteration

[-] Tilde ?: Recursive Processing


Miscellaneous Operations
------------------------

[-] Tilde (: Case Conversion

[-] Tilde ): End of Case Conversion

[-] Tilde P: Plural


Miscellaneous Pseudo-Operations
-------------------------------

[-] Tilde ;: Clause Separator

[-] Tilde ^: Escape Upward

[x] Tilde #\Newline: Ignored Newline


Implementation ideas

  * keep a { directive => format_directive } or even a { format_string =>
    format_control } mapping to avoid re-parsing format directives/strings over
    and over.

  * have a format controller class hierarchy, including literals for the parts
    between format directives; every object is constructed while parsing the
    format string, yielding a chain of format controllers that each chomp up
    0..n format args depending on their type

