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
 ~:C  : like ~C for printing characters, spelled out for others
        (e.g. #\Rubout)
 ~@C  : spelled out for all, for the Lisp reader to read
 ~:@C : like ~:C, "and then if the character requires unusual shift
        keys on the keyboard to type it, this fact is mentioned."
        Fsck that. Will be (more or less) gracefully ignored.

[x] Tilde %: print a #\Newline; with ~n%, print n newlines.

[x] Tilde &: fresh line; print a newline if we aren't already at the start of a
          line.

[x] Tilde |: Print a page separator; with ~n|, print n page separators

[x] Tilde ~: print a tilde; with ~n~, print n tildes.


Radix Control
-------------

all implemented because they are important and/or easy to implement 

[·] Tilde R: Radix  
    ~radix,mincol,padchar,commachar,comma-interval[flags]R  
         where : means print commachar ever comma-interval digits,  
           and @ means *always* print the sign, also for  
                       non-negative numbers  
    ~R   : cardinal English number: four (very restricted range)  
    ~:R  : ordinal English number: fourth (very restricted range)  
    ~@R  : Roman numeral: IV (1..3999 for historical reasons)  
    ~:@R : old Roman numeral: IIII (1..3999 for historical reasons)  

    Roman numbers were much easier to implement than expected.

[x] Tilde D: Decimal

[x] Tilde B: Binary

[x] Tilde O: Octal

[x] Tilde X: Hexadecimal


Floating Point
--------------

want to implement F (done), E (started, on hold), and G; not so sure
about $

[x] Tilde F: Fixed-Format Floating-Point

[•] Tilde E: Exponential Floating-Point

[ ] Tilde G: General Floating-Point

[ ] Tilde $: Monetary Floating-Point


Printer Operations
------------------

[x] Tilde A: Aesthetic, the most basic and at the same time the most
    important format directive, as it can be used for everything

[x] Tilde S: Standard, also pretty basic while not quite as commonly
    in use as tilde A

[-] Tilde W: Write -- rejected because it assumes too much of the CL
    printer concept that lyk has not


Pretty Printer Operations
-------------------------

all rejected because they are too complex to implement; I *may*
change my mind about tilde /, though, as it sounds relatively easy
and could be fun to have; would need some special-casing in the
format string parser

[-] Tilde _: Conditional Newline

[-] Tilde <: Logical Block

[-] Tilde I: Indent

[-] Tilde /: Call Function
    A formatting function would look like this:

      (defun ff (stream arg colon-flag atsign-flag &rest params)
        (format nil ">>~S<<" arg))

    http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Body/sec_22-3-5-4.html


Layout Control
--------------

all rejected because they are too complex to implement

[-] Tilde T: Tabulate

[-] Tilde <: Justification

[-] Tilde >: End of Justification


Control-Flow Operations
-----------------------

all rejected because they are too complex to implement; also, the
idea of having a separate programming language[1] seems overblown
(but hey, that is CL, right?)

[1] according to some blog post 
    http://arcanesentiment.blogspot.com/2009/02/value-of-extensible-format-strings.html

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

tilde newline makes sense and is relatively easy; the others are
tied to the control-flow things I don't implement anyway

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

