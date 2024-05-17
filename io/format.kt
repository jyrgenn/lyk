// The format function, to be. For now I will only write up the requirements as
// I understand them.

package org.w21.lyk

/***********************************

 BASIC OUTPUT (and I think I'll keep it at that for now)
 
 directive ::= tilde [prefix_parameters] [flags] type

 prefix_parameters ::= [parameter] [, [parameter]]

 parameter ::= /([+-][0-9]+|v|#|'.)/

 type ::= "C" | "%"| "&" | "|" | "~"


 Tilde C: Character
 ~C   : the character itself
 ~:C  : like ~C for printing characters, spelled out for others (#\Rubout)
 ~@C  : spelled out for all, for the Lisp reader to read
 ~:@C : like ~:C, "and then if the character requires unusual shift keys on the
        keyboard to type it, this fact is mentioned." Fsck that.

 Tilde %: print a #\Newline; with ~n%, print n newlines.

 Tilde &: fresh line; print a newline if we aren't already at the start of a
          line.

 Tilde |: Print a page separator; with ~n|, print n page separators

 Tilde ~: print a tilde; with ~n~, print n tildes.


 Implementation ideas

  * keep a { directive => format_directive } or even a { format_string =>
    format_control } mapping to avoid re-parsing format directives/strings over
    and over.

  * have a format controller class hierarchy, including literals for the parts
    between format directives; every object is constructed while parsing the
    format string, yielding a chain of format controllers that each chomp up
    0..n format args depending on their type

 ***********************************/

