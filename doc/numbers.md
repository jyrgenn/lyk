;;; lyk doc

Numbers
-------

Numbers in Lyk are very simple -- they are Kotlin Double values,
nothing else. If they have an integer value, they can be used as
integers, but under the hood they are still Doubles. There are no
complex or rational numbers.

One consequence of this is that numbers-pretending-to-be-integers
start to get imprecise when they get so large that the mantissa of
the floating point representation isn't enough to keep all digits.
This is a known bug, and maybe enough of a motivation to implement
some decent integer representation some time.


Number Literals
---------------

The syntax of number literals is a subset of that of Common Lisp.

Integer literals can be written in decimal as a sequence of decimal
digits [0-9], or in binary as a sequence of binary digits [01]
preceded by ``#b'', or in octal as a sequence of octal digits [0-7]
preceded by ``#o'', or in hexadecimal as a seqence of fexadecimal
digits [0-9a-fA-F] preceded by ``#x''.

Decimal floating-point numbers can be written in Kotlin syntax:

    float_lit = decimals "." [ decimals ] [ exponent ] |
                decimals exponent |
                "." decimals [ exponent ] .
    decimals  = decimal_digit { decimal_digit } .
    exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals .

Decimal integers and floating point numbers can be preceded by
``-'', resulting in the appropriate negative number. Also,
optionally by ``+''.

Examples:

         1  the number one
  31557600  the number of seconds in a Julian astronomical year
 3.15576e7  the number of seconds in a Julian astronomical year
-3.1415926  as seconds, minus a ten-millionth of a year (roughly)
 #b1001001  the number seventy-three
 #o1001001  the number two hundred sixty-two thousand six hundred
            fifty-seven
 #x1001001  the number sixteen million seven hundred eighty-one
            thousand three-hundred thirteen
 #17r21     the number thirty-five
