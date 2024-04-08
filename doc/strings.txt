;;; lyk doc

String Literals
---------------

String literals are relatively C- or Go-like. They start with a
double quote and end with one. Strings may contain (literal)
newlines. There are a few special character escapes:

   \a : bell
   \b : backspace
   \f : form feed
   \n : newline
   \r : carriage return
   \t : horizontal tab
   \v : vertical tab

Characters in a string can be specified as Unicode code points:

   \ddd       with each d an octal digit (up to 3
   \xXX       with each X a hexadecimal digit (exactly 2)
   \uXXXX     with each X a hexadecimal digit (exactly 4)
   \UXXXXXXXX with each X a hexadecimal digit (exactly 8)

All others are taken literally when preceded by a backslash
(including double quote and backslash).
