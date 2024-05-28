;;; lyk doc

Character Literals
------------------

The syntax of character literals follows that of Common Lisp:
#\<char> (where <char> is a single character) is a literal for
<char>. #\<character-name> is a literal for the character described
by <character-name>, which is one of

    Space
    Newline
    Tab
    Page
    Rubout
    Linefeed
    Return
    Backspace
    Bell

The <character-name> is case insensitive.

In addition, numeric character literals can be specified as

    #\xXX        (XX: two hexadecimal digits [0-9a-fA-F])
    #\uXXXX      (XXXX: four hexadecimal digits)
    #\UXXXXXXXX  (XXXXXXXX: eight hexadecimal digits)
    #\bB...      (B...: any number of binary digits [01])
    #\O...       (O...: any number of at least two octal digits [0-7])

with the hexadecimal, binary, or octal digits, respectively,
representing the number of a Unicode code point.

Examples:

    #\)            (a closing parenthesis)
    #\B            (the letter B)
    #\Page         (the form feed control code)
    #\x42          (the letter B)
    #\u0042        (the letter B)
    #\U000000042   (the letter B)
    #\b1000010     (the letter B)
    #\102          (the letter B)
    
