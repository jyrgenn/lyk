;;; lyk doc

Regular Expressions
-------------------

The regular expression syntax supported is that of the Kotlin regexp
package, which is mostly the same as in e.g. Perl or Python. See
https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html
for details.

Regular expressions can be constructed using the function #'regexp,
which takes a single string argument and returns a regexp object. It
is an error if the string does not conform to the abovementioned
regexp syntax.

Regular expression literals are either #/.../ or #rX...Y, where the
opening character X can be any character except whitespace or a
semicolon. If X is one of the opening brackets [, {, (, <, the
closing character Y must be the corresponding closing bracket >, ),
}, ]. Otherwise, Y must be the same as X. The "\<letter>" character
classes like "\d" do not (yet) work with regexp literals, because
they confuse the reader.

Metacharacters in the regular expression, including the closing
character, can be escaped using a backslash. Character literal
escaping with \a, \b, \f, \n, \t, \v, \[digits], \x..., \X...,
\u..., \U... takes precedence, though, so beware.

Examples:

   #/[a-fA-F0-9]+/
   #/.*\/$/
   #r{[a-fA-F0-9/]+}
   #r(gnagna\)foo)

Regexp objects are callables that can be used like functions. Their
return value is a match list, which may be nil. The elements of the
match list are the full match, followed by the group matches.

Example:

   (#/[bs]([aeiou]+)/ "blubberfasel")
   => ("be" "e")

Multiple matches, case sensitivity, multi-line mode, and greediness
can be controlled through a `limit' parameter and inline regexp
flags. See (doc #//) for details. Example:

   (#/(?i)[bs]([aeiou]+)/ "blubBerfasel" t)
   => (("Be" "e") ("se" "e"))
