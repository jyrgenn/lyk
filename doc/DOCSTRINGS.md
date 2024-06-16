;;; Docstrings for lyk v0.2-126-gb3833e0-dirty kotlinc-jvm 2.0.0 ni@abild

`%`  
builtin function `(% num1 num2) => modulo`  
Return modulo of `num1` divided by `num2` (which must be integers).  
[builtin function defined in builtins/numbers.kt:395:1]

`*`  
builtin function `(* &rest numbers) => product`  
Return the product of all NUMBERS.  
[builtin function defined in builtins/numbers.kt:71:1]

`**`  
builtin function `(** base power) => number`  
Return `base` raised to the power `power`.  
[builtin function defined in builtins/numbers.kt:417:1]

`*lc-closure*`  
builtin function `(*lc-closure* &rest args) => the-list`  
This is a list collector closure. It takes an arbitrary number of arguments,  
which are then added to the end of the list, and returns the list. The normal  
use case would be to call it a number of times to add items to the list, and  
then call it once without arguments to return the resulting list. This is more  
efficient than using append repeatedly.  
  
Example:  
    (let ((lc (list-collector)))  
      (lc 'see)  
      (lc 0 'evil)  
      (lc 'hear "no" 'evil)  
      (lc))  
    => (see 0 evil hear "no" evil)  
[builtin function defined in l/alldocs.lisp:1]

`+`  
builtin function `(+ &rest numbers) => sum`  
Return the sum of the NUMBERS.  
[builtin function defined in builtins/numbers.kt:19:1]

`-`  
builtin function `(- num1 &rest numbers) => number`  
Return NUM1 minus all NUMBERS, or the negation of sole arg NUM1.  
[builtin function defined in builtins/numbers.kt:43:1]

`/`  
builtin function `(/ num1 &rest numbers) => number`  
Return NUM1 divided by all NUMBERS, or the inverse of sole arg NUM1.  
[builtin function defined in builtins/numbers.kt:94:1]

`/=`  
builtin function `(/= &rest items) => t/nil`  
Return t if all `items` are different, otherwise nil.  
Comparable are symbols, numbers, and strings.  
[builtin function defined in builtins/numbers.kt:286:1]

`1+`  
builtin function `(1+ number) => number`  
Return a value of `number` plus 1.  
[builtin function defined in builtins/numbers.kt:437:1]

`1-`  
builtin function `(1- number) => number`  
Return a value of `number` plus 1.  
[builtin function defined in builtins/numbers.kt:456:1]

`<`  
builtin function `(< &rest items) => t/nil`  
Return t if all `items` are in strictly ascending order, otherwise nil.  
Comparable are symbols, numbers, and strings.  
[builtin function defined in builtins/numbers.kt:197:1]

`<=`  
builtin function `(<= &rest items) => t/nil`  
Return t if all `items` are in ascending order, otherwise nil.  
Comparable are symbols, numbers, and strings.  
[builtin function defined in builtins/numbers.kt:345:1]

`<=>`  
builtin function `(<=> item1 item2) => number`  
Return -1, 0, or 1 if `item1` is less than, equal to, or greater than `item2`.  
Comparable are symbols, numbers, and strings.  
[builtin function defined in builtins/numbers.kt:375:1]

`=`  
builtin function `(= &rest items) => t/nil`  
[builtin function defined in builtins/numbers.kt:256:1]

`=/=`  
macro `(=/= ob1 ob2)`  
Return true iff OB1 and OB2 are unequal (in terms of #'equal).  
[macro defined in preload/20-macros.lisp:23]

`>`  
builtin function `(> &rest items) => t/nil`  
Return t if all `items` are in strictly descending order, otherwise nil.  
Comparable are symbols, numbers, and strings.  
[builtin function defined in builtins/numbers.kt:227:1]

`>=`  
builtin function `(>= &rest items) => t/nil`  
Return t if all `items` are in descending order, otherwise nil.  
Comparable are symbols, numbers, and strings.  
[builtin function defined in builtins/numbers.kt:315:1]

`abs`  
builtin function `(abs number) => absolute-value`  
Return the absolute value of `number`.  
[builtin function defined in builtins/numbers.kt:141:1]

`acos`  
builtin function `(acos number) => radians`  
Return the arc cosine of `number`.  
[builtin function defined in builtins/numbers.kt:159:1]

`acosh`  
builtin function `(acosh number) => radians`  
Return the arc hyperbolic cosine of `number`.  
[builtin function defined in builtins/numbers.kt:177:1]

`all-symbols`  
lambda function `(all-symbols &optional comparison-function) => value`  
Return a list of all symbols, alphabetically sorted.  
If optional `comparison-function` is supplied , use it to sort the list.  
if `comparison-function` is t, sort the list alphabetically.  
[lambda function defined in preload/other.lisp:27]

`and`  
special form `(and &rest args) => value`  
Evaluate `args` until one is nil; return the last evaluated value.  
[special form defined in builtins/basic.kt:386:1]

`append`  
builtin function `(append &rest lists) => value`  
Return a new list that is the concatenation of `lists`.  
The list structure of all but the last list is copied.  
[builtin function defined in builtins/basic.kt:1370:1]

`apply`  
builtin function `(apply function &rest args+) => value`  
Apply `function` to `args+` and return the result value.  
`args+` is a spreadable argument list, meaning the last argument (meant  
to be a list) is appended to the previous ones.  
[builtin function defined in builtins/basic.kt:928:1]

`apropos`  
builtin function `(apropos pattern &optional as-list) => none/list`  
Print all interned symbols whose name contains `pattern`.  
`pattern` may be a string or a regular expression.  
If optional `as-list` is true, return a list of the symbol names.  
[builtin function defined in builtins/system.kt:253:1]

`aref`  
builtin function `(aref vector index) => element`  
Return the element of `vector` at `index`.  
[builtin function defined in builtins/vectors.kt:17:1]

`arg-number-is-one`  
lambda function `(arg-number-is-one arg) => value`  
Iff ARG (or the number of its elements) is 1, return true.  
[lambda function defined in preload/other.lisp:3]

`ash`  
builtin function `(ash integer count) => number`  
Return `integer` arithmetically shifted by `count` bit positions.  
Ash shifts left if `count` is positive, right if `count` is negative.  
The shifted value has the same sign as `integer`.  
[builtin function defined in builtins/numbers.kt:496:1]

`asin`  
builtin function `(asin number) => radians`  
Return the arc sine of `number`.  
[builtin function defined in builtins/numbers.kt:521:1]

`asinh`  
builtin function `(asinh number) => arc-hsine`  
Return the arc hyperbolic sine of `number`.  
[builtin function defined in builtins/numbers.kt:540:1]

`assert`  
special form `(assert test-form &optional message) => nil`  
Evaluate `test-form`, and if the result is nil, raise an error.  
The error message includes `test-form`, and, if present, `message`  
(which is evaluated in that case).  
[special form defined in builtins/system.kt:228:1]

`assoc`  
builtin function `(assoc item alist) => cons/nil`  
Look up `item` in `alist` and return the pair whose car is equal to `item`.  
Return nil if `item` is not found.  
[builtin function defined in builtins/alists.kt:39:1]

`assoc-if`  
builtin function `(assoc-if predicate alist) => cons/nil`  
Return the first cons in `alist` for whose car `predicate` is true.  
If there is no such cons, return nil.  
[builtin function defined in builtins/alists.kt:147:1]

`assoc-if-not`  
builtin function `(assoc-if-not predicate alist) => cons/nil`  
Return the first cons in `alist` for whose car `predicate` is false.  
If there is no such cons, return nil.  
[builtin function defined in builtins/alists.kt:171:1]

`assq`  
builtin function `(assq item alist) => cons/nil`  
Look up `item` in `alist` and return the pair whose car is eq to `item`.  
Return nil if `item` is not found as a car in one of the pairs in `alist`.  
[builtin function defined in builtins/alists.kt:93:1]

`atan`  
builtin function `(atan number) => radians`  
Return the arc tangent of `number`.  
[builtin function defined in builtins/numbers.kt:559:1]

`atanh`  
builtin function `(atanh number) => arc-htangent`  
Return the arc hyperbolic tangent of `number`.  
[builtin function defined in builtins/numbers.kt:578:1]

`atom`  
builtin function `(atom object) => t/nil`  
Return t if `arg` is atomic (i.e. symbol, number, string, char), nil else.  
[builtin function defined in builtins/basic.kt:1637:1]

`barams`  
builtin function `(barams a b c &optional d (e 4) &key (:k1 3) :k2 &rest grabbelsack) => t`  
Exercise all kinds of function parameters and return t.  
[builtin function defined in builtins/system.kt:496:1]

`basename`  
builtin function `(basename pathspec) => file-basename`  
Return the basename of a pathname, meaning without the directory part(s).  
[builtin function defined in builtins/files.kt:21:1]

`boundp`  
builtin function `(boundp symbol) => t/nil`  
Return t if a value is bound to `symbol`, nil otherwise.  
[builtin function defined in builtins/basic.kt:792:1]

`build-info`  
builtin function `(build-info &optional as-string) => alist`  
Return an alist with data decribing the current program build.  
If optional argument `as-string` is true, return the info as a string.  
[builtin function defined in builtins/system.kt:328:1]

`builtinp`  
builtin function `(builtinp object) => t/nil`  
Return t if `object` is a builtin function, else nil.  
[builtin function defined in builtins/basic.kt:1171:1]

`caaaar`  
builtin function `(caaaar list) => value`  
Return the caaaar of `list`.  
[builtin function defined in builtins/cxr.kt:233:1]

`caaadr`  
builtin function `(caaadr list) => value`  
Return the caaadr of `list`.  
[builtin function defined in builtins/cxr.kt:251:1]

`caaar`  
builtin function `(caaar list) => value`  
Return the caaar of `list`.  
[builtin function defined in builtins/cxr.kt:88:1]

`caadar`  
builtin function `(caadar list) => value`  
Return the caadar of `list`.  
[builtin function defined in builtins/cxr.kt:269:1]

`caaddr`  
builtin function `(caaddr list) => value`  
Return the caaddr of `list`.  
[builtin function defined in builtins/cxr.kt:287:1]

`caadr`  
builtin function `(caadr list) => value`  
Return the caadr of `list`.  
[builtin function defined in builtins/cxr.kt:106:1]

`caar`  
builtin function `(caar list) => value`  
Return the caar of `list`.  
[builtin function defined in builtins/cxr.kt:16:1]

`cadaar`  
builtin function `(cadaar list) => value`  
Return the cadaar of `list`.  
[builtin function defined in builtins/cxr.kt:305:1]

`cadadr`  
builtin function `(cadadr list) => value`  
Return the cadadr of `list`.  
[builtin function defined in builtins/cxr.kt:323:1]

`cadar`  
builtin function `(cadar list) => value`  
Return the cadar of `list`.  
[builtin function defined in builtins/cxr.kt:124:1]

`caddar`  
builtin function `(caddar list) => value`  
Return the caddar of `list`.  
[builtin function defined in builtins/cxr.kt:341:1]

`cadddr`  
builtin function `(cadddr list) => value`  
Return the cadddr of `list`.  
[builtin function defined in builtins/cxr.kt:359:1]

`caddr`  
builtin function `(caddr list) => value`  
Return the caddr of `list`.  
[builtin function defined in builtins/cxr.kt:142:1]

`cadr`  
builtin function `(cadr list) => value`  
Return the cadr of `list`.  
[builtin function defined in builtins/cxr.kt:34:1]

`car`  
builtin function `(car list) => object`  
Return the contents of the address part of the `list` register.  
The car of nil is nil.  
[builtin function defined in builtins/basic.kt:19:1]

`catch`  
special form `(catch tag &rest bodyforms) => value`  
Eval `bodyforms` as implicit progn. If a throw occurs to the `tag`,  
which is evaluated, return the value that is thrown. Otherwise, return  
the value of the last bodyform.  
[special form defined in builtins/basic.kt:727:1]

`cdaaar`  
builtin function `(cdaaar list) => value`  
Return the cdaaar of `list`.  
[builtin function defined in builtins/cxr.kt:377:1]

`cdaadr`  
builtin function `(cdaadr list) => value`  
Return the cdaadr of `list`.  
[builtin function defined in builtins/cxr.kt:395:1]

`cdaar`  
builtin function `(cdaar list) => value`  
Return the cdaar of `list`.  
[builtin function defined in builtins/cxr.kt:160:1]

`cdadar`  
builtin function `(cdadar list) => value`  
Return the cdadar of `list`.  
[builtin function defined in builtins/cxr.kt:413:1]

`cdaddr`  
builtin function `(cdaddr list) => value`  
Return the cdaddr of `list`.  
[builtin function defined in builtins/cxr.kt:431:1]

`cdadr`  
builtin function `(cdadr list) => value`  
Return the cdadr of `list`.  
[builtin function defined in builtins/cxr.kt:178:1]

`cdar`  
builtin function `(cdar list) => value`  
Return the cdar of `list`.  
[builtin function defined in builtins/cxr.kt:52:1]

`cddaar`  
builtin function `(cddaar list) => value`  
Return the cddaar of `list`.  
[builtin function defined in builtins/cxr.kt:449:1]

`cddadr`  
builtin function `(cddadr list) => value`  
Return the cddadr of `list`.  
[builtin function defined in builtins/cxr.kt:467:1]

`cddar`  
builtin function `(cddar list) => value`  
Return the cddar of `list`.  
[builtin function defined in builtins/cxr.kt:196:1]

`cdddar`  
builtin function `(cdddar list) => value`  
Return the cdddar of `list`.  
[builtin function defined in builtins/cxr.kt:485:1]

`cddddr`  
builtin function `(cddddr list) => value`  
Return the cddddr of `list`.  
[builtin function defined in builtins/cxr.kt:503:1]

`cdddr`  
builtin function `(cdddr list) => value`  
Return the cdddr of `list`.  
[builtin function defined in builtins/cxr.kt:214:1]

`cddr`  
builtin function `(cddr list) => value`  
Return the cddr of `list`.  
[builtin function defined in builtins/cxr.kt:70:1]

`cdr`  
builtin function `(cdr list) => object`  
Return the contents of the decrement part of the `list` register.  
The cdr of nil is nil.  
[builtin function defined in builtins/basic.kt:38:1]

`ceiling`  
builtin function `(ceiling number) => number`  
Return `number` truncated to an integer towards positive infinity.  
[builtin function defined in builtins/numbers.kt:670:1]

`char-code`  
builtin function `(char-int char) => code`  
Return a non-negative integer encoding the `char` object.  
`char` may also be a string of length 1.  
[builtin function defined in builtins/chars.kt:41:1]

`char-equal`  
builtin function `(char-equal &rest characters) => t/nil`  
Return true iff all characters are the same except for case.  
[builtin function defined in builtins/chars.kt:141:1]

`char-greaterp`  
builtin function `(char-greaterp &rest characters) => t/nil`  
Return true iff the characters are monotonically decreasing.  
[builtin function defined in builtins/chars.kt:313:1]

`char-int`  
builtin function `(char-int char) => code`  
Return a non-negative integer encoding the `char` object.  
`char` may also be a string of length 1.  
[builtin function defined in builtins/chars.kt:41:1]

`char-lessp`  
builtin function `(char-lessp &rest characters) => t/nil`  
Return true iff the characters are monotonically increasing.  
[builtin function defined in builtins/chars.kt:215:1]

`char-not-equal`  
builtin function `(char-not-equal &rest characters) => t/nil`  
Return true iff all characters are unequal, without regarding case.  
[builtin function defined in builtins/chars.kt:159:1]

`char-not-greaterp`  
builtin function `(char-not-greaterp &rest characters) => t/nil`  
Return true if the characters are monotonically non-decreasing.  
[builtin function defined in builtins/chars.kt:255:1]

`char-not-lessp`  
builtin function `(char-not-lessp &rest characters) => t/nil`  
Return true iff the characters are monotonically not increasing.  
[builtin function defined in builtins/chars.kt:353:1]

`char/=`  
builtin function `(char/= &rest characters) => t/nil`  
Return true iff all given characters are unequal to each other.  
[builtin function defined in builtins/chars.kt:177:1]

`char<`  
builtin function `(char< &rest characters) => t/nil`  
Return true iff the characters are monotonically increasing.  
[builtin function defined in builtins/chars.kt:195:1]

`char<=`  
builtin function `(char<= &rest characters) => t/nil`  
Return true if the characters are monotonically non-decreasing.  
[builtin function defined in builtins/chars.kt:235:1]

`char=`  
builtin function `(char= &rest characters) => t/nil`  
Return true if the characters are all equal.  
[builtin function defined in builtins/chars.kt:275:1]

`char>`  
builtin function `(char> &rest characters) => t/nil`  
Return true iff the characters are monotonically decreasing.  
[builtin function defined in builtins/chars.kt:293:1]

`char>=`  
builtin function `(char>= &rest characters) => t/nil`  
Return true iff the characters are monotonically not increasing.  
[builtin function defined in builtins/chars.kt:333:1]

`charp`  
builtin function `(charp object) => t/nil`  
Return t iff `object` is a character, nil else.  
[builtin function defined in builtins/basic.kt:2020:1]

`cis`  
builtin function `(cis &rest &rest) => nothing`  
Function is not implemented; will throw an error.  
[builtin function defined in builtins/numbers.kt:850:1]

`close`  
builtin function `(close stream) => t/nil`  
Close an open stream. Return t if the stream was open, nil else  
[builtin function defined in builtins/io.kt:379:1]

`code-char`  
builtin function `(code-char code) => char`  
Return a character with the code attribute given by `code`. If no such  
character exists and one cannot be created, return nil.  
[builtin function defined in builtins/chars.kt:18:1]

`collect-performance-data`  
special form `(collect-performance-data &rest bodyforms) => alist`  
Evaluate `bodyforms` and return a list of performance data.  
This list is an alist with the number of conses created, the number  
of evals performed, and the number of seconds taken to perform the  
evaluation, e.g.  
    ((conses . 6055097) (evals . 4755187) (secs . 0.265043))  
[special form defined in builtins/system.kt:751:1]

`concatenate`  
lambda function `(concatenate result-type &rest seqs) => value`  
[lambda function defined in preload/sequences.lisp:2]

`cond`  
special form `(cond &rest clauses) => value`  
For each clause of the form (condition expressions...) evaluate condition.  
For the first one that is non-nil, return the last value of evaluating the  
expressions. If none of the conditions is non-nil, return nil.  
[special form defined in builtins/basic.kt:440:1]

`conjugate`  
builtin function `(conjugate &rest &rest) => nothing`  
Function is not implemented; will throw an error.  
[builtin function defined in builtins/numbers.kt:832:1]

`cons`  
builtin function `(cons car cdr) => cons`  
Return a new cons consisting of `car` and `cdr`.  
[builtin function defined in builtins/basic.kt:153:1]

`console-reader-stream-p`  
builtin function `(console-reader-stream-p object) => t/nil`  
Return t iff `object` is a console reader stream, nil else.  
[builtin function defined in builtins/io.kt:483:1]

`consp`  
builtin function `(consp object) => t/nil`  
Return t is `object` is a cons cell, else nil.  
[builtin function defined in builtins/basic.kt:1020:1]

`copy-seq`  
builtin function `(copy-seq sequence) => copied-sequence`  
Return a copy of `sequence`.  
The elements of the new sequence are the same as the corresponding  
elements of the given sequence.  
[builtin function defined in builtins/sequences.kt:87:1]

`cos`  
builtin function `(cos radians) => result`  
Return the cosine of `radians`  
[builtin function defined in builtins/numbers.kt:1270:1]

`cosh`  
builtin function `(cosh radians) => result`  
Return the hyperbolic cosine of `radians`  
[builtin function defined in builtins/numbers.kt:1216:1]

`debug`  
special form `(debug topic &rest data) => t/nil`  
Print a debug message, if `topic` (a symbol, not evaluated) is enabled.  
If the topic is enabled, evaluate all other arguments and print a debug  
message from all of the value.  
Return t if the topic is enabled; nil otherwise.  
[special form defined in builtins/system.kt:89:1]

`debug-vars`  
macro `(debug-vars &rest vars)`  
[macro defined in preload/20-macros.lisp:117]

`decf`  
macro `(decf place &optional delta)`  
Decrement number-valued PLACE by DELTA (or 1); return new value.  
[macro defined in preload/30-places.lisp:9]

`declare`  
macro `(declare &rest declarations)`  
Declare variable types by making type assertions.  
A declaration like (declare (number n m) (string name) (symbol a b c))  
generates the corresponding type assertions for the named variables and  
their respectively declared types. For the available type symbols see  
the variable *object-types*.  
[macro defined in preload/20-macros.lisp:124]

`deep-copy`  
lambda function `(deep-copy ob) => value`  
Return a deep copy of object ob.  
[lambda function defined in preload/copy.lisp:3]

`deep-copy-cons`  
lambda function `(deep-copy-cons p) => value`  
Return a deep copy of cons P.  
See the documentation of function #'copy for details.  
[lambda function defined in preload/copy.lisp:12]

`deep-copy-table`  
lambda function `(deep-copy-table tbl) => value`  
Return a deep copy of table TBL.  
See the documentation of function #'copy for details.  
[lambda function defined in preload/copy.lisp:27]

`deep-copy-vector`  
lambda function `(deep-copy-vector v) => value`  
Return a deep copy of vector V.  
See the documentation of function #'copy for details.  
[lambda function defined in preload/copy.lisp:19]

`define-hook`  
builtin function `(define-hook hook-symbol &optional function) => hook-symbol`  
Define a hook `hook-symbol` for later use.  
[builtin function defined in builtins/system.kt:1267:1]

`define-repl-short-command`  
macro `(define-repl-short-command name docstring &rest bodyforms)`  
Define short command `name` for the repl with `docstring` and `bodyforms`.  
`name` must be a :keyword.  
[macro defined in preload/short-commands.lisp:34]

`defmacro`  
special form `(defmacro name lambda-list &optional docstring &rest bodyforms) => name`  
Create a macro with name `name`, parameters `lambda-list`, and `bodyforms`.  
`params` is a list of parameter symbols for the function.  
Optional `docstring` should describe what the function does.  
Return the function symbol `name`.  
  
On calling the function, `bodyforms` will be evaluated in an environment  
with the parameters bound to the actual arguments. The value of the last  
form evaluated will be returned.  
[special form defined in builtins/macros.kt:23:1]

`defparameter`  
special form `(defparameter symbol &optional initial-value docstring) => symbl`  
Define variable `symbol` with optional `initial-value` and `docstring`.  
If the variable is already bound, its value is changed nonetheless.  
[special form defined in builtins/basic.kt:1490:1]

`defsetf`  
macro `(defsetf access-fn update-fn-or-lambda-list &rest forms)`  
Define an update-function for an access-function, to be used by setf.  
Short form:  
    (defsetf access-fn update-fn)  
where both ACCESS-FN and UPDATE-FN are function symbols.  
  
Long form:  
    (defsetf access-fn (lambda-list) forms ...)  
defines an update function with the parameters specified in the  
LAMBDA-LIST, and the forms executed for update in a body where  
these parameters are bound accordingly.  
  
In both cases the update function is called with the arguments of the access  
function plus the value argument.  
[macro defined in preload/30-places.lisp:41]

`defun`  
special form `(defun name lambda-list &optional docstring &rest bodyforms) => name`  
Create a function with name `name`, `lambda-list`, and `bodyforms`.  
Optional `docstring` should describe what the function does.  
Return the function symbol `name`.  
On calling the function, `bodyforms` will be evaluated in an environment  
with the parameters bound to the actual arguments. The value of the last  
form evaluated will be returned.  
[special form defined in builtins/basic.kt:537:1]

`defvar`  
special form `(defvar symbol &optional initial-value docstring) => symbol`  
Define variable `symbol` with optional `initial-value` and `docstring`.  
If the variable is already bound, its value is not changed.  
[special form defined in builtins/basic.kt:1460:1]

`delete`  
builtin function `(delete item sequence) => result-sequence`  
Return a sequence from which the occurences of `item` have been removed.  
The sequence may be modified.  
[builtin function defined in builtins/sequences.kt:170:1]

`delete-file`  
builtin function `(delete-file pathname) => t`  
Delete the file denoted by `pathname` and return t on success.  
On failure, signal an error.  
[builtin function defined in builtins/files.kt:80:1]

`denominator`  
builtin function `(denominator number) => number`  
Return the denominator of `number`.  
As only real numbers are supported, the denominator of a number is  
always 1.  
[builtin function defined in builtins/numbers.kt:1014:1]

`describe`  
builtin function `(describe object) => description-list`  
Describe `object` -- return a alist with the object's attributes.  
[builtin function defined in builtins/system.kt:354:1]

`digit-char-p`  
builtin function `(digit-char-p char &optional (radix 10)) => weight`  
If `char` is a digit in the given `radix`, return its integer value, or nil.  
The radix must be in the range 2..36.  
[builtin function defined in builtins/numbers.kt:1525:1]

`directory`  
builtin function `(directory pathspec) => pathnames`  
Return a list of pathnames matching `pathspec`.  
The last path component may contain wildcard characters.  
[builtin function defined in builtins/files.kt:60:1]

`directory-namestring`  
builtin function `(dirname pathspec) => file-dirname`  
Return the dirname of a pathname, meaning only the directory part(s).  
[builtin function defined in builtins/files.kt:40:1]

`directory-p`  
builtin function `(directory-p pathspec) => t/nil`  
Return t if the file denoted by `pathspec` is a directory, nil else.  
[builtin function defined in builtins/files.kt:320:1]

`dirname`  
builtin function `(dirname pathspec) => file-dirname`  
Return the dirname of a pathname, meaning only the directory part(s).  
[builtin function defined in builtins/files.kt:40:1]

`div`  
builtin function `(div number divisor) => number`  
Return the result of the integer division of `number` by `divisor`.  
[builtin function defined in builtins/numbers.kt:1465:1]

`do-symbols`  
special form `(do-symbols control-vars &rest bodyforms) => result`  
(do-symbols (var [result-form]) bodyforms...)  
Iterate over all symbols, bind them to `var`, and execute `bodyforms`.  
If `result-form` is specified, evaluate and return it afterwards.  
[special form defined in builtins/system.kt:181:1]

`doc`  
builtin function `(doc symbol-or-function &optional return-as-string synopsis-only) => object`  
Return or print the documentation for `arg`, if available.  
If optional `return-as-string` is true, return the docstring as a string,  
otherwise print it and return `*the-non-printing-object*`.  
If optional `synopsis-only` is true, print or return the function's  
synopsis only.  
[builtin function defined in builtins/system.kt:121:1]

`dolist`  
macro `(dolist cvars &rest bodyforms)`  
(dolist (var listform &optional resultform) &rest bodyforms)  
[macro defined in preload/20-macros.lisp:27]

`doseq`  
special form `(doseq control-vars &rest bodyforms) => result`  
Bind `var` to every element of `seq`, execute body and return the result.  
`control-vars` is a list of (var seq [result-form [start [end]]]])  
[special form defined in builtins/sequences.kt:106:1]

`dotimes`  
macro `(dotimes countargs &rest bodyforms)`  
(dotimes (var count-form &optional result-form) &rest bodyforms)  
dotimes evaluates count-form, which should produce an integer. If  
count-form is zero or negative, the body is not executed. dotimes then  
executes the body once for each integer from 0 up to but not including  
the value of count-form, in the order in which the statements occur,  
with var bound to each integer. Then result-form is evaluated. At the  
time result-form is processed, var is bound to the number of times the  
body was executed. [CLHS]  
[macro defined in preload/20-macros.lisp:37]

`dotimes1`  
macro `(dotimes1 countargs &rest bodyforms)`  
(dotimes1 (var count-form &optional result-form) &rest bodyforms)  
dotimes evaluates count-form, which should produce an integer. If  
count-form is zero or negative, the body is not executed. dotimes then  
executes the body once for each integer from 1 up to including the  
value of count-form, in the order in which the statements occur, with  
var bound to each integer. Then result-form is evaluated. At the time  
result-form is processed, var is bound to the number of times the body  
was executed.  
[macro defined in preload/20-macros.lisp:57]

`e`  
builtin function `(e) => e`  
Return the value of the constant E.  
[builtin function defined in builtins/numbers.kt:1124:1]

`easter`  
builtin function `(easter year) => year-month-day`  
Calculate the easter date for `year`; return as a list (year month mday).  
The formula to calculate the date is according to Lichtenberg as cited by  
Wikipedia in  
https://de.wikipedia.org/wiki/Gau%C3%9Fsche_Osterformel#Eine_erg.C3.A4nzte_Osterformel  
[builtin function defined in builtins/numbers.kt:601:1]

`elements`  
builtin function `(elements sequence) => elements-list`  
Return a list with all elements of `sequence`, in order.  
[builtin function defined in builtins/sequences.kt:63:1]

`elt`  
builtin function `(elt sequence index &optional default) => element`  
Return the value of `sequence` (list, vector) element at `index`.  
Throw an error if the (zero-based) `index` is not in the sequence.  
[builtin function defined in builtins/sequences.kt:20:1]

`env-table`  
builtin function `(env-table &optional environment) => table`  
Return a table with all variables and values in the current environment.  
Optionally, specify an environment.  
[builtin function defined in builtins/system.kt:593:1]

`env-vars`  
builtin function `(env-vars &optional environment) => var-list`  
Return a list with all variables in the current environment.  
Optionally, specify an environment.  
[builtin function defined in builtins/system.kt:623:1]

`environmentp`  
builtin function `(environmentp object) => t/nil`  
Return t if `object` is an environment, else nil.  
[builtin function defined in builtins/basic.kt:948:1]

`eq`  
builtin function `(eq arg1 arg2) => value`  
Return t if arguments have the same atomic value or are the same object.  
Strings and numbers are atomic, and equal strings/numbers are also eq.  
[builtin function defined in builtins/basic.kt:597:1]

`eql`  
builtin function `(eq arg1 arg2) => value`  
Return t if arguments have the same atomic value or are the same object.  
Strings and numbers are atomic, and equal strings/numbers are also eq.  
[builtin function defined in builtins/basic.kt:597:1]

`equal`  
builtin function `(equal obj1 obj1) => t/nil`  
Return non-nil iff the arguments are the same or have the same contents.  
[builtin function defined in builtins/basic.kt:1655:1]

`error`  
builtin function `(error message-format &rest format-args) => no-return`  
Raise error with `message-format` and optional `format-args`.  
The error exits all active calls immediately, except for errset.  
For the error message, the `format-args` will be formatted using  
`message-format` as a format string.  
[builtin function defined in builtins/basic.kt:676:1]

`error-object-p`  
builtin function `(error-object-p object) => t/nil`  
Return t iff `object` is an error object, nil else.  
[builtin function defined in builtins/system.kt:1063:1]

`errorp`  
builtin function `(errorp object) => t/nil`  
Return t if `object` is an error object, else nil.  
[builtin function defined in builtins/basic.kt:966:1]

`errset`  
special form `(errset expr &optional (print-error t)) => result`  
Return the value of `expr` as a singleton list; on error return nil.  
In the latter case, a description of the error is in *last-error*, and,  
if optional `print-error` is non-nil or omitted, it is printed as well.  
[special form defined in builtins/basic.kt:835:1]

`eval`  
builtin function `(eval expr &optional environment) => value`  
Evaluate `expr` in optional `environment` and return the value.  
`environment` may be nil, in which case the current environment is used.  
[builtin function defined in builtins/basic.kt:1675:1]

`evenp`  
builtin function `(evenp n) => t/nil`  
Return a true value iff the number is an even integer.  
[builtin function defined in builtins/numbers.kt:780:1]

`example-startup-hook-function`  
lambda function `(example-startup-hook-function load-files expr-list other-args) => value`  
Print the arguments passed to lyk.  
This is an example for a startup hook function.  
[lambda function defined in preload/other.lisp:99]

`exit`  
builtin function `(exit &optional (exit-status 0)) => none`  
End the lisp interpreted with (optional) exit status.  
[builtin function defined in builtins/system.kt:530:1]

`exp`  
builtin function `(exp power) => result`  
Return e raised to the power of `power`.  
[builtin function defined in builtins/numbers.kt:1180:1]

`expand-file-name`  
builtin function `(expand-file-name filename &optional default-directory) => pathname`  
Convert `filename` to absolute, and canonicalize it.  
Second arg `default-directory` is the directory to start with if  
NAME is relative (does not start with slash or tilde).  
If DEFAULT-DIRECTORY is nil or missing, the process's current working  
directory is used.  
`filename` should be a string that is a valid file name for the  
underlying filesystem.  
  
File name components that are ‘.’ are removed, and so are file name  
components followed by ‘..’, along with the ‘..’ itself; note that  
these simplifications are done without checking the resulting file  
names in the file system.  
  
Multiple consecutive slashes are collapsed into a single slash.  
  
An initial "~" in `filename` expands to your home directory.  
An initial "~USER" in `filename` expands to USER’s home directory.  
If USER doesn’t exist, "~USER" is not expanded.  
[This function documentation is copied from GNU Emacs, with a few  
changes.]  
[builtin function defined in builtins/files.kt:188:1]

`expt`  
builtin function `(expt base power) => result`  
Return `base` raise to the power `power`.  
[builtin function defined in builtins/numbers.kt:970:1]

`factor`  
builtin function `(factor int) => factor-list`  
Return the list of prime factors of `int`.  
[builtin function defined in builtins/factor.kt:119:1]

`fboundp`  
builtin function `(fboundp symbol) => t/nil`  
Return t if a function is bound to `symbol`, nil otherwise.  
[builtin function defined in builtins/basic.kt:811:1]

`fceiling`  
builtin function `(fceiling number) => result`  
Return `number` truncated to an integer towards positive infinity.  
[builtin function defined in builtins/numbers.kt:1142:1]

`ffloor`  
builtin function `(ffloor number) => number`  
Return `number` truncated to an integer towards negative infinity.  
[builtin function defined in builtins/numbers.kt:700:1]

`file-author`  
builtin function `(file-author pathspec) => string`  
Return the author of the file `pathname`.  
The pathspec is either a pathname string or an open stream.  
[builtin function defined in builtins/files.kt:117:1]

`file-io-stream-p`  
builtin function `(file-io-stream-p object) => t/nil`  
Return t iff `object` is a file io stream, nil else.  
[builtin function defined in builtins/io.kt:577:1]

`file-length`  
builtin function `(file-length pathspec) => number`  
Return the length of the file `pathspec` in bytes.  
The pathspec is either a pathname string or an open stream.  
[builtin function defined in builtins/files.kt:137:1]

`file-namestring`  
builtin function `(basename pathspec) => file-basename`  
Return the basename of a pathname, meaning without the directory part(s).  
[builtin function defined in builtins/files.kt:21:1]

`file-reader-stream-p`  
builtin function `(file-reader-stream-p object) => t/nil`  
Return t iff `object` is a file reader stream, nil else.  
[builtin function defined in builtins/io.kt:502:1]

`file-writer-stream-p`  
builtin function `(file-writer-stream-p object) => t/nil`  
Return t iff `object` is a file writer stream, nil else.  
[builtin function defined in builtins/io.kt:559:1]

`filter`  
lambda function `(filter predicate l) => value`  
Return a list of elements from list L for which PREDICATE is true.  
[lambda function defined in preload/lists.lisp:12]

`find`  
builtin function `(find item sequence &key :from-end :test :test-not (:start 0) :end :key) => element`  
Return first `item` from `sequence` if it is in `sequence`, or nil.  
If `from-end` is true, return the last `item` found instead.  
If `test` is not nil, it is used as a function to check the equality  
of the elements of the sequence with `item`. Per default, use `equal`.  
If `test-not` is not nil, it is used as a function to check the  
non-equality of the elements of the sequence with `item`.  
Use `start` as the start index of the subsequence to search.  
Use `end`, if non-nil, as the end index of the subsequence to search.  
Use `key`, if non-nil, as a function applied to the elements of the  
sequence before testing against the result.  
[builtin function defined in builtins/sequences.kt:249:1]

`find-if`  
builtin function `(find-if predicate sequence &key :from-end (:start 0) :end :key) => element`  
Return first element from `sequence` where `predicate` is true, or nil.  
If `from-end` is true, return the last `item` found instead.  
Use `start` as the start index of the subsequence to search.  
Use `end`, if non-nil, as the end index of the subsequence to search.  
Use `key`, if non-nil, as a function applied to the elements of the  
sequence before testing against the result.  
[builtin function defined in builtins/sequences.kt:325:1]

`find-if-not`  
builtin function `(find-if-not predicate sequence &key :from-end (:start 0) :end :key) => element`  
Return first element from `sequence` where `predicate` is false, or nil.  
If `from-end` is true, return the last `item` found instead.  
Use `start` as the start index of the subsequence to search.  
Use `end`, if non-nil, as the end index of the subsequence to search.  
Use `key`, if non-nil, as a function applied to the elements of the  
sequence before testing against the result.  
[builtin function defined in builtins/sequences.kt:349:1]

`finish-output`  
builtin function `(finish-output &optional output-stream) => nil`  
Flush pending output to `output-stream` and then return.  
If `output-stream` is not specified, use *stdout*.  
[builtin function defined in builtins/io.kt:614:1]

`flet`  
special form `(flet bindings &rest bodyforms) => value`  
Bind one or more functions to symbols and evaluate `bodyforms`.  
The `bindings` are of the form (symbol (lambda-list) . bodyforms).  
[special form defined in builtins/basic.kt:1581:1]

`float`  
lambda function `(float number) => value`  
Return the argument as a float.  
This is a null operation for a number argument (as all numbers are floats).  
For any other argument type, this function raises an error.  
[lambda function defined in preload/numeric.lisp:17]

`floor`  
builtin function `(floor number) => number`  
Return `number` truncated to an integer towards negative infinity.  
[builtin function defined in builtins/numbers.kt:688:1]

`fmakunbound`  
builtin function `(fmakunbound symbol) => symbol`  
Make `symbol`s function be undefined, return `symbol`.  
[builtin function defined in builtins/basic.kt:887:1]

`for`  
macro `(for params &rest bodyforms)`  
(for (var from to [step [test]])  
The for loop uses `var' as the counter variable, starting with `from',  
adding `step' to `var' after each run, ending when `(test var to)' no  
longer is true. The default step is 1; the default test is #'<.  
[macro defined in preload/40-macros-using-places.lisp:3]

`format`  
builtin function `(format destination format-string &rest args) => nil-or-string`  
Format `args` according to `format-string` and write to `dest` (stream,  
t, or nil). nil means return the result as a string, t means write to  
standard output.  
The format string is modeled after Common Lisp's, but lacks many of its  
properties. Currently the following format directives are implemented:  
  ~A : Aesthetic  
  ~S : Standard  
  ~C : Character  
  ~F : Fixed format floating point  
  ~% : Newline  
  ~& : Fresh line  
  ~| : Page separator  
  ~~ : Tilde  
  ~#\Newline : Ignored newline  
[builtin function defined in builtins/io.kt:281:1]

`fround`  
builtin function `(fround number) => number`  
Return `number` rounded to the nearest integer.  
Ties (<integer> + 0.5) are rounded towards an even integer.  
[builtin function defined in builtins/numbers.kt:652:1]

`fset`  
builtin function `(fset symbol new-func) => new-func`  
Set function of `symbol` to `new-func` (a function) and return `new-func`.  
[builtin function defined in builtins/basic.kt:1437:1]

`ftruncate`  
builtin function `(ftruncate number) => result`  
Return `number` truncated towards zero.  
[builtin function defined in builtins/numbers.kt:1318:1]

`funcall`  
builtin function `(funcall function &rest arguments) => value`  
Apply `function` to `arguments` and return the result value.  
[builtin function defined in builtins/basic.kt:907:1]

`function`  
special form `(function arg) => function`  
Return the function value of a symbol, or the argument if it is a function.  
[special form defined in builtins/basic.kt:617:1]

`function-body`  
builtin function `(function-body function) => bodyforms`  
Return the body forms of `function`.  
The returned object is a list of the actual body forms of the function,  
not a copy; modifying it will have direct impact on the function's  
behaviour.  
[builtin function defined in builtins/basic.kt:1910:1]

`function-definition`  
builtin function `(function-definition function) => function-definition-form`  
Return `function`'s definition form.  
`function` must be a lambda function or a macro, not a builtin.  
The function definition returned contains the real function body,  
not a copy.  
[builtin function defined in builtins/system.kt:651:1]

`function-docstring`  
builtin function `(function-docstring function) => docstring`  
Return `function`'s docstring.  
[builtin function defined in builtins/system.kt:669:1]

`function-parameters`  
builtin function `(function-parameters function) => parameter-list`  
Return the parameter list of `function`.  
The returned list is a reconstruction of the original parameter list;  
modifying it will not change the function's behaviour.  
[builtin function defined in builtins/system.kt:689:1]

`function-symbols`  
lambda function `(function-symbols) => value`  
Return a list of all function symbols.  
[lambda function defined in preload/other.lisp:40]

`functionp`  
builtin function `(functionp object) => t/nil`  
Return t if `object` is a function, else nil.  
[builtin function defined in builtins/basic.kt:1153:1]

`gc`  
builtin function `(gc) => nil`  
Trigger a garbage collection.  
[builtin function defined in builtins/system.kt:207:1]

`gcd`  
lambda function `(gcd n1 n2) => value`  
Return the greatest common divisor of the arguments.  
[lambda function defined in preload/numeric.lisp:25]

`gensym`  
builtin function `(gensym &optional (prefix "G#")) => symbol`  
Return a new, uninterned and unused symbol with a name prefix \"G#\".  
The symbol is not bound or fbound and has an empty property list.  
If a different `prefix` is given, it is tried as the name for the  
symbol or, if the name is already in use, as the prefix of the name.  
[builtin function defined in builtins/basic.kt:1337:1]

`get`  
builtin function `(get symbol property &optional default-value) => value`  
Return the value of `symbol`'s property `property` (or nil, if not set).  
[builtin function defined in builtins/basic.kt:1700:1]

`get-hook-function`  
builtin function `(get-hook-function hook-symbol) => function`  
Return the function of hook `hook-symbol` (may be nil).  
[builtin function defined in builtins/system.kt:1314:1]

`get-hooks`  
builtin function `(get-hooks) => alist`  
Return an alist with (hooksym . function) pairs for all defined hooks.  
[builtin function defined in builtins/system.kt:1245:1]

`get-iso-time`  
lambda function `(get-iso-time &optional (universal-time (get-universal-time-ns)) &key time-zone (long-form t) fractions append-blank) => value`  
Format the UNIVERSAL-TIME according to ISO 8601 and return the string.  
The default is the current time.  
Key :long-form may be true for long form (default) or nil for short.  
Key fractions may be :ms, :us, or :ns for milliseconds, microseconds, or  
nanoseconds, respectively.  
[lambda function defined in preload/time.lisp:2]

`get-output-stream-string`  
builtin function `(get-output-stream-string string-output-stream) => string`  
Return a string with the contents written to `string-output-stream`.  
This operation clears any characters on `string-output-stream`, so  
the string contains only those characters which have been output since  
the last call to get-output-stream-string or since the creation of the  
string-output-stream, whichever occurred most recently.  
[builtin function defined in builtins/io.kt:460:1]

`get-program-output`  
lambda function `(get-program-output command &key capture-all input error-output (raise-error t) (in-shell t)) => value`  
Run program `command` and return its standard output as a string.  
Raise an error if the program returned a non-zero exit status and the  
keyword argument :raise-error is true.  
  
If &key argument :capture-all is true, capture the error output,  
too, and return a list of exit status, standard output and error output  
of the program run as strings.  
[lambda function defined in preload/other.lisp:55]

`get-working-directory`  
builtin function `(get-working-directory) => pathname`  
Return the current working directory as a string.  
[builtin function defined in builtins/files.kt:226:1]

`getenv`  
builtin function `(getenv variable) => string-value`  
Return the value of `variable` in the process environment.  
If it is not defined, return an empty string.  
[builtin function defined in builtins/system.kt:551:1]

`identity`  
builtin function `(identity arg) => arg`  
Return `arg` unchanged.  
[builtin function defined in builtins/basic.kt:1928:1]

`if`  
macro `(if condition then-clause &rest else-clauses)`  
If `condition` evals to non-nil, eval `then-clause` and return the value.  
Otherwise, evaluate `else-clauses` and return the last value.  
[macro defined in preload/20-macros.lisp:15]

`ignore`  
builtin function `(ignore &rest args) => nil`  
Return nil  
[builtin function defined in builtins/basic.kt:1946:1]

`imagpart`  
builtin function `(imagpart number) => number`  
Return the imagpart of `number`.  
As only real numbers are supported, the imaginary part of a number is  
always zero.  
[builtin function defined in builtins/numbers.kt:1058:1]

`incf`  
macro `(incf place &optional delta)`  
Increment number-valued PLACE by DELTA (or 1); return new value.  
[macro defined in preload/30-places.lisp:3]

`input-stream-p`  
builtin function `(input-stream-p object) => t/nil`  
Return t if `object` is an input stream, nil else.  
[builtin function defined in builtins/io.kt:662:1]

`integer-length`  
builtin function `(integer-length integer) => length`  
Returns the number of bits needed to represent `integer`.  
If the argument is not an integer, it will be rounded to the nearest one.  
[builtin function defined in builtins/numbers.kt:941:1]

`integerp`  
builtin function `(integerp object) => t/nil`  
Return true iff object is an integer number  
[builtin function defined in builtins/numbers.kt:736:1]

`interactive-stream-p`  
builtin function `(interactive-stream-p object) => t/nil`  
Return t if `object` is an interactive stream, nil else.  
[builtin function defined in builtins/io.kt:706:1]

`intern`  
builtin function `(intern name) => symbol`  
Return the (maybe new) interned symbol with the name `name` (a string).  
[builtin function defined in builtins/basic.kt:96:1]

`iota`  
lambda function `(iota count &optional (start 0) (step 1)) => value`  
Return a list of numbers of length COUNT.  
The list starts with optional START (default 0), with each element being  
bigger than the previous one by optional STEP (default 1).  
[lambda function defined in preload/lists.lisp:43]

`isqrt`  
builtin function `(isqrt number) => number`  
Return the integer square root of `number`.  
[builtin function defined in builtins/numbers.kt:475:1]

`join`  
builtin function `(join items &optional (sep " ")) => string`  
Make a string from all `items` (a sequence), separated by `sep`  
and return it.  
[builtin function defined in builtins/strings.kt:82:1]

`keywordp`  
builtin function `(keywordp expr) => t/nil`  
Return t if the argument is a keyword symbol, nil otherwise.  
A keyword is a symbol whose name starts with a colon, but is  
longer than just the colon.  
[builtin function defined in builtins/basic.kt:2058:1]

`known-primes`  
builtin function `(known-primes) => prime-list`  
Return a list of the consecutive prime numbers known so far.  
[builtin function defined in builtins/factor.kt:178:1]

`lambda`  
special form `(lambda lambda-list &rest bodyforms) => function`  
Return an anonymous function with `lambda-list` and `bodyforms`.  
[special form defined in builtins/basic.kt:511:1]

`lambdap`  
builtin function `(lambdap object) => t/nil`  
Return t iff `object` is a lambda, nil else.  
[builtin function defined in builtins/basic.kt:2038:1]

`last`  
builtin function `(last l) => value`  
Return the last pair of list `l`, or nil if `l` is nil.  
[builtin function defined in builtins/basic.kt:1518:1]

`lcm`  
lambda function `(lcm &rest args) => value`  
Return the least common multiple of all arguments.  
[lambda function defined in preload/numeric.lisp:42]

`length`  
builtin function `(length sequence) => number`  
Return the length of `sequence` as a number.  
[builtin function defined in builtins/basic.kt:1207:1]

`let`  
special form `(let bindings &rest bodyforms) => value`  
Evaluate bodyforms with local bindings, return value of last bodyform.  
Bindings are of the form `symbol` or `(symbol)` or `(symbol value)`,  
where the first two bind `symbol` to nil. All `value`s are evaluated  
before any variable bindings are done.  
[special form defined in builtins/basic.kt:346:1]

`let*`  
special form `(let* bindings &rest bodyforms) => value`  
Evaluate bodyforms with local bindings, return value of last bodyform.  
Bindings are of the form `symbol` or `(symbol)` or `(symbol value)`,  
where the first two bind `symbol` to nil. VALUE is evaluated with  
bindings of earlier variables in the same let* already in place.  
[special form defined in builtins/basic.kt:367:1]

`list`  
builtin function `(list &rest elems) => list`  
Return a list with the elements `elems`.  
[builtin function defined in builtins/basic.kt:114:1]

`list*`  
builtin function `(list* &rest elems+) => list`  
Return a list of `elems`, with the last as the end of the list.  
list* is like list except that the last argument to list becomes  
the car of the last cons constructed, while the last argument to  
list* becomes the cdr of the last cons constructed.  
[builtin function defined in builtins/basic.kt:135:1]

`list-collector`  
builtin function `(list-collector) => closure`  
Create a list collector closure and return it as a function.  
  
The returned function takes an arbitrary number of arguments, which  
are then added to the end of the list, and returns the list. The  
normal use case would be to call it a number of times to add items to  
the list, and then call it once without arguments to return the  
resulting list. This is more efficient than using append repeatedly.  
  
Example:  
    (let ((lc (list-collector)))  
      (lc 'see)  
      (lc 0 'evil)  
      (lc 'hear \"no\" 'evil)  
      (lc))  
    => (see 0 evil hear \"no\" evil)  
[builtin function defined in builtins/utils.kt:30:1]

`listp`  
builtin function `(listp object) => t/nil`  
Return t if `object` is a list, else nil.  
[builtin function defined in builtins/basic.kt:1110:1]

`load`  
builtin function `(load filename &key (:verbose t) :print (:error t)) => t/nil`  
Load specified file; return t if the contents was evaluated without error.  
If the filename contains a slash, it is used exactly as given. Otherwise,  
try to find the load file in the directories named in *load-path*. Try the  
filename as given, then with a ".l" suffix, then with  a ".lisp" suffix.  
If keyword verbose is nil (the default is true), do not print an  
informational message after loading.  
If keyword `error` is nil (the default is true), do not raise an error for  
an unfound file, but return nil instead.  
If keyword `print` is true, (the default is nil), the load progress is  
shown by printing the values of the top-level forms evaluated. Otherwise,  
the value of the variable *load-print* is used to determine printing.  
[builtin function defined in builtins/io.kt:199:1]

`load-lyk-rc-file`  
lambda function `(load-lyk-rc-file &rest ignored) => value`  
Load lyk-rc-file, if it exists.  
[lambda function defined in preload/hooks.lisp:6]

`load-preload-code`  
builtin function `(load-preload-code &optional verbose) => object`  
Load the preload code.  
This makes only sense if it hasn't been loaded before.  
[builtin function defined in builtins/system.kt:1141:1]

`log`  
builtin function `(log number &optional (base 2.718281828459045)) => result`  
Return the logarithm of `number` in base `base` (defaults to e).  
[builtin function defined in builtins/numbers.kt:1160:1]

`loop`  
special form `(loop &rest bodyforms) => none`  
Eval `bodyforms` again and again.  
[special form defined in builtins/basic.kt:1243:1]

`lyk-command-options`  
builtin function `(lyk-command-options) => alist`  
Return a alist of lyk's command line options and their values.  
The debug options will not be shown; use `(set-debug)` for those.  
[builtin function defined in builtins/system.kt:1353:1]

`macroexpand`  
builtin function `(macroexpand form) => expanded-form`  
Expand macros in `form` and return the expanded form.  
macroexpand repeatedly expands form until it is no longer a macro form.  
[builtin function defined in builtins/macros.kt:48:1]

`macroexpand-1`  
builtin function `(macroexpand-1 form) => expanded-form`  
Do one step of macro expansion in `form` and return the expanded form.  
[builtin function defined in builtins/macros.kt:66:1]

`macrop`  
builtin function `(macrop object) => t/nil`  
Return t if `object` is a macro, else nil.  
[builtin function defined in builtins/basic.kt:1189:1]

`make-list`  
lambda function `(make-list n el) => value`  
Return a list of length `n` with elements `el`.  
`el` may be a parameter-less function; in that case it is called for  
 each place on the list, and its return value is filled in as the  
element of the list.  
[lambda function defined in preload/lists.lisp:31]

`make-string`  
builtin function `(make-string length &optional (initial " ")) => string`  
Return a string of `length` with the contents taken from `initial`.  
[builtin function defined in builtins/strings.kt:38:1]

`make-string-input-stream`  
builtin function `(make-string-input-stream string) => stream`  
Return a string input stream. This stream will supply, in order, the  
characters in the string.  
[builtin function defined in builtins/io.kt:224:1]

`make-string-output-stream`  
builtin function `(make-string-output-stream) => string-stream`  
Return a output string stream that stores the text that is output to it.  
See get-output-stream-string for returning the contents of that stream.  
[builtin function defined in builtins/io.kt:438:1]

`make-symbol`  
builtin function `(make-symbol name) => symbol`  
Create and return a fresh, uninterned symbol whose name is `name`.  
The new symbol is neither bound nor fbound and has a null property list.  
[builtin function defined in builtins/system.kt:709:1]

`make-table`  
builtin function `(make-table &rest pairs) => table`  
Return a new table, optionally filled with `pairs`.  
Every argument that is not a pair will be used as (arg . nil).  
[builtin function defined in builtins/tables.kt:60:1]

`make-vector`  
builtin function `(make-vector length &optional initial) => vector`  
Return a new vector of length `length` and each element set to `initial`.  
[builtin function defined in builtins/vectors.kt:49:1]

`makunbound`  
builtin function `(makunbound symbol) => symbol`  
Make `symbol`s value be undefined, return `symbol`.  
[builtin function defined in builtins/basic.kt:867:1]

`map`  
builtin function `(mapcar function &rest lists+) => value-list``  
Apply `function` to the first members of the argument lists, then the  
second and so on; return the list of resulting values. Any excess  
values are discarded.  
Example:  
  (mapcar #'cons '(3 4 5) '(a b c d))  
  => ((3 . a) (4 . b) (5 . c))  
[builtin function defined in builtins/basic.kt:1819:1]

`mapcar`  
builtin function `(mapcar function &rest lists+) => value-list``  
Apply `function` to the first members of the argument lists, then the  
second and so on; return the list of resulting values. Any excess  
values are discarded.  
Example:  
  (mapcar #'cons '(3 4 5) '(a b c d))  
  => ((3 . a) (4 . b) (5 . c))  
[builtin function defined in builtins/basic.kt:1819:1]

`max`  
builtin function `(max number &rest numbers) => maximum`  
Return the largest number of all arguments  
[builtin function defined in builtins/numbers.kt:1371:1]

`maybe-run-short-command`  
lambda function `(maybe-run-short-command expr) => value`  
Run a short command if `expr` is a keyword matching one.  
Return true iff a short command was run or at least attempted.  
[lambda function defined in preload/short-commands.lisp:8]

`measure-time`  
special form `(measure-time &rest bodyforms) => duration`  
Run bodyforms and return the elapsed time in seconds.  
[special form defined in builtins/system.kt:777:1]

`member`  
lambda function `(member item list &key (test (function eq))) => value`  
Find first ITEM in LIST and return the tail of the list beginning with item.  
Keyword :TEST specifies a test predicate function of two arguments to use  
instead of eq.  
[lambda function defined in preload/sequences.lisp:35]

`member-if`  
lambda function `(member-if predicate list) => value`  
Find first item in LIST for which PREDICATE is true  
and return the tail of the list beginning with this item.  
[lambda function defined in preload/sequences.lisp:45]

`member-if-not`  
lambda function `(member-if-not predicate list) => value`  
Find first item in LIST for which PREDICATE is false  
and return the tail of the list beginning with this item.  
[lambda function defined in preload/sequences.lisp:54]

`min`  
builtin function `(min number &rest numbers) => minimum`  
Return the smallest number of all arguments  
[builtin function defined in builtins/numbers.kt:1398:1]

`minusp`  
builtin function `(minusp number) => t/nil`  
Return true iff `number` is less than zero.  
[builtin function defined in builtins/numbers.kt:904:1]

`mod`  
builtin function `(mod number divisor) => modulus`  
Return the modulus of the division of `number` by `divisor`.  
[builtin function defined in builtins/numbers.kt:1445:1]

`namestring`  
builtin function `(namestring pathname) => normalized-pathname`  
Return `pathspec` as a pathname in normalized form.  
This means multiple consecutive slashes and /./ are reduced to a single  
slash, and /../ constructs are resolved.  
[builtin function defined in builtins/files.kt:280:1]

`nconc`  
builtin function `(nconc &rest lists) => value`  
Return a new list that is the concatenation of `lists`.  
The list structure of all but the last list is modified.  
[builtin function defined in builtins/basic.kt:1403:1]

`new-environment`  
builtin function `(new-environment &optional (parent-environment t) value-table) => environment`  
Return a new environment. Optional `parent-environment` is the parent,  
otherwise the current environment. If `parent-environment` is nil, there  
is no parent environment, i.e. a top-level environment is created; if  
`parent-environmenta is t (the default), the parent is the current  
environment. If `value-table` is non-nil, it is a table with symbol/value  
pairs to populate the new environment.  
[builtin function defined in builtins/environments.kt:21:1]

`next-prime`  
builtin function `(next-prime integer) => prime`  
Return the next prime number greater than `integer`.  
[builtin function defined in builtins/factor.kt:160:1]

`no-warnings`  
special form `(no-warnings &rest bodyforms) => result`  
Eval bodyforms and return the result while suppressing warnings  
(and other info/notice messages). If *warnings-as-errors* is true,  
the error is raised nevertheless.  
[special form defined in builtins/system.kt:801:1]

`no-warnings-as-errors`  
macro `(no-warnings-as-errors &rest bodyforms)`  
Evaluate `bodyforms` with *warnings-as-errors* set to nil.  
After that, restore the original value.  
[macro defined in preload/other.lisp:45]

`not`  
builtin function `(not expr) => t/nil`  
Return t if `expr` is nil, else nil.  
[builtin function defined in builtins/basic.kt:578:1]

`nreverse`  
builtin function `(nreverse list) => list`  
Reverse `list` (maybe by modifying a list's cdrs) and return the result.  
[builtin function defined in builtins/basic.kt:1783:1]

`nth`  
builtin function `(nth n list) => object`  
Return the `n`th element of `list`.  
[builtin function defined in builtins/basic.kt:1964:1]

`nthcdr`  
builtin function `(nthcdr n list) => object`  
Return the `n`th cons of `list`.  
[builtin function defined in builtins/basic.kt:1992:1]

`null`  
builtin function `(null expr) => t/nil`  
Return t if `expr` is nil, else nil.  
[builtin function defined in builtins/basic.kt:566:1]

`numberp`  
builtin function `(numberp object) => t/nil`  
Return t is `object` is a number, else nil.  
[builtin function defined in builtins/basic.kt:1002:1]

`numbers`  
builtin function `(numbers) => number-list`  
Return a list of all number objects currently in use.  
[builtin function defined in builtins/system.kt:160:1]

`numerator`  
builtin function `(numerator number) => number`  
Return the numerator of `number`.  
As only real numbers are supported, the numerator of a number is  
always the number itself.  
[builtin function defined in builtins/numbers.kt:992:1]

`oddp`  
builtin function `(oddp n) => t/nil`  
Return a true value iff the number is an odd integer.  
[builtin function defined in builtins/numbers.kt:806:1]

`open`  
builtin function `(open fname &key (:if-does-not-exist :error) (:direction :input) (:if-exists :overwrite)) => stream`  
Open a file (or reopen a stream) and return the connected stream.  
Options: :direction followed by :input or :output or :io,  
:if-exists followed by :new-version or :append or :overwrite  
or :supersede or :error or nil  
:if-does-not-exist followed by :error or :create or nil  
[builtin function defined in builtins/io.kt:317:1]

`open-interactive-stream`  
builtin function `(open-interactive-stream &optional (prompt "")) => interactive-stream`  
Return an interactive input stream.  
This is a stream from which can be read using the interactive line  
editor with `prompt`. If `prompt` is a function, it will be called  
with no arguments for every line, and its return value will be used  
as the line editor prompt.  
[builtin function defined in builtins/io.kt:644:1]

`open-stream-p`  
builtin function `(open-stream-p object) => t/nil`  
Return t if `object` is an open stream, nil else.  
[builtin function defined in builtins/io.kt:728:1]

`or`  
special form `(or &rest args) => value`  
Evaluate `args` until one is non-nil; return the last evaluated value.  
[special form defined in builtins/basic.kt:411:1]

`output-stream-p`  
builtin function `(output-stream-p object) => t/nil`  
Return t if `object` is an output stream, nil else.  
[builtin function defined in builtins/io.kt:684:1]

`parse-integer`  
builtin function `(parse-integer string &key (:start 0) :end :radix :junk-allowed) => integer`  
[builtin function defined in builtins/numbers.kt:1560:1]

`phase`  
builtin function `(phase number) => phase`  
Return the angle part of `number`'s polar representation.  
As only real numbers are supported, the return value is only -Pi for  
negative numbers, Pi for positive numbers, and zero for zero.  
[builtin function defined in builtins/numbers.kt:1080:1]

`pi`  
builtin function `(pi) => pi`  
Return the value of the constant Pi.  
[builtin function defined in builtins/numbers.kt:1106:1]

`plain-file-p`  
builtin function `(plain-file-p pathspec) => t/nil`  
Return t if the file denoted by `pathspec` is a plain file, nil else.  
[builtin function defined in builtins/files.kt:339:1]

`plural-ies`  
lambda function `(plural-ies arg) => value`  
Return a plural "ies" or singular "y" as appropriate for ARG.  
If ARG is a number, return "ies" if it is zero or greater than 1, but  
"y" if it is 1.  
If ARG is a sequence, do the same according to the number of it elements.  
[lambda function defined in preload/other.lisp:18]

`plural-s`  
lambda function `(plural-s arg) => value`  
Return a plural-s or an empty string as appropriate for ARG.  
If ARG is a number, return "s" if it is zero or greater than 1, but  
an empty string if it is 1.  
If ARG is a sequence, do the same for the number of it elements.  
[lambda function defined in preload/other.lisp:9]

`plusp`  
builtin function `(plusp number) => t/nil`  
Return true iff `number` is greater than zero.  
[builtin function defined in builtins/numbers.kt:922:1]

`pop`  
macro `(pop place)`  
Return the car of the value of PLACE; store the cdr of the value into PLACE.  
[macro defined in preload/30-places.lisp:24]

`position`  
builtin function `(position item sequence &key :from-end :test :test-not (:start 0) :end :key) => element`  
Return first index of `item` from `sequence` if it is in `sequence`, or nil.  
If `from-end` is true, return the last `item` found instead.  
If `test` is not nil, it is used as a function to check the equality  
of the elements of the sequence with `item`. Per default, use `equal`.  
If `test-not` is not nil, it is used as a function to check the  
non-equality of the elements of the sequence with `item`.  
Use `start` as the start index of the subsequence to search.  
Use `end`, if non-nil, as the end index of the subsequence to search.  
Use `key`, if non-nil, as a function applied to the elements of the  
sequence before testing against the result.  
[builtin function defined in builtins/sequences.kt:276:1]

`position-if`  
builtin function `(position-if predicate sequence &key :from-end (:start 0) :end :key) => element`  
Return first element from `sequence` where `predicate` is true, or nil.  
If `from-end` is true, return the last `item` found instead.  
Use `start` as the start index of the subsequence to search.  
Use `end`, if non-nil, as the end index of the subsequence to search.  
Use `key`, if non-nil, as a function applied to the elements of the  
sequence before testing against the result.  
[builtin function defined in builtins/sequences.kt:379:1]

`position-if-not`  
builtin function `(position-if-not predicate sequence &key :from-end (:start 0) :end :key) => element`  
Return first element from `sequence` where `predicate` is false, or nil.  
If `from-end` is true, return the last `item` found instead.  
Use `start` as the start index of the subsequence to search.  
Use `end`, if non-nil, as the end index of the subsequence to search.  
Use `key`, if non-nil, as a function applied to the elements of the  
sequence before testing against the result.  
[builtin function defined in builtins/sequences.kt:402:1]

`pow`  
lambda function `(pow base power) => value`  
Return BASE to the power of (integer) POWER.  
[lambda function defined in preload/numeric.lisp:1]

`prime-number-p`  
builtin function `(prime-number-p integer) => t/nil`  
Return true iff `integer` is a prime number.  
[builtin function defined in builtins/factor.kt:142:1]

`prime-numbers`  
lambda function `(prime-numbers start end) => value`  
Return a list of consecutive prime numbers with start <= prime < end.  
[lambda function defined in preload/numeric.lisp:72]

`prin1`  
builtin function `(prin1 arg &optional output-stream) => arg`  
Print `arg` to `stream` (or standard output) suitable for input to (read),  
with quoting and escaping where necessary.  
[builtin function defined in builtins/io.kt:76:1]

`prin1-to-string`  
builtin function `(prin1-to-string arg) => string`  
Print `arg` to a string as with prin1 and return the string.  
[builtin function defined in builtins/io.kt:96:1]

`princ`  
builtin function `(princ arg &optional output-stream) => arg`  
Print `arg` to `stream` (or standard output) without quoting or escaping.  
[builtin function defined in builtins/io.kt:114:1]

`princ-to-string`  
builtin function `(princ-to-string arg) => string`  
Print `arg` to a string without quoting or escaping and return the string.  
Also known as princs.  
[builtin function defined in builtins/io.kt:135:1]

`princs`  
builtin function `(princs arg) => string`  
Print `arg` to a string without quoting or escaping and return the string.  
Also known as princ-to-string.  
[builtin function defined in builtins/io.kt:149:1]

`print`  
builtin function `(print arg &optional output-stream) => arg`  
Print `arg` to `stream` (or standard output) suitable for input to (read),  
with quoting and escaping where necessary, preceded by a newline and  
followed by a blank.  
[builtin function defined in builtins/io.kt:55:1]

`print-file`  
lambda function `(print-file pathname &optional (stream *standard-output*)) => value`  
Print the contents of file `pathname` to `stream`.  
[lambda function defined in preload/short-commands.lisp:103]

`println`  
builtin function `(println &key (:sep " ") &rest args) => *the-non-printing-object*`  
Print all arguments, separated by :sep, terminated by a newline.  
[builtin function defined in builtins/io.kt:34:1]

`probe-file`  
builtin function `(probe-file pathspec) => result`  
If the file `pathspec` exists, return its truename, else nil.  
[builtin function defined in builtins/files.kt:298:1]

`process-env`  
builtin function `(process-env) => table`  
Return a table with all process environment variables and their values.  
[builtin function defined in builtins/system.kt:569:1]

`prog1`  
special form `(prog1 result-form &rest bodyforms) => first-value`  
Evaluate all forms and return the value of the first one.  
[special form defined in builtins/basic.kt:489:1]

`prog2`  
macro `(prog2 first-form result-form &rest otherforms)`  
Evaluate all forms and return the value of the second.  
[macro defined in preload/20-macros.lisp:145]

`progn`  
special form `(progn &rest bodyforms) => last-value`  
Evaluate all `bodyforms` and return the value of the last one.  
[special form defined in builtins/basic.kt:471:1]

`provide`  
builtin function `(provide feature) => feature`  
Declare `feature` (a symbol) as provided in case it will be required.  
[builtin function defined in builtins/system.kt:443:1]

`push`  
macro `(push element place)`  
Prepend ITEM to the list stored in PLACE and store the result in PLACE.  
Return the new value of PLACE.  
[macro defined in preload/30-places.lisp:15]

`put`  
builtin function `(put symbol property value) => value`  
Return the value of `symbol`'s property `property` (or nil, if not set).  
[builtin function defined in builtins/basic.kt:1721:1]

`quasiquote`  
special form `(quasiquote expr) => expanded-form`  
In `expr`, replace unquoted items them with their values as appropriate.  
Return the resulting form.  
  
»unquote-splicing« (»,@«). In the latter case, if the value is a list,  
splice it into the surrounding list.  
[special form defined in builtins/macros.kt:125:1]

`quit`  
builtin function `(exit &optional (exit-status 0)) => none`  
End the lisp interpreted with (optional) exit status.  
[builtin function defined in builtins/system.kt:530:1]

`quote`  
special form `(quote expr) => expr`  
Return the expression `expr` without evaluating it.  
[special form defined in builtins/basic.kt:192:1]

`random`  
builtin function `(random &optional limit int) => random-number`  
Return a non-negative pseudo-random number less than 1 (or `limit`).  
If optional `int` is non-nil, the returned number is an integer.  
[builtin function defined in builtins/numbers.kt:1337:1]

`range`  
lambda function `(range limit-or-start &optional limit-or-not (step 1)) => value`  
Return a list of numbers similar to the corresponding Python function.  
Invoked as (range <limit>), it starts at 0 and ends before limit.  
Invoked as (range <start> <limit>), it starts at start and ends before limit.  
Optional STEP is the step width between the list members, defaulting to 1.  
It is an error if any of the values is not an integer.  
[lambda function defined in preload/lists.lisp:53]

`rational`  
builtin function `(rational &rest &rest) => nothing`  
Function is not implemented; will throw an error.  
[builtin function defined in builtins/numbers.kt:868:1]

`rationalize`  
builtin function `(rationalize &rest &rest) => nothing`  
Function is not implemented; will throw an error.  
[builtin function defined in builtins/numbers.kt:886:1]

`read`  
builtin function `(read &optional input-stream eof-error-p eof-value) => value`  
Read an expression from `input-stream` (or stdin) and return it.  
`input-stream` may be a stream or a string.  
[builtin function defined in builtins/basic.kt:1541:1]

`read-line`  
builtin function `(read-line &optional input-stream (eof-error-p t) eof-value trim-nl) => line`  
Read a line from *stdin* (or `input-stream`) and return it as a string.  
If `eof-error-p` is true (which is the default), raise an error on EOF.  
Otherwise, return `eof-value` instead.  
If `trim-nl` is true, trim a trailin newline character from the line.  
[builtin function defined in builtins/io.kt:405:1]

`realpart`  
builtin function `(realpart number) => number`  
Return the realpart of `number`.  
As only real numbers are supported, the real part of a number is  
always the number itself.  
[builtin function defined in builtins/numbers.kt:1036:1]

`reduce`  
lambda function `(reduce func l) => value`  
Reduce a list L to a single value using FUNC and return the value.  
This is done by continually replacing the first two elements of L by the  
result of applying FUNC to both, until there is only one element left.  
[lambda function defined in preload/lists.lisp:21]

`regexp`  
builtin function `(regexp pattern) => regexp-object`  
Return a new regexp object built from `pattern` (a kind of string).  
See `regexp-match` for more information about regular expressions.  
[builtin function defined in builtins/strings.kt:103:1]

`regexp-match`  
builtin function `(regexp-match regexp string &optional limit) => value`  
Return a list of matches if regexp `regexp` matches `string`, nil else.  
The returned value in case of a match is a list of the values for the  
whole match and possible group matches. If an optional group (...)?  
does not match, its value is "".  
With optional third argument `limit`, a list of match lists for  
(potentially) multiple matches is returned. If `limit` is t, all matches  
are considered; otherwise, a number specifies the number of matches to  
be considered.  
Regular expression syntax is that of the Kotlin regexp package (RE2),  
which is largely similar to that of the Perl and Python languages.  
A \"(?flags)\" specification in the regexp can modify the behaviour  
of the match in the current group. Possible flags are:  
  
i  case-insensitive (default false)  
m  multi-line mode: ^ and $ match begin/end line in addition to begin/end  
   text (default false)  
s  let . match \\n (default false)  
[builtin function defined in builtins/strings.kt:142:1]

`regexp-replace`  
builtin function `(regexp-replace re string replacement &optional (limit 0)) => t/nil`  
Return t if regexp `re` replacees `string`, nil else.  
[builtin function defined in builtins/strings.kt:204:1]

`regexp-split`  
builtin function `(regexp-split re string &optional (limit 0)) => string-list`  
Split the string around regexp matches and return a list of the parts.  
If `limit` is greater than (default) 0, only so many parts are split  
[builtin function defined in builtins/strings.kt:184:1]

`regexpp`  
builtin function `(regexpp object) => t/nil`  
Return t is `object` is a regexp, else nil.  
[builtin function defined in builtins/basic.kt:1038:1]

`rem`  
builtin function `(rem number divisor) => remainder`  
Return the remainder of the division of `number` by `divisor`.  
[builtin function defined in builtins/numbers.kt:1425:1]

`remove-if`  
lambda function `(remove-if pred l) => value`  
Remove items from list L for which predicate PRED is true.  
[lambda function defined in preload/lists.lisp:2]

`remprop`  
builtin function `(remprop symbol property) => value`  
Remove `property` from `symbol` and return the previous value (or Nil).  
[builtin function defined in builtins/basic.kt:1743:1]

`require`  
builtin function `(require feature &optional filename) => t`  
If `feature` (a symbol) is not already provided, load it from `filename`  
(default: name of the feature). If the feature is still not provided,  
throw an error.  
[builtin function defined in builtins/system.kt:465:1]

`return`  
builtin function `(return &optional value) => none`  
Return from the innermost function with `value` (defaults to nil).  
[builtin function defined in builtins/system.kt:1104:1]

`reverse`  
builtin function `(reverse sequence) => list`  
Reverse `sequence` (by copying) and return the result.  
The resulting sequence does not share structure with the argument list.  
[builtin function defined in builtins/basic.kt:1765:1]

`root-environment`  
builtin function `(root-environment) => root-environment`  
Return the root environment.  
[builtin function defined in builtins/environments.kt:108:1]

`round`  
builtin function `(round number) => number`  
Return `number` rounded to the nearest integer.  
Ties (<integer> + 0.5) are rounded towards an even integer.  
[builtin function defined in builtins/numbers.kt:639:1]

`rplaca`  
builtin function `(rplaca cons new-car) => cons`  
Replace the car of `cons` with `new-car` and return `cons`.  
[builtin function defined in builtins/basic.kt:56:1]

`rplaca-ret-value`  
builtin function `(rplaca-ret-value cons new-car) => new-car`  
Replace the car of `cons` with `new-car` and return `new-car`.  
Intended for use by setf.  
[builtin function defined in builtins/basic.kt:1866:1]

`rplacd`  
builtin function `(rplacd cons new-cdr) => cons`  
Replace the cdr of `cons` with `new-cdr` and return `cons`.  
[builtin function defined in builtins/basic.kt:76:1]

`rplacd-ret-value`  
builtin function `(rplacd-ret-value cons new-cdr) => new-cdr`  
Replace the cdr of `cons` with `new-car` and return `new-cdr`.  
Intended for use by setf.  
[builtin function defined in builtins/basic.kt:1887:1]

`run-hook-function`  
builtin function `(run-hook-function hook-symbol &rest args) => return-value`  
Run the hook function of hook `hook-symbol` and return its value.  
The `args` are passed to the function.  
[builtin function defined in builtins/system.kt:1333:1]

`run-program`  
builtin function `(run-program command &key :in-shell :input (:output t) (:error-output t) :env :raise-error) => exit-status`  
Run an external command and return its exit status. If command  is a  
list of strings, run it directly. Otherwise, if it is a single string:  
  - if &key `in-shell` is t, run command as a shell command line with  
    `/bin/sh`.  
  - if &key `in-shell` is a string, use it as the shell and run the  
    command in it, using the "-c" option like for `/bin/sh`.  
  - if &key `in-shell` is nil (the default), run command with `/bin/sh`  
    if it contains shell meta characters (~"'`|&,;[{(<>)}]*?$).  
    Otherwise, split the string on whitespace and run it directly.  
If &key `input` is a string or a stream, use it as the standard input  
stream of the command. If it is t, the command reads from the normal  
standard input; if it is nil, redirect it from the null device.  
If &key `output` is a stream, use it as the standard output stream  
of the command. You can use `make-string-output-stream` (with  
`get-output-stream-string`) to capture the output of the command in the  
program. The same goes for `error-output` and the standard error output.  
If `output` is t, use the standard output; if it is nil, redirect the  
command's output to the null device. The same goes for `error-output`.  
If &key `env` (a table) is non-nil, use it as the process environment of  
the command. Otherwise, use the default process environment.  
If &key `raise-error` is true, raise an error if the command returns a  
non-zero exit status.  
[builtin function defined in builtins/system.kt:897:1]

`sassoc`  
builtin function `(sassoc item alist default) => cons/nil`  
Look up `item` in `alist`; return the pair whose car is equal to `item`.  
If `item` is not in `alist`, return `default`, or if it is a function,  
call it with no args and return the result.  
[builtin function defined in builtins/alists.kt:63:1]

`sassq`  
builtin function `(sassq item alist default) => cons/nil`  
Look up `item` in `alist`; return the pair whose car is eq to `item`.  
If `item` is not in `alist`, return `default`, or if it is a function,  
call it with no args and return the result.  
[builtin function defined in builtins/alists.kt:117:1]

`select-string-from-prefix`  
lambda function `(select-string-from-prefix prefix selections) => value`  
If `prefix` is a prefix of just one of `selections`, return that one.  
Otherwise, return nil if it matches none, a list if it matches more than one.  
[lambda function defined in preload/other.lisp:86]

`seq`  
lambda function `(seq start end &optional step) => value`  
Return a list of numbers from START to END.  
Optional STEP (default 1 or -1) specifies the step-width.  
[lambda function defined in preload/numeric.lisp:59]

`sequencep`  
builtin function `(sequencep object) => t/nil`  
Return t if `object` is a sequence, else nil.  
Lists, strings, and vectors are sequences.  
[builtin function defined in builtins/basic.kt:1130:1]

`set`  
builtin function `(set value symbol) => value`  
Assign `value` to the variable `symbol`; return the new value.  
[builtin function defined in builtins/basic.kt:172:1]

`set-debug`  
builtin function `(set-debug &rest symbols) => symbol-list`  
Activate and deactivate debug topics (symbols), items/areas to be debugged.  
Topics are activated by using their name as argument, or deactivated with  
`-name`. To deactivate all, use `=off`. To show what topics are available,  
use `=list`.  
Return the active debug topics (a list of symbols) or all with `list`.  
[builtin function defined in builtins/system.kt:29:1]

`set-hook-function`  
builtin function `(set-hook-function hook-symbol function) => nil`  
Associate a `function` with the `hook-symbol`, to be called  
when the hook is activated. If `function` is nil, nothing will  
be called when the hook is activated.  
[builtin function defined in builtins/system.kt:1289:1]

`setelt`  
builtin function `(setelt sequence index value) => element`  
Set element `index` of sequence `sequence` to `value`; return `value`.  
The index is zero-based. It is an error if `index` is negative or  
greater than the length of the sequence.  
[builtin function defined in builtins/sequences.kt:43:1]

`setf`  
macro `(setf place value)`  
Set field at PLACE to VALUE.  
[macro defined in preload/30-places.lisp:104]

`setq`  
special form `(setq &rest symbol-value-settings) => new-value`  
Assign the value of `expr` to (un-evaluated) `symbol` and return the value.  
Multiple settings like `(setq var1 form1 var2 form2 ...)` are possible.  
First form1 is evaluated and the result is stored in the variable var1,  
then form2 is evaluated and the result stored in var2, and so forth.  
[special form defined in builtins/basic.kt:213:1]

`signum`  
builtin function `(signum number) => sign`  
Return -1 / 0 / 1 if NUMBER is negative / zero / positive, respectively.  
[builtin function defined in builtins/numbers.kt:755:1]

`sin`  
builtin function `(sin radians) => result`  
Return the sine of `radians`  
[builtin function defined in builtins/numbers.kt:1288:1]

`sinh`  
builtin function `(sinh radians) => result`  
Return the hyperbolic sine of `radians`  
[builtin function defined in builtins/numbers.kt:1234:1]

`sleep`  
builtin function `(sleep seconds) => nil`  
Suspend execution for approximately the number of `seconds` and return.  
[builtin function defined in builtins/system.kt:1081:1]

`sort`  
lambda function `(sort seq &optional (pred (function <))) => value`  
Sort sequence `seq` with predicate `pred` and return the result.  
[lambda function defined in preload/sequences.lisp:14]

`sqrt`  
builtin function `(sqrt number) => number`  
Return the sqare root of `number`.  
[builtin function defined in builtins/numbers.kt:718:1]

`stream`  
builtin function `(stream object) => stream`  
Return an input stream from `object`.  
If `object` is a stream, return it. Otherwise take the string  
representation of `object` and make a stream from that.  
[builtin function defined in builtins/io.kt:245:1]

`streamp`  
builtin function `(streamp object) => t/nil`  
Return t iff `object` is a stream, nil else.  
[builtin function defined in builtins/io.kt:595:1]

`string`  
builtin function `(string &rest items) => string`  
Make a string from all arguments and return it.  
[builtin function defined in builtins/strings.kt:20:1]

`string-capitalize`  
builtin function `(string-capitalize string &optional start end) => string`  
Return `string` with all lowercase chars replaced by uppercase chars.  
`start` and `end` may specify the region of the string to be treated;  
defaults are 0 and the end of the string.  
[builtin function defined in builtins/strings.kt:387:1]

`string-concat`  
builtin function `(string &rest items) => string`  
Make a string from all arguments and return it.  
[builtin function defined in builtins/strings.kt:20:1]

`string-contains-p`  
builtin function `(string-contains-p haystack needle) => t/nil`  
Return t if string `haystack` contains `needle`.  
[builtin function defined in builtins/strings.kt:583:1]

`string-downcase`  
builtin function `(string-downcase string &optional start end) => string`  
Return `string` with all lowercase chars replaced by uppercase chars.  
`start` and `end` may specify the region of the string to be treated;  
defaults are 0 and the end of the string.  
[builtin function defined in builtins/strings.kt:325:1]

`string-ends-with`  
builtin function `(string-ends-with string prefix) => t/nil`  
Return true iff `string` ends with `prefix`.  
[builtin function defined in builtins/strings.kt:621:1]

`string-left-trim`  
builtin function `(string-left-trim char-bag string) => trimmed-string`  
Return a substring of `string`, with characters in `char-bag` stripped  
off the beginning. `char-bag` may be t, in which case all whitespace  
characters will be stripped, or a sequence of characters.  
[builtin function defined in builtins/strings.kt:492:1]

`string-reader-stream-p`  
builtin function `(string-reader-stream-p object) => t/nil`  
Return t iff `object` is a string reader stream, nil else.  
[builtin function defined in builtins/io.kt:521:1]

`string-right-trim`  
builtin function `(string-right-trim char-bag string) => trimmed-string`  
Return a substring of `string`, with characters in `char-bag` stripped  
off the end. `char-bag` may be t, in which case all whitespace characters  
will be stripped, or a sequence of characters.  
[builtin function defined in builtins/strings.kt:521:1]

`string-split`  
builtin function `(string-split string &optional separator limit keep-empty) => string-list`  
Split `string` into parts separated by `separator`; return them as list.  
If `separator` is a regexp object, a regexp match is done instead of a  
string match. If it is nil or unspecified, it is assumed to be  
whitespace. if `limit` is non-nil and positive, it is the maximum  
number of parts into which the string is split.  
If separator and keep-empty are both nil, don't keep empty parts at  
the beginning or the end of the list.  
[builtin function defined in builtins/strings.kt:231:1]

`string-starts-with`  
builtin function `(string-starts-with string prefix) => t/nil`  
Return true iff `string` starts with `prefix`.  
[builtin function defined in builtins/strings.kt:602:1]

`string-trim`  
builtin function `(string-trim char-bag string) => trimmed-string`  
Return a substring of `string`, with characters in `char-bag` stripped  
off the beginning and end. `char-bag` may be t, in which case all  
whitespace characters will be stripped, or a sequence of characters.  
[builtin function defined in builtins/strings.kt:460:1]

`string-upcase`  
builtin function `(string-upcase string &optional start end) => string`  
Return `string` with all lowercase chars replaced by uppercase chars.  
`start` and `end` may specify the region of the string to be treated;  
defaults are 0 and the end of the string.  
[builtin function defined in builtins/strings.kt:287:1]

`string-writer-stream-p`  
builtin function `(string-writer-stream-p object) => t/nil`  
Return t iff `object` is a string writer stream, nil else.  
[builtin function defined in builtins/io.kt:540:1]

`stringp`  
builtin function `(stringp object) => t/nil`  
Return t is `object` is a string, else nil.  
[builtin function defined in builtins/basic.kt:984:1]

`subseq`  
builtin function `(subseq sequence start &optional end) => subsequence`  
Return a copy of the subsequence of `sequence` from `start` to `end`.  
If `end` is omitted, the end of the sequence is assumed.  
[builtin function defined in builtins/sequences.kt:146:1]

`substring`  
builtin function `(substring string start &optional end) => string`  
Return a substring of `string`, bounded by indices `start` and `end`  
(or the end of the original string).  
[builtin function defined in builtins/strings.kt:550:1]

`symbol-function`  
builtin function `(symbol-function symbol) => symbol`  
Return the function bound to `symbol`.  
It is an error if there is no function bound to `symbol`.  
[builtin function defined in builtins/basic.kt:650:1]

`symbol-name`  
builtin function `(symbol-name symbol) => string`  
Return the name of `symbol` as a string.  
[builtin function defined in builtins/basic.kt:1619:1]

`symbol-value`  
builtin function `(symbol-value symbol) => value`  
Return the value of `symbol`.  
[builtin function defined in builtins/system.kt:728:1]

`symbolp`  
builtin function `(symbolp object) => t/nil`  
Return t if `object` is a symbol, else nil.  
[builtin function defined in builtins/basic.kt:1056:1]

`symlink-p`  
builtin function `(symlink-p pathspec) => t/nil`  
Return t if the file denoted by `pathspec` is a plain file, nil else.  
[builtin function defined in builtins/files.kt:358:1]

`system-perfdata`  
builtin function `(system-perfdata) => result`  
Return the overall system performance data as an alist.  
The items in the returned list are, in this order:  
  `call`: the number of function calls  
  `cons`: the number of conses created  
  `eval`: the number of evalulations done  
  `secs`: the number of seconds elapsed since the start of lyk  
[builtin function defined in builtins/system.kt:1223:1]

`system-running-time`  
builtin function `(system-running-time) => milliseconds`  
Return the number of milliseconds since the lyk system started.  
[builtin function defined in builtins/system.kt:1182:1]

`system-started-time`  
builtin function `(system-started-time) => milliseconds`  
Return the number of milliseconds since the lyk system started.  
[builtin function defined in builtins/system.kt:1200:1]

`system-time`  
builtin function `(system-time) => milliseconds`  
Return the current system time in milliseconds.  
This is the number of milliseconds since the beginning of the Unix  
epoch, in theory. It may not be a good source for determining the  
current calendar time.  
[builtin function defined in builtins/system.kt:1164:1]

`table-count`  
builtin function `(table-count table) => count`  
Return the number of key-value pairs in `table`.  
[builtin function defined in builtins/tables.kt:78:1]

`table-exists`  
builtin function `(table-exists table key) => t/nil`  
Return t if `key` exists in `table`, nil else.  
[builtin function defined in builtins/tables.kt:97:1]

`table-get`  
builtin function `(table-get table key &optional default) => value`  
Return value associated in `table` with `key`.  
If `key` is not present, return `default` (which defaults to nil).  
[builtin function defined in builtins/tables.kt:18:1]

`table-inc`  
builtin function `(table-inc table key &optional (increment 1) &key :create (:initial 0)) => value`  
Increment (and return) the numeric value for `key` in `table` by `increment`.  
If keyword argument `create` is non-nil and `key` does not exist in table,  
create the key with the value `initial` before incrementing. Otherwise, it  
is an error if `key` does not exists in `table`.  
[builtin function defined in builtins/tables.kt:122:1]

`table-keys`  
builtin function `(table-keys table) => keys`  
Return a list of all keys in `table`.  
[builtin function defined in builtins/tables.kt:159:1]

`table-pairs`  
builtin function `(table-pairs table) => pairs`  
Return a list with all (key . value) pairs in `table`.  
[builtin function defined in builtins/tables.kt:196:1]

`table-put`  
builtin function `(table-put table key value) => value`  
Make `table` associate `key` with `value`, return `value`.  
[builtin function defined in builtins/tables.kt:38:1]

`table-remove`  
builtin function `(table-remove table key) => table`  
Remove `key` from `table` and return `table`.  
[builtin function defined in builtins/tables.kt:215:1]

`table-values`  
builtin function `(table-values table) => values`  
Return a list with all values in `table`.  
[builtin function defined in builtins/tables.kt:178:1]

`tablep`  
builtin function `(tablep object) => t/nil`  
Return t is `object` is a table, else nil.  
[builtin function defined in builtins/basic.kt:1074:1]

`tan`  
builtin function `(tan radians) => result`  
Return the tangent of `radians`  
[builtin function defined in builtins/numbers.kt:1252:1]

`tanh`  
builtin function `(tanh radians) => result`  
Return the hyperbolic tangent of `radians`  
[builtin function defined in builtins/numbers.kt:1198:1]

`terpri`  
builtin function `(terpri &optional output-stream) => nil`  
Terminate a print line by sending a newline to the output-stream  
(or standard output).  
[builtin function defined in builtins/io.kt:168:1]

`the-environment`  
builtin function `(the-environment) => environment`  
Return the current environment.  
[builtin function defined in builtins/environments.kt:64:1]

`throw`  
builtin function `(throw tag value) => no-return`  
Cause a non-local control transfer to the nearest enclosing catch  
whose tag is eq to `tag`. The value returned by that catch is `value`.  
[builtin function defined in builtins/basic.kt:770:1]

`truncate`  
builtin function `(truncate number) => result`  
Return `number` truncated towards zero.  
[builtin function defined in builtins/numbers.kt:1306:1]

`type-of`  
builtin function `(type-of object) => symbol`  
Return the type of `object` as a symbol.  
[builtin function defined in builtins/basic.kt:1225:1]

`unless`  
macro `(unless condition &rest else-clauses)`  
If `condition` evaluates to nil, eval all `else-clauses` and return  
the value of the last. Otherwise, return nil.  
[macro defined in preload/20-macros.lisp:2]

`unquote`  
special form `(unquote form) => no-return`  
Throw an error if called as a function outside of a quasiquote.  
[special form defined in builtins/macros.kt:143:1]

`unquote-splicing`  
builtin function `(unquote-splicing form) => no-return`  
Throw an error if called as a function outside of a quasiquote.  
[builtin function defined in builtins/macros.kt:161:1]

`until`  
special form `(until condition &rest bodyforms) => value`  
If `condition` evaluates nil, evaluate `bodyforms`; repeat.  
[special form defined in builtins/basic.kt:1286:1]

`unwind-protect`  
special form `(unwind-protect bodyform &rest cleanupforms) => value`  
Eval `bodyform`, and even in case of an error or throw, eval `cleanupforms`.  
If `bodyform` completes normally, return its value after executing the  
`cleanupforms`.  
[special form defined in builtins/basic.kt:1311:1]

`user-homedir-pathname`  
builtin function `(user-homedir-pathname) => pathname`  
Return the pathname of the user's home directory.  
[builtin function defined in builtins/files.kt:244:1]

`user-name`  
builtin function `(user-name) => string`  
Return the username of the current user.  
[builtin function defined in builtins/system.kt:1122:1]

`vector`  
builtin function `(vector &rest elements) => vector`  
[builtin function defined in builtins/vectors.kt:101:1]

`vector-get`  
builtin function `(vector-get vector index) => element`  
Return the element of `vector` at `index`.  
[builtin function defined in builtins/vectors.kt:29:1]

`vector-set`  
builtin function `(vector-set vector index value) => value`  
Set the element of `vector` at `index` to `value`; return `value`.  
[builtin function defined in builtins/vectors.kt:80:1]

`vectorp`  
builtin function `(vectorp object) => t/nil`  
Return t is `object` is a vector, else nil.  
[builtin function defined in builtins/basic.kt:1092:1]

`warning`  
builtin function `(warning message-format &rest format-args) => nil`  
Raise a warning with `message-format` and optional `format-args`.  
If warnings are treated as errors (i.e. *warnings-as-errors* is true),  
the warning exits all active calls immediately, except for errset.  
Otherwise, only the message is printed as a warning, formatted as  
specified.  
For the warning message, the `format-args` will be formatted using  
`message-format` as a format string.  
[builtin function defined in builtins/basic.kt:703:1]

`warnings-as-errors`  
builtin function `(warnings-as-errors &optional on) => t/nil`  
Return t if warnings as handled as errors, nil otherwise.  
With optional `on` argument, set warnings to be handled as errors iff  
`on` is true. In this case, return the status from *before* setting it.  
[builtin function defined in builtins/system.kt:830:1]

`when`  
macro `(when condition &rest then-clauses)`  
If `condition` evaluates to non-nil, eval all `then-clauses` and return  
the value of the last. Otherwise return nil.  
[macro defined in preload/20-macros.lisp:9]

`while`  
special form `(while condition &rest bodyforms) => value`  
If `condition` evaluates non-nil, evaluate `bodyforms`; repeat.  
[special form defined in builtins/basic.kt:1263:1]

`with-environment`  
special form `(with-environment env &rest bodyforms) => value`  
Eval `bodyforms` in environment `env` and return the last value.  
[special form defined in builtins/environments.kt:82:1]

`with-gensyms`  
macro `(with-gensyms syms &rest body)`  
Run the BODY with the symbols in SYMS (a list) bound to gensyms.  
This is meant to simplify macro definitions that would otherwise  
use a  
  (let ((param1 (gensym))  
        (param2 (gensym)))  
        ... )  
    ,@body)  
symbol definition chain explicitly.  
[macro defined in preload/40-macros-using-places.lisp:16]

`with-input-from-string`  
macro `(with-input-from-string control &rest bodyforms)`  
(with-input-from-string (variable string-value) bodyforms ...)  
Bind variable to a stream make from string-value and evaluate `bodyforms`.  
[macro defined in preload/20-macros.lisp:151]

`with-lines-from-file`  
macro `(with-lines-from-file declarations &rest bodyforms)`  
(with-lines-from-file (line-symbol pathname &optional chomp) body ...)  
Evaluate BODYFORMS with LINE-SYMBOL bound to the lines of file PATHNAME  
in sequence. On failure to open the file, an error is raised.  
Instead of a pathname, an already open input port can be used as well,  
but that port will be closed afterwards.  
If &optional CHOMP is non-nil, the newline characters are removed from  
the lines.  
[macro defined in preload/20-macros.lisp:100]

`with-open-file`  
macro `(with-open-file file-declaration &rest bodyforms)`  
(with-open-file (stream-symbol pathname &rest options) BODY...)  
Evaluate BODYFORMS with STREAM-SYMBOL bound to a port open to PATHNAME.  
Options: :direction followed by :input (default) or :output or :io,  
         :if-exists followed by :supersede (default) or :new-version  
                             or :append or :overwrite or :error or nil  
         :if-no-exists followed by :error (default) or :create or nil  
[macro defined in preload/20-macros.lisp:81]

`with-output-to-string`  
macro `(with-output-to-string control &rest bodyforms)`  
(with-output-to-string (variable) bodyforms ...)  
Bind variable to an output stream and evaluate `bodyforms`;  
return the content written to the output stream as a string.  
[macro defined in preload/20-macros.lisp:158]

`write-line`  
builtin function `(write-line string &optional output-stream &key :start :end) => string`  
Write the `string` to `output-stream`, then output a newline afterwards.  
Keyword parameters `start` and `end`, if specified, denote the start  
and end positions of the portion of `string` being written.  
Return `string`.  
[builtin function defined in builtins/io.kt:810:1]

`write-string`  
builtin function `(write-string string &optional output-stream) => string`  
Write the `string` to `output-stream`.  
Keyword parameters `start` and `end`, if specified, denote the start  
and end positions of the portion of `string` being written.  
Return `string`.  
[builtin function defined in builtins/io.kt:789:1]

`zerop`  
builtin function `(zerop number) => t/nil`  
Return t if number is zero, nil otherwise  
[builtin function defined in builtins/numbers.kt:121:1]

`λ`  
special form `(lambda lambda-list &rest bodyforms) => function`  
Return an anonymous function with `lambda-list` and `bodyforms`.  
[special form defined in builtins/basic.kt:511:1]

