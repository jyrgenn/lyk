;;; lyk doc

Table Literals
--------------

The syntax of table literals is #:(<conses>), where <conses> is one
or more cons cells. Of each cons, the car is the key and the cdr is
the value of a table entry. If there is more than one cons with the
same key, all but one of them are silently ignored. Examples:

    #:()
    #:((#\4 . 52)(#\@ . 64)(#\[ . 91))
    #:((3 . "119")(a . b)("shoo" . 13.4))

The print syntax is like that, only where the cdr of of a cell is
nil, it is printed as <dot> nil for clarity, like this:

    #:((3 . "three")(4 . "four")(gagh . nil))


Functions
---------

tablep
    builtin function (tablep object) => t/nil
    Return t is `object` is a table, else nil.

table-get
    builtin function (table-get table key &optional default) => value
    Return value associated in `table` with `key`.
    If `key` is not present, return `default` (which defaults to nil).

table-put
    builtin function (table-put table key value) => value
    Make `table` associate `key` with `value`, return `value`.

make-table
    builtin function (make-table &rest pairs) => table
    Return a new table, optionally filled with `pairs`.
    Every argument that is not a pair will be used as (arg . nil).

table-count
    builtin function (table-count table) => count
    Return the number of key-value pairs in `table`.

table-exists
    builtin function (table-exists table key) => t/nil
    Return t if `key` exists in `table`, nil else.

table-inc
    builtin function (table-inc table key &optional (increment 1) &key
                      :create (:initial 0)) => value
    Increment (and return) the numeric value for `key` in `table` by
    `increment`.
    If keyword argument `create` is non-nil and `key` does not exist in
    table, create the key with the value `initial` before incrementing.
    Otherwise, it is an error if `key` does not exists in `table`.

table-keys
    builtin function (table-keys table) => keys
    Return a list of all keys in `table`.

table-values
    builtin function (table-values table) => values
    Return a list with all values in `table`.

table-pairs
    builtin function (table-pairs table) => pairs
    Return a list with all (key . value) pairs in `table`.

table-remove
    builtin function (table-remove table key) => table
    Remove `key` from `table` and return `table`.

deep-copy-table
    lambda function (deep-copy-table tbl) => value
    Return a deep copy of table TBL.
    See the documentation of function #'copy for details.

