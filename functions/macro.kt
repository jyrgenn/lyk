// the dreaded Lisp macro

package org.w21.lyk

class Macro: Lambda {

    override func call(arglist: LispObject): LispObject {
        throw FunctionError("macro $name called before being defined")
    }
}
