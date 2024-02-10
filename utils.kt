
fun interface LocationHolder {
    fun location(): String
}


class ListCollector {
    var head: Object = Nil
    var last: Cons? = null

    fun append(arg: Object) {
        val newpair = Cons(arg, Nil)
        if (last != null) {
            last.rplacd(newpair)
        } else {
            head = newpair
        }
        last = newpair
    }

    fun lastcdr(arg: LispObject) {
        if (last is Pair) {
            last.rplacd(arg)
        } else {
            last = arg
            head = arg
        }
    }

    fun list() = head
}
