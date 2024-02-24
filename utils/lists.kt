
package org.w21.lyk

fun collectedList(closure: (lc: ListCollector) -> Unit): LispObject {
    val lc = ListCollector()
    closure(lc)
    return lc.list()
}
