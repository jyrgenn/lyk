
import org.w21.lyk.*


fun main() {
    println(";; this is lyk")
    println("> $Nil")
    for (name in listOf("la la la",
                        "123.4",
                        "12",
                        "Jürgen",
                        ".",
                        "",
                        "swoop",
                        "#hoo#",
                        ",döng",
                        ",",
                        "car",
                        "cdr"
         )) {
        val sym = intern(name)
        println("sym \"$name\" = ${sym.description()}")
    }
}
