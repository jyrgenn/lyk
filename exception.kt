
class SyntaxError(val error: String,
                  val lh: LocationHolder): Exception(error)
{
    override fun toString() = "${lh.location()}: $message"
}

class InternalReaderError(val error: String,
                          val reader: Reader): Exception(error) {
    override fun toString() = "$reader: $message"
}
