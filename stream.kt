


class Stream: LispObject(
                  val input: Boolean = true,       // is input stream?
                  val output: Boolean = false,     // is output stream?
                  val error: Boolean = false,      // i.e. stderr
                  val name: String? = null, // pathname or *stdin/out/err* or
                                            // network something
                  val path: String? = null, // file pathname
                  val append: Boolean = false,
              ) {
    
}
