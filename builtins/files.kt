// file and path operations

package org.w21.lyk

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths


/// builtin basename
/// fun     bi_basename
/// std     pathspec
/// key     
/// opt     
/// rest    
/// ret     file-basename
/// special no
/// doc {
/// Return the basename of a pathname, meaning without the directory part(s).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_basename(args: LObject, kwArgs: Map<LSymbol, LObject>,
                suppp: Map<LSymbol, Boolean>): LObject {
    val fname = pathname_from(arg1(args))
    return makeString(basename(fname))
}

/// builtin dirname
/// fun     bi_dirname
/// std     pathspec
/// key     
/// opt     
/// rest    
/// ret     file-dirname
/// special no
/// doc {
/// Return the dirname of a pathname, meaning only the directory part(s).
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_dirname(args: LObject, kwArgs: Map<LSymbol, LObject>,
               suppp: Map<LSymbol, Boolean>): LObject {
    val fname = pathname_from(arg1(args))
    return makeString(dirname(fname))
}

/// builtin directory
/// fun     bi_directory
/// std     pathspec
/// key     
/// opt     
/// rest    
/// ret     pathnames
/// special no
/// doc {
/// Return a list of pathnames matching `pathspec`.
/// The last path component may contain wildcard characters.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_directory(args: LObject, kwArgs: Map<LSymbol, LObject>,
                 suppp: Map<LSymbol, Boolean>): LObject {
    val pathspec = arg1(args)
    return dirlist(pathspec.toString())
}

/// builtin delete-file
/// fun     bi_delete_file
/// std     pathname
/// key     
/// opt     
/// rest    
/// ret     t
/// special no
/// doc {
/// Delete the file denoted by `pathname` and return t on success.
/// On failure, signal an error.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_delete_file(args: LObject, kwArgs: Map<LSymbol, LObject>,
            suppp: Map<LSymbol, Boolean>): LObject {
    val pathname = stringArg(arg1(args))
    Files.delete(Paths.get(pathname))
    return T
}

// Return a pathname from a String (that is easy) or an open LStream
fun pathname_from(pathspec: LObject): String {
    when (pathspec) {
        is LStream -> {
            if (pathspec.path == null) {
                throw ArgumentError("stream $pathspec is not a file stream")
            }
            return pathspec.path
        }
        is LString -> return pathspec.the_string
        else -> throw TypeError("pathspec is not a stream or a pathname:"
                                + " ${pathspec.type} ${pathspec.desc(null)}")
    }
}

/// builtin file-author
/// fun     bi_file_author
/// std     pathspec
/// key     
/// opt     
/// rest    
/// ret     string
/// special no
/// doc {
/// Return the author of the file `pathname`.
/// The pathspec is either a pathname string or an open stream.

/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_file_author(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
    val pathname = pathname_from(arg1(args))
    return makeString(Files.getOwner(Paths.get(pathname)).toString())
}

/// builtin file-length
/// fun     bi_file_length
/// std     pathspec
/// key     
/// opt     
/// rest    
/// ret     number
/// special no
/// doc {
/// Return the length of the file `pathspec` in bytes.
/// The pathspec is either a pathname string or an open stream.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_file_length(args: LObject, kwArgs: Map<LSymbol, LObject>,
                   suppp: Map<LSymbol, Boolean>): LObject {
    val pathname = pathname_from(arg1(args))
    return makeNumber(Files.size(Paths.get(pathname)))
}

/// builtin namestring
/// fun     bi_namestring
/// std     pathname
/// key     
/// opt     
/// rest    
/// ret     normalized-pathname
/// special no
/// doc {
/// Return `pathspec` as a pathname in normalized form.
/// This means multiple consecutive slashes and /./ are reduced to a single
/// slash, and /../ constructs are resolved.
// /// Other than with expand-file-name, ~ and ~user are not expanded.
// /// The value returned is a string, even if the argument is not.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_namestring(args: LObject, kwArgs: Map<LSymbol, LObject>,
            suppp: Map<LSymbol, Boolean>): LObject {
    return makeString(File(stringArg(arg1(args))).normalize().toString())
}

