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

val reTildePathname = Regex("^~([^/]*)(/.*)?$")

fun tildeExpand(part: String): String {
    val sws = StringWriterStream()
    bi_run_program(LCons(makeString("echo " + part), Nil),
                   mapOf(outputKSym to sws, inShellKSym to T),
                   mapOf<LSymbol, Boolean>(
                       outputKSym to true,
                       inShellKSym to true))
    val result = sws.value_and_reset()
    return result.trimEnd('\n')
}

/// builtin expand-file-name
/// fun     bi_expand_file_name
/// std     filename
/// key     
/// opt     default-directory
/// rest    
/// ret     pathname
/// special no
/// doc {
/// Convert `filename` to absolute, and canonicalize it.
/// Second arg `default-directory` is the directory to start with if
/// NAME is relative (does not start with slash or tilde).
/// If DEFAULT-DIRECTORY is nil or missing, the process's current working
/// directory is used.
/// `filename` should be a string that is a valid file name for the
/// underlying filesystem.
/// 
/// File name components that are ‘.’ are removed, and so are file name
/// components followed by ‘..’, along with the ‘..’ itself; note that
/// these simplifications are done without checking the resulting file
/// names in the file system.
/// 
/// Multiple consecutive slashes are collapsed into a single slash.
/// 
/// An initial "~" in `filename` expands to your home directory. 
/// An initial "~USER" in `filename` expands to USER’s home directory.
/// If USER doesn’t exist, "~USER" is not expanded.
/// [This function documentation is copied from GNU Emacs, with a few
/// changes.]
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_expand_file_name(args: LObject, kwArgs: Map<LSymbol, LObject>,
            suppp: Map<LSymbol, Boolean>): LObject {
    val (fname, defdir) = args2(args)
    val filename = stringArg(fname, " filename")
    val defaultdir = if (defdir === Nil) {
        System.getProperty("user.dir")
    } else {
        stringArg(defdir, " default-directory")
    }

    val tildematch = reTildePathname.find(filename)
    if (tildematch != null) {
        var user = tildematch.groups[1]?.value ?: ""
        var path = tildematch.groups[2]?.value ?: ""
        val home = tildeExpand("~" + user)
        if (!home.startsWith("~")) {
            return normalize_path(home + path)
        }
    }
    if (filename.startsWith("/")) {
        return normalize_path(filename)
    }
    return normalize_path(defaultdir + "/" + filename)
}

/// builtin get-working-directory
/// fun     bi_get_working_directory
/// std     
/// key     
/// opt     
/// rest    
/// ret     pathname
/// special no
/// doc {
/// Return the current working directory as a string.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_get_working_directory(args: LObject, kwArgs: Map<LSymbol, LObject>,
                             suppp: Map<LSymbol, Boolean>): LObject {
           return makeString(System.getProperty("user.dir"))
}

/// builtin user-homedir-pathname
/// fun     bi_user_homedir_pathname
/// std     
/// key     
/// opt     
/// rest    
/// ret     pathname
/// special no
/// doc {
/// Return the pathname of the user's home directory.
/// }
/// end builtin
@Suppress("UNUSED_PARAMETER")
fun bi_user_homedir_pathname(args: LObject, kwArgs: Map<LSymbol, LObject>,
                             suppp: Map<LSymbol, Boolean>): LObject {
    return makeString(System.getProperty("user.home"))
}


fun normalize_path(path: String): LString {
    var norm = File(path).normalize().toString()

    // for some reason, normalize() doesn't fully normalize
    if (norm.startsWith("/../")) {
        norm = norm.substring(3)
    }
    if (norm == "/..") {
        norm = "/"
    }
    return makeString(norm)
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
    return normalize_path(stringArg(arg1(args)))
}

