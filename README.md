# Ystok-Library

<b><i>Portable Common Lisp library of general purpose macros, functions, and utilities</i></b>

Internationalization and localization

Ystok-Library includes the defnative macro for customizing national language strings. Such strings are compiled into application when delivering in a specific language.

The lang/ subdirectory contains language files for German and Russian.

For run-time string translation, Ystok-Library includes the following:

    language primary tag/subtag relationship,
    module bundle structures,
    label files for each bundle and language,
    gettext function.

File utilities

The file-utils.lisp contains some useful functions:

    pathname conversion and pathname-equal,
    copy-file.

Octets utilities

    Types octet, octet-vector, and simple-octet-vector.
    Functions octets-to-(un)signed and integer-to-octets.<
    Functions octets-to-string and string-to-octets, which are not built in every Common Lisp implementation.
    Reading and printing octet vectors.

META

The library includes the portable code of META, a classic technique for building recursive descent parsers, which is both simple and effective. See "Pragmatic Parsing in Common Lisp" of Henry G. Baker at http://citeseer.ist.psu.edu/baker91pragmatic.html or inside http://portal.acm.org/  or just download Prag-Parse.ps, the original article in PostScript.

The meta-parse-number.lisp file in the distribution bundle presents an example of using META. This approach is used by

    Ystok-URI for parsing URI strings,
    Ystok-Local-Time for parsing ISO8601 and local format of date/time and intervals.

LispWorks compatibility code

The lw-compat.lisp file contains reimplementation of useful primitives from LispWorks system and lispworks packages.

    Macros appendf, nconcf, push-end, when-let, when-let*.
    Functions cdr-assoc, remove-properties, string-append, and the like.

Dependencies

Ystok-Library optionally depends on Ystok-FFC.

Ystok-Library is required by YstokCard, YstokGrid, YstokHelp, YstokHTML, YstokSQL, YstokWidgets, Ystok-Local-Time, Ystok-URI, and our version of ACL-Compat lite.
Platforms

The source code was tested on the following Lisp implementations:

    LispWorks 4.4, 5.0, and 6.1 for Windows,
    SBCL 1.0.55 for Windows.

Download and installation

Ystok-Library is available from
http://lisp.ystok.ru/ylib/ylib-1-4-025.zip or
http://lisp.ystok.ru/ylib/ylib-1-4-025.tgz.

The distribution package contains both the ASDlite/ASDF-based ycard.asd and the LispWorks-based defsys.lisp system definition files.
