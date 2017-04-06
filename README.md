# Ystok-Library

<b><i>Portable Common Lisp library of general purpose macros, functions, and utilities</i></b>

Internationalization and localization
-------------------------------------

Ystok-Library includes the <code>defnative</code> macro for customizing national language strings. Such strings are compiled into application when delivering in a specific language.

The lang/ subdirectory contains language files for German and Russian.

For run-time string translation, Ystok-Library includes the following:
* language primary tag/subtag relationship,
* module bundle structures,
* label files for each bundle and language,
* <code>gettext</code> function.

File utilities
--------------

The file-utils.lisp contains some useful functions:
* pathname conversion and <code>pathname-equal</code>,
* <code>copy-file</code>.

Octets utilities
--------------

* Types <code>octet</code>, <code>octet-vector</code>, and <code>simple-octet-vector</code>.
* Functions <code>octets-to-(un)signed</code> and <code>integer-to-octets</code>.
* Functions <code>octets-to-string</code> and <code>string-to-octets</code>, which are not built in every Common Lisp implementation.
* Reading and printing octet vectors.

META
----

The library includes the portable code of META, a classic technique for building recursive descent parsers, which is both simple and effective. See "Pragmatic Parsing in Common Lisp" of Henry G. Baker at http://citeseer.ist.psu.edu/baker91pragmatic.html or inside http://portal.acm.org/  or just download Prag-Parse.ps, the original article in PostScript.

The meta-parse-number.lisp file in the distribution bundle presents an example of using META. This approach is used by
* [Ystok-URI](http://lisp.ystok.ru/yuri/) for parsing URI strings,
* [Ystok-Local-Time](http://lisp.ystok.ru/ylocal-time/) for parsing ISO8601 and local format of date/time and intervals.

LispWorks compatibility code
----------------------------

The lw-compat.lisp file contains reimplementation of useful primitives from [LispWorks](http://lisp.ystok.ru/links.html#LispWorks) <code>system</code> and <code>lispworks</code> packages.
* Macros <code>appendf</code>, <code>nconcf</code>, <code>push-end</code>, <code>when-let</code>, <code>when-let*</code>.
* Functions <code>cdr-assoc</code>, <code>remove-properties</code>, <code>string-append</code>, and the like.

Dependencies
------------

Ystok-Library optionally depends on [Ystok-FFC](https://github.com/RN-S1/Ystok-FFC).

Platforms
---------

The source code was tested on the following Lisp implementations:
* [LispWorks](http://www.lispworks.com/) 4.4, 5.0, and 6.1 for Windows,
* [SBCL](http://www.sbcl.org/) 1.0.55 for Windows.


The Ystok-Library contains both the ASDlite/ASDF-based ycard.asd and the LispWorks-based defsys.lisp system definition files.


<i>forked from</i><br>
<i>[http://ystok.ru/](http://lisp.ystok.ru/ylib/)</i>
