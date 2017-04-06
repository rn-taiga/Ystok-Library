;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-
;;; Ystok-Library - Package definition
;;; Copyright (c) 2003-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage :ystok.library
  (:nicknames :yl)
  (:use :common-lisp)
  (:documentation "The library of common use")
  #+lispworks
  (:import-from :lw
   lw:appendf
   lw:locale-char-equal
   lw:locale-string-equal lw:locale-string-greaterp lw:locale-string-lessp
   lw:current-pathname lw:pathname-location
   lw:nconcf
   lw:push-end
   lw:rebinding
   ;lw:removef					; LispWorks's macroexpansion look odd
   lw:string-append
   lw:when-let lw:when-let*
   lw:whitespace-char-p
   lw::without-properties)
  #+lispworks
  (:import-from :system
   system:cdr-assoc
   system::copy-file
   system:directory-pathname-p
   system:directory-pathname
   system::remove-properties)
  #+lispworks
  (:import-from  #+lispworks4 :package #-lispworks4 :system
   #+lispworks4 package::set-equal  #-lispworks4 sys::set-equal)
  ;#+(and lispworks win32)
  ;(:import-from :win32 win32:WORD win32:DWORD)
  #+sbcl
  (:import-from :sb-ext
   sb-ext:octets-to-string
   sb-ext:string-to-octets)
  (:export
   ;; General constants, types, macros, and functions
   #:+all-string+
   #:+em-dash+
   #:+en-dash+
   #:+nil-circle+
   #:+none-string+
   #:+null-string+
   #:*kwd-package*
   #:*unbound-value*

   #:allf
   #:appendf					; lw-compat
   #:defconstant*
   #:deletef
   #:export-toplevel
   #:if-bind
   #:in-syntax
   #:iff
   #:imp
   #:neq
   #:nilf
   #:nconcf
   #:orf
   #:push-end					; lw-compat
   #:removef					; superseeds lw's
   #:toggle
   #:when-bind
   #:while
   #:with-dspec
   #:with-gensyms
   #:with-package
   #:xor
   
   ;; Debug, help, and logging stuff
   #:*log-options*
   #:*log-stream*
   #:debug-format
   #:debug-off
   #:debug-on
   #:define-debug-option
   #:help-key
   #:helped-condition
   #:hcerror
   #:herror
   #:hwarn
   #:logg
   #:with-restarts

   ;; Locale
   #:~$
   #:$-format-to-string
   #:*century-digits*
   #:*currency-thousand-separator*
   #:*currency-digits*
   #:*currency-symbol*
   #:*currency-order*
   #:*date-separator*
   #:*date-format*
   #:*default-char-type*
   #:*decimal-separator*
   #:decline-word
   #:define-label-language
   #:defnative
   #:e-format-to-string
   #:~e
   #:ef-type
   #:f-format-to-string
   #:~f
   #:genderize-word
   #:gettext
   #:in-bundle
   ;#:*internal-format*
   #:*language*
   #:labels-pathname
   #:language-primary-list
   #:language-primary-subtag
   #:*language-subtag-map*
   #:load-labels-file
   #:locale-char-equal
   #:locale-char-downcase #:locale-char-upcase
   #:locale-string-capitalize #:locale-string-downcase #:locale-string-upcase
   #:locale-string-equal #:locale-string-greaterp #:locale-string-lessp	; lw-compat
   #:make-bundle
   #:*month-abbrevs*
   #:*month-names*
   #:ordinalize-tail
   #:parse-number
   #:*time-separator*
   #:*time-format*
   #:transliterate-char
   #:transliterate-string
   #:unload-labels
   #:*warn-on-missing-localization*
   #:*weekday-names*
   #:*weekday-abbrevs*

   ;; Misc.utils, octets
   #:assoq
   #:cdr-assoq
   #:cdr-assoq3
   #+(or lispworks allegro sbcl) class-all-subclasses
   #:char-position
   #:copy-object
   #:clone-object
   #:compose
   #:crc-8
   #:dlookup #:dmax #:dmin #:dsum
   #:fand
   #:first-or-self
   #:flatten
   #:hex-digit-char ;code-hex-digit
   #:hex-digit-char-p
   #:hex-string-to-octets
   #:insert
   #:integer-to-octets
   #:interleave-seq
   #:iplusp
   #:listify
   ;#:lrc
   #:make-octet-vector
   #:mappend
   #:memq
   #:put					; lw-compat
   #:octet
   #:octet-and
   #:octet-vector
   #:octet-vector-p
   #:octets-to-signed
   #:octets-to-unsigned
   #:octets-to-string
   #:object-hex-string
   #:pad-octets
   #:random-elt
   #:retain-properties
   #:sans
   #:shrink-vector
   #:simple-octet-vector
   #+lispworks split
   #:split-seq #:split-seq-if
   #:string-append*				; both functions.lisp and lw-compat
   #:string-to-octets
   #:timestamp-string
   #:user-name
   #:xmemq
   #:xtypep

   ;; Tiny evaluator
   #:*tiny-environment*
   #:*tiny-symbol-value-function*
   #:*tiny-setq-function*
   #:tiny-eval
   
   ;; File and OS utils
   #:*temp-pathnames*
   ;#:absolute-pathname-p
   #:copy-file
   #:copy-stream
   #:current-pathname
   #:define-file-format
   #:directory-pathname
   #:directory-pathname-p
   #:enough-pathname
   #:file-format-flags
   #:file-type-format
   #:get-file-title
   #:get-folder
   #:getenv
   #:intern-upcase
   #:load-list
   #:pathname-equal
   #:pathname-location
   #:safe-namestring

   ;; Other LW compatibility
   #:cdr-assoc
   #:rebinding
   #:remove-property
   #:remove-properties
   #:set-equal
   #:string-append
   #:when-let #:when-let*
   #:whitespace-char-p
   #:without-properties))
