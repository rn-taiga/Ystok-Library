;;; -*- Mode: Lisp; -*-
;;; Ystok-Library - System definition for LispWorks
;;; Copyright (c) 2003-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;(pushnew :ys-product *features*)
;(pushnew :debug *features*)
;(removef *features* :debug) 
;(pushnew :Russian *features*)
;(yl:debug-on :yl)

;#+(or lispworks sbcl) (load #P"PROJECTS:ylib;ffc;defsys")
;#+(or lispworks sbcl) (lw:compile-system 'YSTOK-FFC :load t)

(defsystem YSTOK-LIBRARY (:object-pathname (lw:current-pathname
	#+(not (or German French Russian debug))		"bin/"
	#+(and (not German) (not French) (not Russian) debug)	"bin/debug/"
	#+(and German (not debug))				"bin/de/"
	#+(and German debug)					"bin/debug/de/"
	#+(and French (not debug))				"bin/fr/"
	#+(and French debug)					"bin/debug/fr/"
	#+(and Russian (not debug))				"bin/ru/"
	#+(and Russian debug)					"bin/debug/ru/"))
 :members
 (#+ystok-ffc (YSTOK-FFC :type :system)
  "package"
  "macros"
  "debug"
  "functions" 
  "octets" 
  ;("win32-lw"	  	:features :win32)
  ;("lw-compat"		:features (not :lispworks))
  ("yl-ffc"	:features (and :ystok-ffc :win32))	; for now, only WinAPI OS bindings
  ("ffc-compat"	:features (not (and :ystok-ffc :win32)))
  "locale" 
  ("lang/de"	:features :German)
  ("lang/fr"	:features :French)
  ("lang/ru"	:features :Russian)
  "meta-parse"
  "meta-parse-number"
  "file-utils"
  ("ys-product"	:features :ys-product)
  ("ys-stubs"	:features (not :ys-product))
  ("systema"	:features :ys-product))
 :rules
 ((:in-order-to :compile :all (:caused-by (:compile "package"))
   (:requires (:load "package")))
  (:in-order-to :load :all
   (:requires (:load "package")))

  (:in-order-to :compile ("debug" "functions" "octets")
   (:caused-by (:compile "macros"))
   (:requires (:load "macros")))
  (:in-order-to :load ("debug" "functions" "octets")
   (:requires (:load "macros")))

  (:in-order-to :compile ("octets" "locale" "file-utils")
   (:requires (:load "functions")))
  (:in-order-to :load ("octets" "locale" "file-utils")
   (:requires (:load "functions")))
  
  (:in-order-to :compile "yl-ffc"
   (:caused-by (:compile #+ystok-ffc YSTOK-FFC))
   (:requires (:load #+ystok-ffc YSTOK-FFC "locale")))
  (:in-order-to :load "yl-ffc"
   (:requires (:load #+ystok-ffc YSTOK-FFC "locale")))

  (:in-order-to :compile "ffc-compat"
   (:requires (:load "locale")))
  (:in-order-to :load "ffc-compat"
   (:requires (:load "locale")))

  (:in-order-to :compile ("de" "fr" "ru")
   (:requires (:load "locale")))
  (:in-order-to :load ("de" "fr" "ru")
   (:caused-by (:load "locale"))
   (:requires (:load "locale")))

  (:in-order-to :compile "meta-parse-number"
   (:caused-by (:compile "locale" "meta-parse" "de" "fr" "ru"))
   (:requires (:load "locale" "meta-parse")))
  (:in-order-to :load "meta-parse-number"
   (:requires (:load "locale" "meta-parse" "de" "fr" "ru")))

  (:in-order-to :compile ("ys-product" "ys-stubs")
   (:caused-by (:compile "functions" "octets"))		; expand inline definitions
   (:requires (:load "debug" "locale" "octets")))
  (:in-order-to :load ("ys-product" "ys-stubs")
   (:caused-by (:load "package"))			; do not loose additional exports
   (:requires (:load "debug" "locale" "octets")))

  (:in-order-to :compile "systema"
   (:caused-by (:compile "locale"))			;"ys-product"
   (:requires (:load "ys-product")))
  (:in-order-to :load "systema"
   (:caused-by (:load "locale"))			; in-bundle macro
   (:requires (:load "ys-product")))
))

;(ys:defmodule :ylib (1 0 :abbrev "lib")) ; <- set in systema.lisp
;(lw:compile-system 'YSTOK-LIBRARY :load t)
