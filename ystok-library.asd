;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-
;;; Ystok-Library - ASDlite/ASDF system definition
;;; Copyright (c) 2003-2014 Dr. Dmitry Ivanov. All rights reserved.

(in-package :cl-user)

(asdf:defsystem ystok-library
  :version "1.4.025"
  :description "Portable Common Lisp library of general purpose macros, functions, and utilities"
  :maintainer "Dmitry Ivanov http://lisp.ystok.ru/"
  :licence "LLGPL"
  #+asdlite #+asdlite
  :output-pathname (asd:current-location "bin/")
  :depends-on (#+ystok-ffc ystok-ffc)
  :components
  ((:file "package")
   (:file "macros" :depends-on ("package"))
   (:file "debug" :depends-on ("macros"))
   (:file "functions" :depends-on ("macros"))
   (:file "octets" :depends-on ("functions"))
   #-lispworks (:file "lw-compat" :depends-on ("package"))
   (:file "locale" :depends-on ("functions"
                                #-lispworks "lw-compat"))
   ;; For now, OS API bindings are Windows only.
   #+(and ystok-ffc win32) (:file "yl-ffc"     :depends-on ("locale"))
   #-(and ystok-ffc win32) (:file "ffc-compat" :depends-on ("locale"))
   (:module "lang" :depends-on ("locale")
    :components
    (#+German  (:file "de")
     #+French  (:file "fr")
     #+Russian (:file "ru")))
   (:file "meta-parse"        :depends-on ("package"))
   (:file "meta-parse-number" :depends-on ("lang" "meta-parse"))
   (:file "file-utils"        :depends-on ("functions"))
   #+ys-product (:file "ys-product" :depends-on ("debug" "locale"))
   #-ys-product (:file "ys-stubs"   :depends-on ("debug" "functions"))
   #+ys-product (:file "systema"    :depends-on ("ys-product"))))

;(load #P"PROJECTS:ylib;ystok-library.asd")
;(asd:operate :load 'ystok-library)
;(asdf:operate 'asdf:load-op 'ystok-library)
