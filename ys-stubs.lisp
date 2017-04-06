;;; Ystok Library - Stub functions and macros
;;; Copyright (c) 2003-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ystok.library)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export (mapcar (lambda (name) (intern (string name)))
  '(#:delivery-level
    #:keep-symbol-names
    #:product-pathname
    #:thys-preference))))

(defun delivery-level () nil)

(defun keep-symbol-names (symbols &optional (context t) recurse)
  (declare (ignore context recurse))
  symbols)

(defvar %thys-product-path%)

(defun product-pathname (&optional relative-pathname)
 ;;; Value: Product installation directory
  (if (boundp '%thys-product-path%)
      (merge-pathnames relative-pathname %thys-product-path%)
      (current-pathname relative-pathname)))

(defun thys-preference (symbol)
  (if (boundp symbol) (symbol-value symbol) nil))
(defun (setf thys-preference) (value symbol)
  (set symbol value))
