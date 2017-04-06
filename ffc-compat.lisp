;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10;  -*-
;;; Ystok Library - Ystok-FFC bindings
;;; Copyright (c) 2003-2014 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ystok.library)

(declaim (inline locale-char-downcase locale-char-upcase get-file-title)
         (ftype (function (character) character) locale-char-downcase locale-char-upcase))

(defun locale-char-downcase (char)
  (char-downcase char))

(defun locale-char-upcase (char)
  (char-upcase char))

;;; Args: arg        String (simple or not), character, or symbol (including NIL).
;;;       start, end Delimit the subsring extracted from arg
           
(defun locale-string-downcase (arg &key (start 0) end)
  (declare (fixnum start))
  (string-downcase arg :start start :end end))

(defun locale-string-upcase (arg &key (start 0) end)
  (declare (fixnum start))
  (string-upcase arg :start start :end end))

(defun locale-string-capitalize (arg &key (start 0) end)
  (declare (fixnum start))
  (string-capitalize arg :start start :end end))

;;; FILE-UTILS

(defun get-file-title (pathname)
  (file-namestring pathname))
