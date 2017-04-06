;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-
;;; Ystok Library - Meta-parser implementation
;;; Copyright (c) 2003-2008 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Based on:
;;; (1) meta.lisp 1.0.1 (c) 2001 by Jochen Schmidt.
;;; (2) "Pragmatic Parsing in Common Lisp" of Henry G. Baker.
;;;     http://citeseer.ist.psu.edu/baker91pragmatic.html or inside http://portal.acm.org/
;;; Differences:
;;; (1) Changed @(type var) semantic to @(predicate [var]); this makes the parser
;;;	even more pragmatic due to eliminationg the typep calls.
;;;	Special checking of occurence T-predicate is made.
;;; (2) Use decorated %source%, %index%, and %end% external symbols for lexical vars.
;;; (3) Replaced typecase by <typepredicate>-p whereever possible.
;;; (4) List match was fixed for top-level patterns and augmented by dotted patterns.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defpackage :ystok.meta
  (:use :common-lisp)
  (:nicknames :meta)
  (:export
   #:*meta-readtable*
   #:enable-meta-syntax
   #:disable-meta-syntax
   #:%end%
   #:%index%
   #:%source%
   #:match
   #:with-string-meta
   #:with-list-meta
   #:with-stream-meta)))

(in-package :ystok.meta)

;;; Reader

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defstruct (meta (:print-function
                  (lambda (self stream depth
                           &aux (char (meta-char self)) (form (meta-form self)))
               (declare (ignore depth))
               (ecase char
                 ((#\@ #\! #\$) (format stream "~A~A" char form))
                 (#\[ (format stream "[~{~A~^ ~}]" form))
                 (#\{ (format stream "{~{~A~^ ~}}" form))))))
  char
  form)

 #+mcl ;MCL won't compile to a file without this.
 (defmethod make-load-form ((self meta) &optional env)
  (declare (ignore env))
  `(make-meta :char ,(meta-char self) :form ',(meta-form self)) )

 (defun meta-reader (stream char) (make-meta :char char :form (read stream)))

 (defparameter *saved-readtable* nil)
 ;(defparameter *saved-readtable* (copy-readtable))
 (defparameter *meta-readtable* (copy-readtable))	; use current, not clear syntax

 (dolist (char '(#\@ #\$ #\!))
   (set-macro-character char #'meta-reader nil *meta-readtable*))
   
 (set-macro-character #\{
     #'(lambda (stream char)
         (make-meta :char char :form (read-delimited-list #\} stream t)))
     nil *meta-readtable*)

 (set-macro-character #\[
     #'(lambda (stream char)
         (make-meta :char char :form (read-delimited-list #\] stream t)))
     nil *meta-readtable*)

 (dolist (char '(#\] #\}))
   (set-macro-character char (get-macro-character #\)) nil *meta-readtable*))
) ;eval-when

(defun enable-meta-syntax (&optional globally)
  (shiftf *saved-readtable* *readtable* *meta-readtable*)
  ;(format t "meta-syntax enabled: ~S" (eq *readtable* *meta-readtable*))
  (when globally
    #+lispworks (editor::set-vector-value
                 (slot-value editor::*default-syntax-table* 'editor::table) '(#\[ #\{) 2)
    #+lispworks (editor::set-vector-value 
                 (slot-value editor::*default-syntax-table* 'editor::table) '(#\] #\}) 3)))

(defun disable-meta-syntax (&optional globally)
  (when *saved-readtable*			; without-interrupts preferrable
    (shiftf *readtable* *saved-readtable* nil))
  (when globally
    #+lispworks (editor::set-vector-value
                 (slot-value editor::*default-syntax-table* 'editor::table) '(#\[ #\{) 7)
    #+lispworks (editor::set-vector-value
                 (slot-value editor::*default-syntax-table* 'editor::table) '(#\] #\}) 7)))

#+lispworks
(lw:define-action "Delivery Actions" "Shake meta-parse readtables"
                  #'(lambda () (setq *saved-readtable* nil
                                     *meta-readtable* nil)
                               (unintern *saved-readtable*) 
                               (unintern *meta-readtable*)))

(defmacro match-test (test x y)
 ;;; Tester form compiler
  (if (or (symbolp test) (and (consp test) (eq (first test) 'lambda)))
      `(,test ,x ,y)
      `(funcall ,test ,x ,y)))

(defmacro string-match (pattern &key (test 'eql))
  (cond ((characterp pattern)
         `(when (and (< %index% %end%)
                     (match-test ,test (char %source% %index%) ,pattern))
                     ;(eql (char %source% %index%) ,pattern))
            (incf %index%)))
        ((stringp pattern)
         (let ((old-index-symbol (gensym "OLD-INDEX-")))
           `(let ((,old-index-symbol %index%))
              (or (and ,@(map 'list
                              #'(lambda (char) `(string-match ,char :test ,test))
                              pattern))
                  (progn (setq %index% ,old-index-symbol) nil)))))))

(defmacro string-match-type (predicate &optional var)
  (let ((char-symbol (gensym)))
    `(when (< %index% %end%)
       ,(if var
            `(let ((,char-symbol (char %source% %index%)))
               (declare (type character ,char-symbol))
               (when (,predicate ,char-symbol)  ;(typep ,char-symbol ',pattern)
                 (setq ,var ,char-symbol)
                 (incf %index%)))
            `(when (,predicate (char %source% %index%))
               (incf %index%))))))

;;; Lists: functions list-match[-type] should eat elements by poping them form %source%

(defun compile-list (pattern &key (test 'eql))
  (if (atom pattern)					; end of the list pattern
      (if (meta-p pattern)				; is it dotted ending with meta?
          (compileit pattern :subject-type :self :test test)
          `(match-test ,test %source% ',pattern))
      `(and ,(compileit (first pattern) :subject-type 'list :test test)
            ,(compile-list (rest pattern) :test test))))

(defmacro list-match (pattern &key (test 'eql) self)
  (if self						; pattern is for the entire subject
      (if (atom pattern)		 		; kept in %source% itself
          `(match-test ,test %source% ',pattern)
          (compile-list pattern :test test))
      `(when (and (consp %source%)	 		; pattern is for the first element
                  ,(if (atom pattern)			
                       `(match-test ,test (first %source%) ',pattern)
                       `(let ((%source% (first %source%)))	; drill down to the first
                          ,(compile-list pattern :test test)))) ; with new lexical %source%
         (setq %source% (rest %source%))
         t)))

(defmacro list-match-type (predicate &optional var self)
  (if self						; test the type of the entire
      `(,@(if (eq predicate t)				; subject kept in %source% itself
              '(progn)
              `(when (,predicate %source%)))
        ,@(when var `((setq ,var %source%)))
        t)
      `(when ,(if (eq predicate t)
                  '(consp %source%)
                  `(and (consp %source%) (,predicate (first %source%))))
         ,(if var
              `(setq ,var (pop %source%))
              '(setq %source% (rest %source%)))
         t)))


(defmacro stream-match (pattern &key (test 'eql))
  `(when (match-test ,test (peek-char %source%) ,pattern)
     (read-char %source%)))

(defmacro stream-match-type (predicate &optional var)
  `(when (,predicate (peek-char %source%)) ; (typep (peek-char ,%source%) ',pattern)
     ,(if var
          `(setq ,var (read-char %source%))
          `(read-char %source%))))

(defun compileit (pattern &key subject-type (test 'eql))
  (if (meta-p pattern)
      (ecase (meta-char pattern)
        (#\! (meta-form pattern))
        (#\[ `(and ,@(mapcar (lambda (form)
                               (compileit form :subject-type subject-type :test test))
                             (meta-form pattern))))
        (#\{ `(or ,@(mapcar (lambda (form)
                              (compileit form :subject-type subject-type :test test))
                            (meta-form pattern))))
        (#\$ `(not (do () ((not ,(compileit (meta-form pattern)
                                            :subject-type subject-type
                                            :test test))))))
        (#\@ (let ((form (meta-form pattern)))
               (if (eq subject-type :self)
                   `(list-match-type ,(first form) ,(second form) ,t)
                   `(,(ecase subject-type
                        (string 'string-match-type)
                        (list   'list-match-type)
                        (stream 'stream-match-type))
                     ,(first form)
                     ,@(when (second form) `(,(second form))))))))
      (if (eq subject-type :self)
          `(list-match ,pattern :test ,test :self ,t)
          `(,(ecase subject-type
               (string 'string-match)
               (list   'list-match)
               (stream 'stream-match))
            ,pattern
            :test ,test))))

;;; Args: test - keyword argument
;;; - is evaluated when passed to outer with-...-meta
;;; - is not evaluated when passed to inner match!

(defmacro with-stream-meta ((stream &key (test 'eql)) &body body)
  `(let ((%source% ,stream))
     (macrolet ((match (pattern &key (test ',test))
                  (compileit pattern :subject-type 'stream :test test)))
       ,@body)))

(defmacro with-string-meta ((string &key (start 0) end (test 'eql)) &body body)
  `(let* ((%source% ,string)
          (%index% ,start)
          (%end% (or ,end (length %source%))))
     (declare (fixnum %index% %end%) (type simple-string %source%))
     (macrolet ((match (pattern &key (test ',test))
                  (compileit pattern :subject-type 'string :test test)))
       ,@body)))

(defmacro with-list-meta ((list &key (test 'eql)) &body body)
  `(let ((%source% ,list))
     (macrolet ((match (pattern &key (test ',test))
                  (compileit pattern :subject-type :self :test test)))
       ,@body)))
