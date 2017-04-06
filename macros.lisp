;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-
;;; Ystok-Library - Macros and basics
;;; Copyright (c) 2003-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ystok.library)

;; SBCL eql-strictness workaround: make sure the value is evaluated only once.
(defmacro defconstant* (name value &optional doc)
 #+lispworks
 `(dspec:def (defconstant* ,name)
    (cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                    ,@(when doc (list doc))))
 #-lispworks
 `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                  ,@(when doc (list doc))))

(defconstant* +nil-circle+ '#1=(nil . #1#))	; cicled list of NILs, e.g. for :names

(defconstant* +null-string+ "")
(defconstant* +all-string+  #-Russian " (all)"	#+Russian " (все)")
(defconstant* +none-string+ #-Russian " (none)"	#+Russian " (нет)")

(defconstant +en-dash+ (code-char #x2013))
(defconstant +em-dash+ (code-char #x2014))

(defparameter *unbound-value* #+lispworks clos::*slot-unbound-string* 
			      #-lispworks "#< Unbound Slot >")

(defmacro in-syntax (readtable)
 ;;; K.Pitman's scheme for using on per-file basis
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* ,readtable)))

;;; Packaging and symbols

(defmacro export-toplevel ((&optional package) &body symbols)
  (let ((args (when package `(',package))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (export (mapcar (lambda (arg) (intern (string arg) ,@args))
                       '(,@symbols))
               ,@args))))

(defmacro with-gensyms (vars &body body)
  #+lispworks `(lw:with-unique-names ,vars ,@body)
  #+sbcl      `(sb-int:with-unique-names ,vars ,@body)
  #-(or lispworks sbcl)
  `(let ,(mapcar (lambda (symbol) `(,symbol (gensym))) vars)
     ,@body))

(defmacro with-package (package &body body)
  `(let ((*package* (if (packagep ,package)
                        ,package
                        (find-package ,package))))
     ,@body))

(defmacro with-dspec (dspec &body body)
 ;;; Useful within defining macro definition that does not contains standard
  ;; defxxx forms, so the definition location is not recorded automatically.
  ;; Args: body ::= ([:check-redefinition-p {:warn|:quiet|:error}] <form>*)
  (declare (ignorable dspec))
  #+lispworks
  (let ((end (when (eq (first body) :check-redefinition-p)
               (prog1 (list :check-redefinition-p (second body))
                 (setq body (cddr body))))))
    `(dspec:def ,dspec
       (when (dspec:record-definition ',dspec (dspec:location) ,.end)
         ,@body)))
  #-lispworks
  `(progn ,@body))

;;; Program control

(defmacro allf (val &rest args)
  (with-gensyms (gval) 
    `(let ((,gval ,val)) 
       (setf ,@(mapcan (lambda (place) `(,place ,gval))
                       args)))))

#+lispworks (editor:setup-indent "allf" 1 2)	; make all args indented by 2

(defun fand (fn &rest functions)
 "Function intersection. E.g. (funcall (fand #'integerp #'plusp #'oddp) 1) returns t"
  (if functions
      (lambda (arg) (and (funcall fn arg) (funcall (apply #'fand functions) arg)))
      fn))

(defmacro if-bind ((var form &rest predicates) form1 form2)
  `(let ((,var ,form))
     ,(if (consp predicates)
          `(if (and ,var (funcall (load-time-value (fand ,@predicates)) ,var))
               ,form1
               ,form2)
          `(if ,var 
               ,form1
               ,form2))))

#+lispworks (editor:setup-indent "if-bind" 1 2)	; make all args indented by 2

(defmacro neq (x y)
  `(not (eq ,x ,y)))

(defmacro nilf (&rest args) ;`(allf nil ,@args)
  `(setf ,@(mapcan (lambda (place) (list place nil)) args)))

(defmacro orf (place value)
  `(or ,place (setf ,place ,value)))

(define-modify-macro toggle2 () not)

(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle2 ,a))
               args)))

(defmacro when-bind ((var form &rest predicates) &body body)
 ;;; Similiar to lw:when-let, but allows predicates and disallows declarations
  ;; for var in the form.
 `(let ((,var ,form))
    ,(if (consp predicates)                            ; compute (consp predicates) at 
         ;`(when (and ,var (funcall (fand ,@predicates) ,var))              ; run-time 
         `(when (and ,var (funcall (load-time-value (fand ,@predicates)) ,var)) ; load
            ,@body)
         `(when ,var 
            ,@body))))
;(when-bind (val 3) (* val val)) ;=> 9

(defmacro while (test &body body)
 ;;; Evaluate the body while the test is true; test is evaluated before each iteration
  (let ((while (gensym)))
    `(block nil
       (tagbody ,while
                (unless ,test (return))
		,@body
                (go ,while)))))

;;; Logical operations

(declaim (ftype (function (t t) t) iff) (inline iff))
(defun iff (arg1 arg2)
 ;;; TODO: Extend to an arbitrary number of args and rewrite as a macro
  (if arg1 arg2 (not arg2)))

(defun xor (&rest args)
  (do ((value (first args) (if (first rest) (not value) value))
       (rest (rest args) (rest rest)))
      ((null rest) value)))
        
(defmacro imp (arg1 arg2)
  `(if ,arg1 ,arg2 t))

;;; lw:removef macroexpansion looks huge, even Stepper does not like it
(defmacro removef (seq item &rest args &environment env)
  (multiple-value-bind (var val values writer reader) (get-setf-expansion seq env)
    `(let* (,@(mapcar #'list var val)
            (,(first values) (remove ,item ,reader ,@args)))
       ,writer)))

(defmacro deletef (seq item &rest args &environment env)
  (multiple-value-bind (var val values writer reader) (get-setf-expansion seq env)
    `(let* (,@(mapcar #'list var val)
            (,(first values) (delete ,item ,reader ,@args)))
       ,writer)))

(defmacro sans (plist &rest keywords)
 ;;; Expand into a call that returns a property list with all the keywords removed
  ;; Args: plist    Evaluated
  ;;       keywords Not evaluated
  ;; Q: Shall we introduce sans* as a replacement of remove-properties?
  (if (rest keywords)
      `(remove-properties ,plist ',keywords)
      `(remove-property ,plist ,(first keywords))))

(defmacro without-reader-errors ((&optional form) &body body)
 ;;; Helper to read is safe manner
  ;; Args: form Evaluated and returned in case of any reader error
  `(handler-case (let ((*read-eval* nil))
                   ,@body)
     (reader-error () ,form)))