;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-
;;; Ystok Library - LispWorks compatibility
;;; Copyright (c) 2003-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Don't use on LispWorks itself!

(in-package :ystok.library)

#+lispworks
(eval-when (:compile-toplevel :load-toplevel)
 (error "This file must not be used on LispWorks!"))

(define-modify-macro appendf (&rest args) append)
(define-modify-macro nconcf (&rest args) nconc)
(defmacro push-end (item place)
  `(nconcf ,place (list ,item)))

(defun cdr-assoc (arg alist &rest args)
  (cdr (apply #'assoc arg alist args)))

(define-setf-expander cdr-assoc (arg alist &rest args &environment env)
  (multiple-value-bind (vars vals new-list writer reader)
      (get-setf-expansion alist env)
    (with-gensyms (id al pair val plist)
      (values `(,id ,@vars ,al ,@(when args `(,plist)))		; ::= :test ... :key ...
              `(,arg ,@vals ,reader ,@(when args `((list ,@args))))
              `(,val)
              `(let ((,pair ,(if args
                                 `(apply #'assoc ,id ,al ,plist)
                                 `(assoc ,id ,al))))
                 (if ,pair
                     (rplacd ,pair ,val)
                     (let ((,(first new-list) (acons ,id ,val ,al)))
                       ,writer))
                 ,val)
              (if args
                  `(apply #'cdr-assoc ,id ,al ,plist)
                  `(cdr-assoc ,id ,al)) ))))
 

(defun locale-char-equal (x y) (char-equal x y))

(defun locale-string-equal (x y) (string-equal x y))
(defun locale-string-greaterp (x y) (string-greaterp x y))
(defun locale-string-lessp (x y) (string-lessp x y))

;(defun do-nothing (&rest args) (delcare (ignore args)) (values))

(defun put (symbol i value)
  (setf (get symbol i) value))

(defmacro rebinding (vars &body body)
 ;; Ensures unique names for all the variables in a groups of forms.
 ;; This macro from Pascal Costanza's lw-compat except for gensyms
 ;; Copyright (c) 2005, 2006, 2008 Pascal Costanza
  (loop for var in vars
	for name = (gensym (symbol-name var))
	collect `(,name ,var) into renames
	collect ``(,,var ,,name) into temps
	finally (return `(let ,renames
			   (with-gensyms ,vars
                             `(let (,,@temps)
                                ,,@body))))))

(defun remove-properties (plist keywords)
  (if (get-properties plist keywords)
      (loop for rest on plist by #'cddr
            unless (member (first rest) keywords :test #'eq)
            collect (first rest) and collect (second rest))
      plist))

(defun string-append (&rest args) 
 ;;; Constructs a string of the same type as the argument with the largest element type.
  ;; Args may also include symbols and single characters.
  (apply #'concatenate 'string (mapcar #'string args)))

(defun string-append* (&rest args)
 ;;; Similar to the above but treats the last argument as "spread"
  ;; in accordance to the traditional Lisp styling convention.
  ;; NB: The following code can bump into call-arguments-limit.
  ;;     For better one, look at list-to-string in HTML-TEMPLATE utils.lisp.
  (apply #'concatenate 'string (append (butlast args) (first (last args)))))


(defmacro when-let (&body body) `(when-bind ,@body))

(defmacro when-let* (bindings &body body)
  (labels ((f (bindings body)
             (if bindings
                 `(when-bind ,(first bindings)
                    ,@(multiple-value-bind (result listp) (f (rest bindings) body)
                        (if listp
                            result
                            (list result))))
                 (values body t))))	; indicate that the first value is a form list
    (if bindings
        (f bindings body)
        `(progn ,@body))))

(defun whitespace-char-p (char)
  (find char '(#\Space #\Tab #\Newline #\Linefeed #\Return #\Page)))


(defmacro without-properties ((new-list list &rest keywords-to-remove) &body body)
  `(let ((,new-list (remove-properties ,list ',keywords-to-remove)))
     ,@body))
