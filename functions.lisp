;;; -*- Mode: Lisp; -*-
;;; Ystok Library - Misc. utility functions
;;; Copyright (c) 2003-2006 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ystok.library)

(defvar *kwd-package* (find-package :keyword))

(defun intern-upcase (string &optional (package *kwd-package*))
  (if (and (stringp string) (plusp (length string)))
      (intern (string-upcase string) package)
      nil))

(declaim (inline hex-digit-char hex-digit-char-p first-or-self listify xmemq)
         (ftype (function (character) t) hex-digit-char-p))

(defun hex-digit-char (code)
  (digit-char code 16))
(defun hex-digit-char-p (char)
  (declare (optimize (debug 0) (speed 3) (safety 0))
           (type character char))
  (digit-char-p char 16))
#|(defun code-hex-digit (code)
  (code-char (+ unsigned-byte (if (< unsigned-byte 10)
                                  #.(char-code #\0)
                                  #.(- (char-code #\a) 10)))))|#

;; Args: char      Separator among the hex digits.
;;       interval  Number of digit pairs (bytes) to group when char is not null.
;;       bit-count Length of every group of digits in bits.
(defgeneric object-hex-string (object &optional char interval bit-count))

(defun assoq (object alist &key key)
  (assoc object alist :test #'eq :key key))

(define-compiler-macro assoq (object alist &key key)
  `(assoc ,object ,alist :test #'eq ,@(when key `(,:key ,key))))

(defun cdr-assoq (object alist &key key)
  (cdr-assoc object alist :test #'eq :key key))

(define-compiler-macro cdr-assoq (object alist &key key)
  `(cdr-assoc ,object ,alist :test #'eq ,@(when key `(,:key ,key))))

(defun cdr-assoq3 (object alist &optional default)
 ;;; Accepts third argument a la get/getf.
  ;; Value: default if no association found.
  (let ((pair (assoq object alist)))
    (if pair (cdr pair) default)))

(define-compiler-macro cdr-assoq3 (object alist &optional default)
  (with-gensyms (pair)
    `(let ((,pair (assoq ,object ,alist)))
       (if ,pair (cdr ,pair) ,default))))

#+(or lispworks allegro sbcl)
(defun class-all-subclasses (class &optional self)
 ;;; Collect unsorted list of all descending subclasses of the class given.
  ;; Args: self If true, push the class itself into the result
  ;; Value: A freshly-consed list.
  ;; NB: Similar to LW-TOOLS::CLASS-ALL-SUBCLASSES but returns metaobjects instead of names
  (let* ((class (if (symbolp class) (find-class class) class))
         (acc (if self (list class) ())))
    (labels ((%collect (class)
               (dolist (class (#+lispworks clos:class-direct-subclasses
                               #+allegro   mop:class-direct-subclasses
                               #+sbcl      sb-mop:class-direct-subclasses
                               class))
                 (pushnew class acc)
                 (%collect class))))
      (%collect class))
    acc))

(defgeneric copy-object (from to)
 ;;; Specialising on type is not enough, one must also specialise on "intent"
  ;; Value: to
 #+(or lispworks allegro sbcl)
 (:method ((from standard-object) (to standard-object))
  ;; (assert (or (subtypep (class-of from) (class-of to))
  ;;             (subtypep (class-of to) (class-of from))))
  (do (slot-name
       (rest #+lispworks (clos:class-slots (class-of from))
             #+allegro (mop:class-slots (class-of from))
             #+sbcl (sb-mop:class-slots (class-of from))
             ;#+pcl (pcl:class-slots (pcl:class-of from))
             (rest rest)))
      ((null rest)
       to)
    (when (slot-exists-p to (setq slot-name
                                  #+lispworks (clos:slot-definition-name (first rest))
                                  #+allegro (mop:slot-definition-name (first rest))
                                  #+sbcl (sb-mop:slot-definition-name (first rest))))
                                  ;#+pcl (slot-value (first rest) 'pcl::name)
      (if (slot-boundp from slot-name)
          (setf (slot-value to slot-name) (slot-value from slot-name))
          (slot-makunbound to slot-name))))) )

(defgeneric clone-object (self &key &allow-other-keys)
 (:documentation "Create a (deep or shallow) copy of the argument")
 (:method (self &key) self)
 (:method ((self standard-object) &key)
  #+lispworks
  (CLOS::COPY-STANDARD-INSTANCE self)			; makes a shallow copy
  #-lispworks
  (copy-object self (allocate-instance (class-of self)))))

;;; CAUTION: All composed functional arguments must be one-parameter!

(defun compose (&rest functions)
  (lambda (arg) (reduce #'funcall functions :from-end t :initial-value arg)))
(define-compiler-macro compose (&whole whole &rest functions)
  (if (every #'constantp functions)
      (labels ((fn (list)
                 (if list
                     (let ((first (first list)))
                       (if (and (consp first)
                                (member (first first) '(quote function))
                                (symbolp (second first)))
                           `(,(second first) ,(fn (rest list)))
                           `(funcall ,first ,(fn (rest list)))))
                     'arg)))
        `(lambda (arg) ,(fn functions)))
      whole))

;;; Finding position of a character in a simple-string

(declaim (ftype (function (character simple-string
                           &key (:start fixnum) (:end (or fixnum null)) (:from-end t))
                          t) char-position)
         (inline char-position))
(defun char-position (char string &key (start 0) end from-end)
  (declare (type character char) (type simple-string string))
  #+lispworks (sys::find-character$simple-string string start (or end (length string))
                                                 char from-end)
  #-lispworks (position char string :test #'char= :start start :end end
                        :from-end from-end))
#+lispworks
(define-compiler-macro char-position (&whole whole char arg2 &key (start 0) end from-end)
  (if (constantp arg2)
      (let ((string (eval arg2)))				; should return string!
        `(,(cond ((lw:base-string-p string)
                  'sys::find-character$simple-base-string)
                 ((every #'lw:base-char-p string)
                  (setq string (coerce string 'base-string))
                  'sys::find-character$simple-base-string)
                 (t
                  'sys::find-character$simple-text-string))
          ,string ,start ,(or end (length string)) ,char ,from-end))
      whole))

(defun first-or-self (arg)
 ;;; The first element of arg, if it is a list; else arg itself.
  ;; Code from Paradigms of AI Programming, Copyright (c) 1991 Peter Norvig
  (if (consp arg) (first arg) arg))

(defun insert (item list &key after before (key #'identity) (test #'eql))
 ;;; Args: after, before  If negative integers, specify positions form the end
  ;;			  of the list: -1 = last, -2 = last-but-one
  ;; Value: The new list with the item inserted.
  ;; CAUTION: May use the same structure as list due to subseq!
  (let ((singleton (list item))
        position)
    (cond ((setq position (cond ((integerp after)
                                 (if (minusp after) (+ (length list) after) after))
                                (after
                                 (position after list :key key :test test))))
           (append (subseq list 0 (1+ position)) singleton (subseq list (1+ position))))
          ((setq position (cond ((integerp before)
                                 (if (minusp before) (+ (length list) before) before))
                                (before
                                 (position before list :key key :test test))))
           (append (subseq list 0 position) singleton (subseq list position)))
          (t (append list singleton)))))

(declaim (inline iplusp))
(defun iplusp (object)
 ;;; Useful function for testing allow-* slots of grid
  (if (integerp object) (locally (declare (integer object)) (plusp object)) nil))

(defun listify (arg)
  (if (listp arg) arg (list arg)))

(defun mappend (fn list)
 ;;; Append the results of calling fn on each element of list.
  ;; Like mapcon, but uses append instead of nconc.
  (apply #'append (mapcar fn list)))

(defun memq (object list &key key)
  (member object list :test #'eq :key key))

(define-compiler-macro memq (object list &key key)
  `(member ,object ,list :test #'eq ,@(when key `(,:key ,key))))

(defun xmemq (list object)
 ;;; Usefull as test or test-not argument of remove/delete
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (member object list :test #'eq))

(defun flatten (exp)
 ;;; Get rid of imbedded lists (to one level only).
  (mappend #'listify exp))

#+lispworks
(setf (fdefinition 'put)
      (fdefinition #+lispworks4 'cl::setf-get  #-lispworks4 'sys::%put))

(defun random-elt (seq) 
 ;;; Pick a random element out of a sequence.
  (elt seq (random (length seq))))

(defun remove-property (plist keyword)
 ;;; Value: New plist that can share the tail structure with the source plist
  ;; LW CAUTION: sys::remf-aux seems destructive!
  (if (eq (getf plist keyword *unbound-value*) *unbound-value*)
      plist
      (do ((acc ())
           (rest plist (cddr rest))
           first)
          ((null rest)
           (nreverse acc))
        (if (eq (setq first (first rest)) keyword)
            (return (nreconc acc (cddr rest)))
            (setq acc (list* (second rest) first acc))))))	; in reverse order

(defun retain-properties (plist keywords)
 ;;; Value: Plist with only the properties mentioned by keywords in reverse order.
  (declare (optimize (debug 0) (speed 3) (safety 0)))
  (do ((acc ())
       (rest plist (cddr rest))
       first)
      ((null rest)
       acc)
    (when (memq (setq first (first rest)) keywords)
      (setq acc (list* first (second rest) acc)))))

(declaim (inline shrink-vector))
(defun shrink-vector (vector size)
 ;;; Args: vector Either simple or not (at least for LispWorks), general or string.
  ;; Value: Vector of the same element type, which is EQ to the argument vector.
  ;; NB: Borrowed from PURI
  #+allegro	(progn (excl::.primcall 'sys::shrink-svector vector size) vector)
  #+sbcl	(sb-kernel:shrink-vector vector size)
  #+cmu		(lisp::shrink-vector vector size)
  #+lispworks	(system::shrink-vector$vector vector size)
  #+scl		(common-lisp::shrink-vector vector size)
  #-(or allegro cmu lispworks sbcl scl)
		(subseq vector 0 size))

#+lispworks
(defun string-append* (&rest args)
 ;;; Similar to string-append but treats the last argument as "spread",
  ;; in accordance to the traditional Lisp styling convention.
  (if (rest args)
      (lw:string-append (lw:string-append* (butlast args))
                        (lw:string-append* (first (last args))))
      (lw:string-append* (first args))))

#+lispworks
(define-compiler-macro string-append* (&whole whole &rest args)
  (if (rest args)
      whole
      `(lw:string-append* ,(first args))))

#+(and lispworks capi)
(setf (fdefinition 'xtypep) (fdefinition 'CAPI::REVERSED-TYPEP))
#-(and lispworks capi)
(defun xtypep (x y) (typep y x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Domain aggregate functions

(defgeneric dlookup (what domain &key where default &allow-other-keys))
 ;;; Args: default - Value to return if zero tuples were found

(defgeneric dmax (what domain &key where default)
 ;;; Args: default  Initial value to compare
 (:method (key (domain vector) &key where default)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;(if where
  (loop for tuple across domain
        for datum = (and (imp where (funcall where tuple))
                         (cond ((numberp key) (elt tuple key))
                               (key (funcall key tuple))	; (functionp key)
                               (t tuple)))
        when datum
          if (numberp default) if (> datum default) do (setq default datum) end
          else do (setq default datum)
        finally (return default)))
 (:method (key (domain list) &key where default)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop for tuple in domain
        for datum = (if (imp where (funcall where tuple))
                        (cond ((numberp key) (elt tuple key))
                              (key (funcall key tuple))	; (functionp key)
                              (t tuple)))
        when datum
          if (numberp default) if (> datum default) do (setq default datum) end
          else do (setq default datum)
        finally (return default))) )

(defgeneric dmin (what domain &key where default)
 ;;; Args: default  Initial value to compare
 (:method (key (domain list) &key where default)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop for tuple in domain
        for datum = (if (imp where (funcall where tuple))
                        (cond ((numberp key) (elt tuple key))
                              (key (funcall key tuple))	; (functionp key)
                              (t tuple)))
        when datum
          if (numberp default) if (< datum default) do (setq default datum) end
          else do (setq default datum)
        finally (return default))) )

(defgeneric dsum (what domain &key where default)
 ;;; Args: default  Initial value to sum up.
  ;; Value: nil (not 0!) if nothing to sum up
 (:method (key (domain vector) &key where default)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;(if where
  (loop for tuple across domain
        for datum = (if (imp where (funcall where tuple))
                        (cond ((numberp key) (elt tuple key))
                              (key (funcall key tuple))	; (functionp key)
                              (t tuple)))
        when datum
          if (numberp default) do (incf default datum) else do (setq default datum)
        finally (return default)))
 (:method (key (domain list) &key where default)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;(if where
  (loop for tuple in domain
        for datum = (if (imp where (funcall where tuple))
                        (cond ((numberp key) (elt tuple key))
                              (key (funcall key tuple))	; (functionp key)
                              (t tuple)))
        when datum
          if (numberp default) do (incf default datum) else do (setq default datum)
        finally (return default))) )


(defun interleave-seq (result-type delimiter seq &optional (keep-empty-subseqs nil))
  ;(unless (every #'null seq)
    (case result-type
      (string
       (with-output-to-string (stream)
         (do* ((some nil) ;(or some first)
               (rest seq (rest rest))
               (first (first rest) (first rest)))
              ((null rest))
           (when (and first			; DI 2013-Apr-19: Exclude empty substrings
                      (or keep-empty-subseqs (not (stringp first)) (plusp (length first))))
             (when some (princ delimiter stream))
             (princ first stream)
             (setq some t)))))
      (list
       (do* ((result ())
             (some nil) ;(or some first)
             (rest seq (rest rest))
             (first (first rest) (first rest)))
            ((null rest)
             (nreverse result))
           (when (or first keep-empty-subseqs)
             (when some (push delimiter result))
             (push first result)
             (setq some t))))))

;;; SEQUENCE SPLIT family 
;;; LispWorks-based equivalent of Edi Weitz's cl-ppcre split,
;;; but accepts Emacs-like regular expressions
;;; (see "Regular expression syntax" section of the LispWorks Editor User Guide).

#+lispworks
(defun split (pattern seq &key (start 0) (end (length seq))
                               limit
                               with-registers-p
                               sharedp
                               (keep-empty-subseqs nil)
                               (case-sensitive :default))
  "Matches PATTERN against target string SEQ as often as possible
 and returns a list of the substrings between the matches.
Arguments:
 PATTERN If matches an empty string the scan is continued one position
	 behind this match. (I don't believe this is valueable for split.)
 WITH-REGISTERS-P
	 If true, substrings corresponding to matched
	 registers are inserted into the list as well.
 LIMIT	 Limits the number of elements returned - registers aren't counted.
 	 If LIMIT is neither NIL nor zero, all empty substrings are kept
	 no matter the value of KEEP-EMPTY-SUBSEQS parameter.
 KEEP-EMPTY-SUBSEQS
	If true, middle empty substrings are included in the result,
	otherwise discarded. Whether the lefmost and rightmost empty
	substrings are stripped or not depends on the LIMIT parameter
	(for cl-ppcre compatibility).
 CASE-SENSITIVE
	See lw:find-regexp-in-string. Does not work for non-ascii characters.
 SHAREDP If true, the substrings may share structure with SEQ."
  ;; OMIT-UNMATCHED-P (unsupported)
  ;;	 If true, unmatched registers will simply be left out,
  ;;	 otherwise they will show up as NIL.
  (declare (optimize (speed 3) (safety 0) (hcl:fixnum-safety 0)))
  (loop with count = (if (eql limit 0) nil limit)
        and substr-fn = (if sharedp #'nsubseq #'subseq)
        and regexp = (if (stringp pattern)
                         (lw:precompile-regexp pattern :case-sensitive case-sensitive)
                         pattern)
        and acc = ()
        and string-seen = nil
        and pos and len and match-end
        while (and (or (null count) (plusp (decf count)))
                   (< start end)
                   (multiple-value-setq (pos len)
                       (lw:find-regexp-in-string regexp seq :start start :end end)))
        do (setq match-end (+ pos len))
        when (or count
                 (< start pos)
                 (and string-seen keep-empty-subseqs))	 ; skip empty strings from the left
        do (push (if (< start pos)
                     (funcall substr-fn seq start pos)
                     +null-string+)
                 acc)
           (setq string-seen t)
        when with-registers-p				; optionally insert matched subject
        do (push (funcall substr-fn seq pos match-end) acc)
        do (setq start (if (zerop len)			; if we had a zero-length match,
                           (1+ pos)			; advance by one position 
                           match-end))
        finally (when (or count (< start end)) 		; skip empty strings from the right
                  (push (funcall substr-fn seq start end) acc))
                (return (nreverse acc))))

#+lispworks
(define-compiler-macro split (&whole form pattern seq &rest rest
                              &key (case-sensitive :default supplied-p) &allow-other-keys
                              &environment env)
  "Make sure that constant forms are compiled into scanners at compile time."
  (if (constantp pattern env)
      `(split (load-time-value (lw:precompile-regexp ,pattern
                                ,@(when supplied-p `((:case-sensitive ,case-sensitive)))))
              ,seq ,@rest)
      form))

;;; Group of functions based on PARTITION 
;;; by Christophe Rhodes http://www-jcsu.jesus.cam.ac.uk/~csr21/
;;; Examples:
;;;
;;; * (partition #\; "a;;b;c")
;;; -> ("a" "b" "c"), 6
;;;
;;; * (partition #\; "a;;b;c" :from-end t)
;;; -> ("a" "b" "c"), 0
;;;
;;; * (partition #\; "a;;b;c" :from-end t :count 1)
;;; -> ("c"), 4
;;;
;;; * (partition #\; "a;;b;c" :keep-empty-subseqs t)
;;; -> ("a" "" "b" "c"), 6
;;;
;;; * (partition-if (lambda (x) (member x '(#\a #\b))) "abracadabra")
;;; -> ("r" "c" "d" "r"), 11
;;;
;;; * (partition #\; ";oo;bar;ba;" :start 1 :end 9)
;;; -> ("oo" "bar" "ba"), 9

(defun split-seq (delimiter seq 
                  &key (count nil) (keep-empty-subseqs nil) 
                       (from-end nil) (start 0) (end nil)
                       (test nil test-supplied) (test-not nil test-not-supplied)
                       (key nil key-supplied))
 "Return a list of subsequences in seq delimited by delimiter.
Arguments:
 KEEP-EMPTY-SUBSEQS
   If true, empty subsequences will be included in the result;
   otherwise they will be discarded.
 FROM-END
   Values of NIL and T are equivalent unless :count is supplied.
 All other keywords work analogously to those for CL:SUBSTITUTE.
Values:
 1) A list of subsequences.
 2) An index suitable as an argument to CL:SUBSEQ into the sequence
    indicating where processing stopped."
  (let ((len (length seq))
        (other-keys (nconc (when test-supplied (list :test test))
                           (when test-not-supplied (list :test-not test-not))
                           (when key-supplied (list :key key)))))
    (declare (fixnum len))
    (unless end (setq end len))
    (if from-end
        (loop for right of-type fixnum = end then left
              for left of-type fixnum = (max (or (apply #'position delimiter seq 
                                                        :end right
                                                        :from-end t
                                                        other-keys)
                                                 -1)
                                             (1- start))
              unless (and (= right (1+ left)) (not keep-empty-subseqs))
              if (and count (>= nr-elts count))
                return (values (nreverse subseqs) right)	; we can't take any more
              else 
                collect (subseq seq (1+ left) right) into subseqs
                and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left of-type fixnum = start then (+ right 1)
            for right of-type fixnum = (min (or (apply #'position delimiter seq 
                                                       :start left
                                                       other-keys)
                                                len)
                                            end)
            unless (and (= right left) (not keep-empty-subseqs))
            if (and count (>= nr-elts count))
              return (values subseqs left)			; we can't take any more
            else
              collect (subseq seq left right) into subseqs
              and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))

(defun split-seq-if (predicate seq 
                     &key (count nil) (keep-empty-subseqs nil)
                          (from-end nil) (start 0) (end nil) (key nil key-supplied))
 "Return a list of subsequences in seq delimited by items satisfying predicate.
Arguments:
 KEEP-EMPTY-SUBSEQS
   If true, empty subsequences will be included in the result;
   otherwise they will be discarded.
 FROM-END
   Values of NIL and T are equivalent unless :count is supplied.
 All other keywords work analogously to those for CL:SUBSTITUTE.
Values:
 1) A list of subsequences.
 2) An index suitable as an argument to CL:SUBSEQ into the sequence
    indicating where processing stopped."

  (let ((len (length seq))
        (other-keys (when key-supplied 
		      (list :key key))))
    (declare (fixnum len))
    (unless end (setq end len))
    (if from-end
        (loop for right of-type fixnum = end then left
              for left of-type fixnum = (max (or (apply #'position-if predicate seq 
                                                        :end right
                                                        :from-end t
                                                        other-keys)
                                                 -1)
                                             (1- start))
              unless (and (= right (1+ left)) (not keep-empty-subseqs))
              if (and count (>= nr-elts count))
                return (values (nreverse subseqs) right)	; we can't take any more
              else 
                collect (subseq seq (1+ left) right) into subseqs
                and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left of-type fixnum = start then (+ right 1)
            for right of-type fixnum = (min (or (apply #'position-if predicate seq 
                                                       :start left
                                                       other-keys)
                                                len)
                                            end)
            unless (and (= right left) (not keep-empty-subseqs))
            if (and count (>= nr-elts count))
              return (values subseqs left)			; we can't take any more
            else
              collect (subseq seq left right) into subseqs
              and sum 1 into nr-elts
            until (<= end right)
            finally (return (values subseqs right))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TINY-EVAL  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple evaluator for use instead of the standard eval function

(defvar *tiny-environment* ()
  "Global enviromnent providing tiny-eval with default.")

(defvar *tiny-symbol-value-function*
  (lambda (symbol environment)				; cdr-assoq
    (let ((pair (assoc symbol environment :test #'eq)))
      (if pair
          (cdr pair)
          (logg :eval "Tiny eval: variable ~S is unbound in environment." symbol))))
  "The function associating symbols with their values.")

(defvar *tiny-setq-function*
  (lambda (symbol value environment)
    (if-bind (pair (assoq symbol environment))
      (rplacd pair value)
      (error "Tiny setq: variable ~S is unbound in environment." symbol))
    value)
  "Function to assign VALUE to SYMBOL; do nothing if it is missing from ENVIRONMENT.")

(defun tiny-eval (form &optional (environment *tiny-environment*))
  "Evaluator combining both eval and apply.
Arguments:
 FORM
   If it is list, the evaluator treats specially the functions that are standard
   macros or special operators in Lisp. For now, the following are recognized:
     IF WHEN UNLESS AND OR NOT QUOTE FUNCTION PROGN SETQ CASE.
   If it is a symbol, the evaluator looks at its name first;
   when the name is boundp, the evaluator interprets the symbol as
   a dynamic variable and retreive its value via symbol-value;
   otherwise the look-up proceeds via *tiny-symbol-value-function*.
 ENVIRONMENT
   Alist binding variables to values."
  ;; NB: The following are also allowed (as they are functions): VALUES.
  (cond ((consp form)
         (let ((function (first form))
               (args (rest form)))
           (flet ((progn% (body &aux (value nil))
                    (dolist (arg body value)
                      (setq value (tiny-eval arg environment)))))
             (case function
               ((IF WHEN)
                (if (tiny-eval (first args) environment)
                    (tiny-eval (second args) environment)
                    (tiny-eval (third args) environment)))
               (UNLESS
                   (if (tiny-eval (first args) environment)
                       (tiny-eval (third args) environment)
                       (tiny-eval (second args) environment)))
               (AND
                (do ((value t)					; we do not use loop always
                     (rest args (rest rest)))			; as it returns t, not the
                    ((null rest) value)				; value of the last form
                  (unless (setq value (tiny-eval (first rest) environment))
                    (return nil))))
               (OR
                (loop for arg in args
                      thereis (tiny-eval arg environment)))
               (NOT
                (not (tiny-eval (first args) environment)))
               (QUOTE
                (first args))
               (PROGN
                (progn% args))
               (SETQ
                ;; Assign "usual" variable in the "lexical" environment, not special
                (funcall *tiny-setq-function*
                         (second form) (tiny-eval (third form) environment) environment))
               (CASE
                (do ((value (tiny-eval (first args) environment))
                     arg
                     (rest (rest args) (rest rest)))
                    ((null rest) nil)
                  (when (and (consp (setq arg (first rest)))
                             (eql value (first arg)))		; not eq
                    (return (progn% (rest arg))))))
               (FUNCTION
                (fdefinition (first args)))
               (otherwise
                ;; Can invoke -> (error 'undefined-function :name function)
                (apply function
                       (loop for arg in args
                             collect (tiny-eval arg environment))))))))
        ((symbolp form)					;(and (not (constantp form)))
         (if (boundp form)
             ;; The symbol is a Lisp keyword, constant, or special variable - return
             ;; its dynamic value, either global or bound within the excution process
             (symbol-value form)
             ;; The symbol is a "usual" variable - seek in the "lexical" environment
             (funcall *tiny-symbol-value-function* form environment)))
        (t form)))						; constant

(pushnew :ylib *features*)
