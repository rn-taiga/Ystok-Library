;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10;  -*-
;;; Ystok Library - Internationalization/localization (i18n/l10n) and external formats
;;; Copyright (c) 2003-2014 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ystok.library)

;; The character element-type we rely on in our code when no info available in advance.
;; Rationaly:
;;  Sometimes we get source modules which
;;  - supplied by developers that are unreachable or too independent;
;;  - we don't want to edit but use them "as is";
;;  - only works properly when lw:*default-character-element-type* is set to base-char.
;; OT1H, lw:*default-character-element-type* governs standard functionality.
;; OTOH, yl:*default-char-type* governs what is under our control.
;;
;; Used by: ystok.sql.sys::with-sql-string-stream
(defparameter *default-char-type* #+lispworks 'lw:simple-char
                                  #+(and sbcl sb-unicode) 'character
                                  #-(or lispworks (and sbcl sb-unicode)) 'base-char)

(defun ef-type (external-format)
 ;;; Lisp impelementaion-specific type corresponding to the extertnal-format designator
  (if (and external-format (not (eq external-format :default)))
      #+lispworks (ef:external-format-type external-format)
      #-lispworks *default-char-type*
      *default-char-type*))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  DEFNATIVE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Complile- and load-time translation of text.
;;; Additionally coalecse strings, either original or localized.

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defvar *localization-hash-table* (make-hash-table :test #'equal))
 (defparameter *warn-on-missing-localization* nil)

 (defmacro defnative (some &optional (native *unbound-value*) locale)
  "Register translation of SOME object into NATIVE object.
 Args: native - can even be NIL."
  (declare (ignore locale))			; use :self or the like as a default?
   ;; Do not put the entry if the translation is empty (for the language desired)
   (if (eq native *unbound-value*)
       (values)
       `(%ensure-native ,some ,native)))

 (defun %ensure-native (some native)
   ;(unless native (setq native some))
   (multiple-value-bind (old present-p) (gethash some *localization-hash-table*)
     (and present-p (not (equalp old native))
          (warn "Redefining localization of ~S from ~S to ~S." some old native)))
   (setf (gethash some *localization-hash-table*) native))

 (defun %read-localized (stream char number)
  (declare (ignore char number))
  (let ((some (read stream nil nil t)))
    (multiple-value-bind (native present-p) (gethash some *localization-hash-table*)
      (if present-p
          native
          (progn (when *warn-on-missing-localization*
                   (warn "No localization found for ~S." some))
            some)))))

 (set-dispatch-macro-character #\# #\L '%read-localized)
) ; eval-when

#+lispworks
(lw:define-action "Delivery Actions" "Shake localization"
                  (lambda () (setf *localization-hash-table* nil)))
                  ;(maphash (lambda (x y) (remhash x yl::*localization-hash-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  LANGUAGES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RFC3066 (obsoletes RFC1766) - Tags for the Identification of Languages
;;; The language tag is composed of 1 or more parts:
;;; a primary language subtag and a (possibly empty) series of subtags.
;;;	Language-Tag ::= Primary-subtag *( "-" Subtag )
;;;	Primary-subtag ::= 1*8ALPHA
;;;	Subtag ::= 1*8(ALPHA / DIGIT)
;;;
;;; Primary language subtag:
;;; - all 2-letter tags are interpreted according to
;;;   ISO 639, "Code for the representation of names of languages"
;;; Second subtag:
;;; - all 2-letter codes are interpreted as ISO 3166 alpha-2
;;;   country codes denoting the area in which the language is used.
;;; ISO 639/ISO 3166 recommendation (not enforced as the tags are case insensitive)
;;;   Language names are in lower case.
;;;   Country codes are in upper case.
;;; Recommendation:
;;;   Use the shortest (only primary) tag if possible
;;;
;;; Arg: lang is a language tag keyword, either primary or with subtags.

;; Association: language-tag -> primary-subtag
(defparameter *language-subtag-map*
  '((:en-US . :en)	; United States
    (:en-AU . :en)	; Australia
    (:en-CA . :en)	; Canada
    (:en-GB . :en)))	; Great Britain

(declaim (inline language-primary-subtag))
(defun language-primary-subtag (lang)
  (cdr-assoq lang *language-subtag-map*))

(defun language-primary-list (lang)
  (if-bind (language-primary-subtag (language-primary-subtag lang))
    (list lang language-primary-subtag)
    (list lang)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  LABELS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run-time translation of text: label -> string
;;; A label is one of the two kinds:
;;; - a string or a number, so we use hash-table similar to cl-l18n,
;;; - a symbol (keyword) or a list, aka path.
;;;
;;; Labels files are
;;;  - placed into lang/ subdirectory of the product-pathname directory,
;;;  - named {bundle-name}-{language}.lab,
;;;  - loaded by calling the read function in loop.
;;;
;;; Label forest is map: language -> (external-format timestamp . tree),
;;; where tree is a kind of (binary tree), one tree per master language.
;;; ((:en :ascii nil
;;;   (:navigation (:toggle (:title . "..."))
;;;                (:sync   (:title . "...")))
;;;   (:prev . "...")
;;;   (:up   . "...")
;;;   (:next . "...")
;;;   ...))

;; Default language as a language tag keyword
(defvar *language* :en)

;; Bundle is a piece of software, ys::module inherits from it
;; The languages slot is the list of tuples:
;;   ((lang external-format timestamp . tree) ...)
;; where timestamp is one of:
;;   NIL = the labes file has not been loaded yet,
;;   file-write-date = the file modification time of the file loaded.
(defstruct (bundle (:copier nil))
  name				  	; used as file name prefix, defaults to "labels"
  (languages '((:en :ascii nil))) 	; list of tuples
  string-map)			  	; hash: string -> ((lang . string) ...)

(defmacro define-label-language (bundle lang &optional (external-format :ascii))
  `(setf (cdr-assoc ,lang (bundle-languages ,bundle)) (list ,external-format nil)))

;; If true, we check the file modified time in every gettext call
;(defvar *check-labels-file-update* nil)

(defun labels-pathname (bundle lang &optional (type "lab"))
 ;;; Args: bunlde Bundle instance or string designator (e.g. :stopwords).
  ;;       lang   Language tag keyword, primary or with subtags.
  ;; Value: Pathname with the name composed of the bundle name and language lowercased.
  (make-pathname :name (format nil "~(~a-~a~)"
                               (if (bundle-p bundle)
                                   (or (bundle-name bundle) "labels")
                                   bundle)
                               lang)
                 :type type 
                 :defaults (product-pathname "lang/")))

(defun unload-labels (bundle lang &optional (tuple (assoq lang (bundle-languages bundle))))
  (when tuple
    (rplacd (cddr tuple) ())				; cut the old tree
    (when (third tuple)					; clear string-amp
      (when-let (hash-table (bundle-string-map bundle))
        (loop for path being each hash-key in hash-table using (hash-value alist)
              do (setf (gethash path hash-table) (delete lang alist :key #'first))))
      (setf (third tuple) nil))))

(defun load-labels-file (bundle lang &key (pathname (labels-pathname bundle lang))
                                          external-format)
 ;;; Value: integer file-write-date = success
  (logg :info "Loading labels file ~a" pathname)
  (let ((*read-eval* nil)
        (*read-suppress* nil)
        (tuple (assoq lang (bundle-languages bundle)))
        (hash-table (bundle-string-map bundle))
        line tree)
    (unless external-format
      (setq external-format (second tuple)))
    (with-open-file (stream pathname :direction :input
                            :external-format (or external-format :default)
                            :element-type (ef-type external-format))
      ;; To indicate that language has been loaded, we put the tree into the forest 
      ;; immediately but defer creation the string-map hash table.
      (if tuple
          (unload-labels bundle lang tuple)
          (push (setq tuple (list* lang external-format (setq tree (cons nil ()))))
                (bundle-languages bundle)))
      (setq tree (cddr tuple))
      (labels ((%put (self tree path value)
                 ;; Args: tree Cons whose cdr part is modified
                 #+debug (assert (consp tree))
                 (let ((alist (cdr tree))
                       (first (first-or-self path))
                       (rest (if (consp path) (rest path) nil)))
                   (if (atom alist)
                       (cond (path
                              (when (and self alist)	; replace atom with ((nil . atom))
                                (rplacd tree (setq alist (acons nil alist nil))))
                              (let ((tuple (list first)))
                                (rplacd tree (cons tuple alist))
                                (%put first tuple rest value)))
                             (alist
                              #1=(warn "In file ~a, duplicated label:~%~s"
                                       pathname line))
                             (self
                              (rplacd tree value)))
                       ;; (consp alist)
                       (let ((tuple (assoq first alist)))
                         (unless tuple
                           (rplacd tree (cons (setq tuple (list first)) alist)))
                         (%put first tuple rest value))))))
        (while (setq line (read stream nil nil))
          (let ((path (car line))
                (value (cdr line)))
            (cond ((or (stringp path) (numberp path))
                   (unless hash-table
                     (setf hash-table (make-hash-table :test #'equal)
                           (bundle-string-map bundle) hash-table))
                   (if-bind (pair (assoq lang (gethash path hash-table)))
                     (progn #1# (rplacd pair value))
                     (push (cons lang value) (gethash path hash-table))))
                  ((or (symbolp path) (consp path)) 
                   (%put nil tree path value))
                  (t (cerror "Skip the line"
                             "In file ~a, invalid label:~%~s"
                             pathname line)))))))
    (setf (third tuple) (file-write-date pathname))))

(defun gettext (path bundle &optional (lang *language*))
 ;;; Args: lang Language tag
  ;; Value: String or NIL
  ;; Usage:
  ;;	(defmacro _ (text)
  ;;	  `(yl:gettext ,text (load-time-value yl:*current-bundle*)))
  (flet ((%not-found (path)
           ;; If there is no match, capitalize sequence starting from an unknown key
           (cond ((or (stringp path) (numberp path)) path)
                 ((consp path) (format nil "~{~:(~A~)~^ ~}" path))
                 (t (format nil "~:(~A~)" path))))
         (%getl (lang)
           ;; Values: 1) result or NIL 2) Arg for %not-found, if null, use path
           (when-let (tuple (assoq lang (bundle-languages bundle)))
             (let ((file-write-date (third tuple)))
               (when (or file-write-date			; already loaded
                         ;(and (not (zerop file-write-date))	; not forced reload
                         ;     (or (not *check-labels-file-update*)
                         ;         (<= (file-write-date (labels-pathname bundle lang))
                         ;             file-write-date)))
                         (load-labels-file bundle lang :external-format (second tuple)))
                 (if (or (stringp path) (numberp path))
                     (when-let* ((hash-table (bundle-string-map bundle))
                                 (alist (gethash path hash-table)))
                       (cdr-assoq lang alist))
                     (when-let (tree (cddr tuple))		; (timestamp . alist)
                       (labels ((%get (self tree path)
                                  #+debug (assert (consp tree))
                                  (let ((alist (cdr tree)))
                                    (if (atom alist)
                                        (cond (path nil)
                                              (alist)		; success!
                                              (self (values nil self)))
                                        (let* ((first (first-or-self path))
                                               (rest (if (consp path) (rest path) nil))
                                               (tuple (assoq first alist)))
                                          (cond (tuple (%get first tuple rest))
                                                (rest nil)
                                                (first (values nil first))
                                                (self (values nil self))))))))
                         (%get nil tree path)))))))) )
    ;; We dive only one level into the language hierarchy
    (multiple-value-bind (result s) (%getl lang)
      (or result
          (when-let (language-primary-subtag (language-primary-subtag lang))
            (multiple-value-bind (result p) (%getl language-primary-subtag)
              (or result
                  (%not-found (or s p path)))))
          (%not-found (or s path))))))

(defvar *current-bundle*)

(defmacro in-bundle (arg)
  `(setq *current-bundle* ,arg))			; specify manually

(set-dispatch-macro-character #\# #\T
  (lambda (stream char arg)
    (declare (ignore char arg))
    `(gettext ,(read stream) (load-time-value *current-bundle*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  STRING  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Must we have these generic with the lang parameter?

(defun decline-word (quantity body tail1 tail234 &optional (tail0 tail234))
 ;;; Change the case or the word depending on quanty (e.g. for plural)
  ;; Args: body      ¬озможна пуста€ строка +null-string+, но не NIL!
  ;;       quantity  Non-negative integer.
  ;;       tailNN    NIL or string.
  ;; NB: In some languages, e.g. English, 234 и 567890 coincide (plural).
  ;; ѕросклон€ть часть речи word по числам, добавив нужное окончание taili к корню body.
  (let ((mod (if (<= 11 quantity 19) 0 (mod quantity 10))))
    (string-append body (cond ((= mod 1) (or tail1 +null-string+))
                              ((<= 2 mod 4) (or tail234 +null-string+))
                              (tail0)
                              (t +null-string+)))))

(defun genderize-word (descriptor &optional (gender t))
 ;;; Select a word case depending of gender
  ;; Args: descriptor ::= (word-male word-female [word-neuter:=word-male])
  ;; NB: In some languages, e.g. English, there is no gender declination.
  ;; ¬ыбрать форму слова по полу gender
  (if (listp descriptor) 
      (case gender
        (:male (first descriptor))
        (:female (second descriptor))
        (otherwise (or (third descriptor) (first descriptor))))
      descriptor))

(defun ordinalize-tail (quantity &optional (gender t))
 ;;; Change the case of ordinal numeral
  (declare (ignore gender))
  (let ((string (format nil "~:R" quantity)))
    (subseq string (- (length string) 2))))

(defun transliterate-char (char)
 ;;; Value: character, string or nil
  ;; Q: Should we use Unicode names or Adobe glyph names?
  char)

(defun transliterate-string (string &key element-type)
  (let ((string (string string)))
    (with-output-to-string (stream nil :element-type (or element-type
                                                         (array-element-type string)))
      (dotimes (i (length string))
        (declare (fixnum i))
        (let ((result (transliterate-char (schar string i))))
          (cond ((characterp result)
                 (write-char result stream))
                (result
                 (write-string result stream))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  READ/PRINT NUMERIC  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The IO and FORMAT packages are specific to LW 4.x on Windows/Linux.
;;; Starting from release 5, LW do not have the these packages on any platform.

(defparameter *decimal-separator* #\.)

;;; Format a la  ~w,d,k,overflowchar,padchar,decimalcharF
;;; where the decimalchar in non-standard
;;; LispWork NB: @ works for non-negative numbers only if w or d is non-null.

#+(and lispworks4 (or win32 linux)) 
(defun ~f (stream arg &optional colon atsign w d k over pad decimal)
  (declare (ignore colon))
  (cond ((not (realp arg))
         (prin1 arg stream)
         (return-from ~f arg))
        ((not (floatp arg))
         (setq arg (float arg))))
  #+lispworks (when (and atsign (not (or w d)) (plusp arg))
                (write-char #\+ stream))
  (when (and (or w d) (null pad)) (setq pad #\Space))
  (if (eql (or decimal (setq decimal *decimal-separator*)) #\.)
      (let ((io:*active-output* stream))
        (declare (dynamic-extent io:*active-output*))
        #1=(format::f-formatter arg w d k over pad atsign))
      (let ((io:*active-output* (make-string-output-stream)))
        (declare (dynamic-extent io:*active-output*))
        #1#
        (write-string (nsubstitute decimal #\.
                                   (get-output-stream-string io:*active-output*)
                                   :count 1)
                      stream)))
  arg)

#-(and lispworks4 (or win32 linux)) 
(defun ~f (stream arg &optional colon atsign w d k over pad decimal)
  (declare (ignore colon))
  (if (eql (or decimal (setq decimal *decimal-separator*)) #\.)
      (format stream #1=(if atsign "~V,V,V,V,V@F" "~V,V,V,V,VF") w d k over pad arg)
      (write-string (nsubstitute decimal #\. (format nil #1# w d k over pad arg)
                                 :count 1)
                    stream))
  arg)

(defun f-format-to-string (arg &key w d k over pad decimal atsign)
  (if arg
      (with-output-to-string (stream)
        (~f stream arg nil atsign w d k over pad decimal))
      +null-string+))

;;; Format a la ~w,d,e,k,overflowchar,padchar,'e,decimalcharE
;;; where the decimalchar in non-standard and exponentchar is always #\e.

#+(and lispworks4 (or win32 linux))
(defun ~e (stream arg &optional colon atsign w d e k over pad #|expchar=#\e|# decimal)
  (declare (ignore colon))
  (cond ((not (realp arg))
         (prin1 arg stream)
         (return-from ~e arg))
        ((not (floatp arg))
         (setq arg (float arg))))
  (when (and w (null pad)) (setq pad #\Space))
  (if (eql (or decimal (setq decimal *decimal-separator*)) #\.)
      (let ((io:*active-output* stream))			 ; k seems obliged
        #2=(format::e-formatter arg w d e (or k 1) over pad #\e atsign))
      (let ((io:*active-output* (make-string-output-stream)))
        (declare (dynamic-extent io:*active-output*))
        #2#
        (write-string (nsubstitute decimal #\.
                                   (get-output-stream-string io:*active-output*)
                                   :count 1)
                      stream)))
   ;     ((format stream (if atsign "~V,V:D" "~V,VD") w pad arg)))
  arg)

#-(and lispworks4 (or win32 linux))
(defun ~e (stream arg &optional colon atsign w d e k over pad #|expchar=#\e|# decimal)
  (declare (ignore colon))
  (if (eql (or decimal (setq decimal *decimal-separator*)) #\.)
      (format stream #1=(if atsign "~V,V,V,V,V,V,'e@E" "~V,V,V,V,V,V,'eE")
              w d e k over pad arg)
      (write-string (nsubstitute decimal #\. (format nil #1# w d e k over pad arg)
                                 :count 1)
                    stream))
  arg)

(defun e-format-to-string (arg &key w d e k over pad decimal atsign)
  (if arg
      (with-output-to-string (stream)
        (~e stream arg nil atsign w d e k over pad decimal))
      +null-string+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  CURRENCY  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *currency-thousand-separator* #\,)
(defparameter *currency-digits* 2)		; the number of fractional digits
(defparameter *currency-symbol* #\$)		; local monetary symbol
(defparameter *currency-order*  0)		; positive currency mode

(defun ~$ (stream arg &optional colon atsign d n w pad
                                decimal (thousand *currency-thousand-separator*)
                                symbol order &aux (dw w))
 ;;; Format a la ~d,n,w,padchar,decimalchar,thousandchar,currencysymbol,currencyorder$
  ;; We cannot rely on the standard ~$ as it does not support thousandchar.
  ;; d	    The number of digits after the decimal.
  ;; n	    The minimum number of digits before decimal (ignored).
  ;; w	    The minimum total width of the field.
  ;; symbol Currency symbol: T means default
  ;; order  0 Prefix, no separation, for example $1.1 
  ;;	    1 Suffix, no separation, for example 1.1$ 
  ;;	    2 Prefix, 1-character separation, for example $ 1.1 
  ;;	    3 Suffix, 1-character separation, for example 1.1 $ 
  ;; @ modifier  If the arg is non-negative, a plus sign is printed.
  ;; : modifier  The sign appears before any padding, and otherwise after the padding.
  ;;
  ;; If w is supplied and the number of other characters to be output is less than w,
  ;; then copies of padchar (which defaults to a space) are output to make the total
  ;; field width equal w. Then n digits are printed for the integer part of arg, 
  ;; with leading zeros if necessary; then a decimal character; 
  ;; then d digits of fraction, properly rounded. 
  ;;
  ;; If the magnitude of arg is so large that more than m digits would have
  ;; to be printed, where m is the larger of w and 100, then an implementation is free,
  ;; at its discretion, to print the number using exponential notation instead, 
  ;; as if by the directive ~w,d+n-1,,,,padcharE.
  ;;
  ;; NB: Due to our relying on LW ~D implementation, which does not breaks padding
  ;;	 by commachar, we can get something like this "00004,324,213.23"

  (declare (ignore n)) ;(unless n (setq n 1))
  (unless (realp arg)
    (prin1 arg stream)
    (return-from ~$ arg))
  (unless d (setq d *currency-digits*))
  (unless decimal (setq decimal *decimal-separator*))
  (when (eql symbol t) (setq symbol *currency-symbol*))
  (unless order (setq order *currency-order*))
  (when (and w symbol) (decf dw (+ (length (string symbol)) (if (> order 1) 1 0))))

  (when (and symbol (evenp order))		; Print prefix currency symbol
    (write-string (string symbol) stream)
    (when (> order 1) (write-char #\Space stream)))
  
  (if thousand
      (let ((sign (cond ((minusp arg) #\-) (atsign #\+)))
            integral fractional)
        (if (integerp arg)
            (setq integral (abs arg)
                  fractional 0s0)
            (multiple-value-setq (integral fractional)		; a la sql-round
                (floor (abs (if (zerop d)			; if d is non-zero
                                (float arg)
                                (let ((factor (if (eql d 2) 1/100 (expt 10 (- d)))))
                                  (* (fround (float arg) factor) factor)))))))
        (cond (w
               (cond (colon
                      (when sign (write-char sign stream) (decf dw))
                      (format stream "~V,V,V:D" (- dw (1+ d)) pad thousand integral))
                     (t
                      (format stream (if atsign "~V,V,V:@D" "~V,V,V:D")
                                     (- dw (1+ d)) pad thousand
                                     (if (minusp arg) (- integral) integral)))))
              (t
               (when sign (write-char sign stream))
               (format stream "~,,V:D" thousand integral)))
        (~f stream fractional nil nil (1+ d) d nil nil nil decimal))
      (~f stream (float arg) nil atsign dw d nil nil pad decimal))

  (when (and symbol (oddp order))		; Print suffix currency symbol
    (when (> order 1) (write-char #\Space stream))
    (write-string (string symbol) stream)))

(defun $-format-to-string (arg &key d n w pad decimal atsign colon
                                    (thousand *currency-thousand-separator*)
                                    symbol order)
  (if arg
      (with-output-to-string (stream nil :element-type yl:*default-char-type*)
        (~$ stream arg colon atsign d n w pad decimal thousand symbol order))
      +null-string+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  DATE/TIME  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *date-separator*	#\-)
(defparameter *date-format*	:ymd)
(defparameter *century-digits*	4)
(defparameter *month-names*  #("January" "February" "March" "April" "May" "June" "July"
                               "August" "September" "October" "November" "December"))
(defparameter *month-abbrevs* (map 'vector (lambda (string) (subseq string 0 3))
                                           *month-names*))
(defparameter *weekday-names*	#("Monday" "Tuesday" "Wednesday" "Thursday"
                                  "Friday" "Saturday" "Sunday"))
(defparameter *weekday-abbrevs*	#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defparameter *time-separator*	#\:)
(defparameter *time-format*	24)		; 12=AM/PM 12-hour, 24=24-hour

(defun timestamp-string (&optional (timezone-p t))
 ;;; Format current universal time with English month abbreviation and optionally
  ;; timezone displacement hours. Useful for logging and debugging.
  (multiple-value-bind (seconds minutes hours day month year weekday daylight-p tz-hour)
      (get-decoded-time)
    (declare (ignore weekday))
    (setq tz-hour (- (if daylight-p 1 0) tz-hour))
    (format nil "~4D-~A-~2,'0D ~2,'0D:~2,'0D:~2,'0D~:[~;~:[+~;-~]~2,'0D~]"
            year (svref (load-time-value *month-abbrevs*) (1- month)) day
            hours minutes seconds
            timezone-p (minusp tz-hour) (abs tz-hour))))
   ;#+sbcl (sbcl-int:format-universal-time nil (get-universal-time)
   ;                :print-timezone timezone-p :print-weekday nil)
