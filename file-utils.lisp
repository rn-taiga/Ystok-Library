;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-
;;; Ystok Library - File and operation system-specific functions
;;; Copyright (c) 2003-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;; Copyright (c) 2004-2006, Peter Seibel and Dr. Edmund Weitz.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ystok.library)

(defvar *temp-pathnames* ()
 "List of temporal files (PDF, HTML, or other) generated during session
and auto removed on end.")

#+lispworks
(lw:define-action "When quitting image" "Delete temporary files"
  (lambda ()
    (do ((pathname (pop *temp-pathnames*) (pop *temp-pathnames*)))
        ((null pathname))
      ;; (delete-file file nil) does not signal if the file does not exist
      ;; but do if other kind of error, e.g. file is busy.
      (ignore-errors (delete-file pathname)))))

;;; CL-FAD

(defparameter *stream-buffer-size* 8192)

(defgeneric copy-stream (from to &key buffer-size)
 (:documentation "Copies into TO \(a stream) from FROM \(also a stream) until the end
of FROM is reached, in blocks of *stream-buffer-size*.  The streams
should have the same element type.")
 (:method (from to &key (buffer-size *stream-buffer-size*))
  (unless (subtypep (stream-element-type to) (stream-element-type from))
    (error "Incompatible streams ~A and ~A." from to))
  (loop with buffer = (make-array buffer-size
                                  :element-type (stream-element-type from))
        for pos = (read-sequence buffer from)
        until (zerop pos)
           do (write-sequence buffer to :end pos))
  (values)))

#-lispworks
(defun copy-file (from to &aux (overwrite t))
  "Copies the file designated by the non-wild pathname designator FROM
to the file designated by the non-wild pathname designator TO.
If OVERWRITE is true overwrites the file designtated by TO if it exists."
  #+allegro
  (excl.osi:copy-file from to :overwrite overwrite)
  #-allegro
  (let ((element-type 'octet))
    (with-open-file (in from :element-type element-type)
      (with-open-file (out to :element-type element-type
                              :direction :output
                              :if-exists (if overwrite :supersede :error))
        (copy-stream in out))))
  (values))

#-lispworks
(defun directory-pathname-p (pathname)
 ;;; Args: pathname Pathname
  ;; Value: True if PATHNAME designate a directory, NIL otherwise.
  ;; NB: It is irrelevant whether file or directory designated does actually exist.
  (flet ((%component-present-p (value)
           ;; Helper checking whether VALUE is neither NIL nor the keyword :UNSPECIFIC
           (and value (not (eq value :unspecific)))))
    (and (not (%component-present-p (pathname-name pathname)))
         (not (%component-present-p (pathname-type pathname))))))

#-lispworks
(defun directory-pathname (arg)
 ;;; cl-fad:pathname-as-directory
  ;; Converts the non-wild pathname designator PATH to directory form.
  (let ((pathname (pathname arg)))
    (if (directory-pathname-p pathname)
        pathname
        (make-pathname :directory (append (or (pathname-directory pathname)
                                              (list :relative))
                                          (list (file-namestring pathname)))
                       :name nil
                       :type nil
                       :defaults pathname))))

;;; OTHERS

#-lispworks
(defun current-pathname (&optional relative-pathname type)
 ;;; Merge relative-pathname against the current file being compiled or loaded
  (let ((base (or *compile-file-pathname* *load-pathname* (user-homedir-pathname))))
    (if relative-pathname
        (merge-pathnames (make-pathname :directory (pathname-directory relative-pathname)
                                        :name (or (pathname-name relative-pathname)
                                                  (pathname-name base))
                                        :type (or type
                                                  (pathname-type relative-pathname)
                                                  (pathname-type base)))
                         (pathname-location base)
                         :newest)
        base)))

;;; getenv borrowed from ASDF 2.013
#+mcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:define-entry-point (_getenv "getenv") ((name :string)) :string))

(defun getenv (arg)
  #+(or abcl clisp xcl) (ext:getenv arg)
  #+allegro (sys:getenv arg)
  #+clozure (ccl:getenv arg)
  #+(or cmu scl) (cdr-assoc arg ext:*environment-list* :test #'string=)
  #+cormanlisp
  (let* ((buffer (ct:malloc 1))
         (cname (ct:lisp-string-to-c-string arg))
         (needed-size (win:getenvironmentvariable cname buffer 0))
         (buffer1 (ct:malloc (1+ needed-size))))
    (prog1 (if (zerop (win:getenvironmentvariable cname buffer1 needed-size))
               nil
               (ct:c-string-to-lisp-string buffer1))
      (ct:free buffer)
      (ct:free buffer1)))
  #+ecl (si:getenv arg)
  #+gcl (system:getenv arg)
  #+genera nil
  #+lispworks (lispworks:environment-variable arg)
  #+mcl (ccl:with-cstrs ((name arg))
          (let ((value (_getenv name)))
            (unless (ccl:%null-ptr-p value)
              (ccl:%get-cstring value))))
  #+sbcl (sb-ext:posix-getenv arg)
  #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl genera lispworks mcl sbcl scl xcl)
  (error "GETENV is not supported on your implementation"))

(defun get-folder (what &key create)
 ;;; Workaround to identify Windows special folders as pathnames, not as files
  ;; Args: what ::= :appdata :common-appdata :local-appdata
  ;;		    :documents :my-documents :common-documents
  ;;		    :program-files :temp
  ;; Value: Pathname with null name and type (with trailing slash /) on success
  ;;	    or NIL if does not exist.
  ;; :appdata
  ;;	Detection of the current user path requires IE 4 and above.
  ;;    Examples:
  ;;	 Windows 2000: #P"C:/Documents and Settings/user/Application Data/"
  ;;     Windows 7,Server 2008: #P"C:/Users/user/AppData/Roaming/"
  ;;                  (Russian: #P"C:/Пользователи/user/AppData/Roaming/")
  ;; :common-appdata
  ;;	Detection of the all users path requires IE 5 and above.
  ;;	Not available on Windows 95 with IE 4 without Active Desktop installed.
  ;; :local-appdata
  ;;	The local (nonroaming) application data directory.
  ;;	Available on Windows 2000 and above.
  ;;    Examples:
  ;;	 Windows 2000: #P"C:/Documents and Settings/user/Local Settings/Application Data/"
  ;;     Windows 7, Server 2008: #P"C:/Users/user/AppData/Local/"
  ;; :documents (= :my-documents), :common-documents
  ;;	Not available on Windows 95 without IE 4 installed.
  ;; :program-files
  ;;    Examples:
  ;;	 Windows 2000: #P"C:/Program Files/"
  ;;     Windows Server 2008: #P"C:/Program Files (x86)/"
  #-lispworks (declare (ignore create))
  (when-let (path (cond #+lispworks
                        ((stringp what)
                         (cond ((lw:file-directory-p what)
                                what)
                               (create
                                (system:make-directory what)	; signals if exists
                                what)))
                        ((keywordp what)
                         (case what
                           #+win32
                           (:program-files
                            (or (getenv "ProgramFiles")
                                (string-append (or (getenv "SystemDrive") "C:")
                                               "\\Program Files")))
                           (:temp
                            (or #+lispworks6 (hcl:get-temp-directory)
                                #-lispworks6 (getenv "TEMP")
                                #-lispworks6 (getenv "TMP")
                                #+lispworks (sys:get-user-profile-directory)
                                #+lispworks (hcl:get-working-directory)))
                           #+lispworks
                           (otherwise
                            (sys:get-folder-path what :create create))))))
    (directory-pathname path)))

(defun %pathname-component-equal (arg1 arg2)
 ;;; CAUTION: LWW probe-file can result in device and version eql to :UNSPECIFIC
  ;;          where as capi:prompt-for-file fill it with NIL!
  (cond ((and (symbolp arg1) (symbolp arg2))			; works for NILs
         (or (eq arg1 arg2)
             (and (eq arg1 :unspecific) (null arg2))		; :UNSPECIFIC and NIL
             (and (null arg1) (eq arg2 :unspecific))))		; are considered equal
        ((and (stringp arg1) (stringp arg2))
         (#-(or linux unix) locale-string-equal
          #+(or linux unix) string=
          arg1 arg2))
        ((and (consp arg1) (consp arg2))			; directories
         (and (eq (first arg1) (first arg2))
              (loop (setq arg1 (rest arg1)
                          arg2 (rest arg2))
                    (unless (if arg1
                                (if arg2 
                                     (#-(or linux unix) locale-string-equal 
                                      #+(or linux unix) string=
                                      (first arg1) (first arg2))
                                    nil)
                                (if arg2
                                    nil
                                    (return t)))
                      (return nil)))))))

;;; The ENOUGH-PATHNAME stub is needed to patch case-sensitive functionality
;;; of enough-namestring in LWW 4.4.6, 5.0, 5.1, e.g.:
;;;  
;;;  (setq f  #P"e:/projects/file.ext"
;;;        d1 #P"e:/projects/"
;;;        d2 #P"E:/PROJECTS/")
;;;  (enough-namestring f d1) => "file.ext"
;;;  (enough-namestring f d2) => "e:\\projects\\file.ext"
;;;  (equal d1 d2) => T
;;;  
;;;  For equal, CLHS specifies the following:
;;;    Two pathnames are equal if and only if all the corresponding components
;;;    (host, device, and so on) are equivalent. Whether or not uppercase and
;;;    lowercase letters are considered equivalent in strings appearing in
;;;    components is implementation-dependent.
;;;    pathnames that are equal should be functionally equivalent.
;;;  
;;;  In the above example, as pathnames d1 and d2 are equal, uppercase and
;;;  lowercase letters in components seem equivalent.
;;;  
;;;  IMHO, d1 and d2 are not functionally equivalent w.r.t. enough-namestring.

(defun enough-pathname (pathspec base)
 ;;; Args: base  Unlikely to contain :back
 (let* ((pathname (pathname pathspec))
        (host (pathname-host pathname)))
   (or (and (or (null host) (%pathname-component-equal host (pathname-host base)))
            (let* (directory
                   (base-rest (pathname-directory base))
                   (rest (pathname-directory pathname))
                   (first (first rest)))
              (unless (memq first '(:relative :back))	; relative means enough
                (when (loop				; loop over base-rest and rest
                       (cond ((null base-rest)
                                ;; Когда либо кончился base-rest, либо несовпадение первых,
                                ;; а первый оставшийся не :absolute и не :relative,
                                ;; сalculate minimum relative path
                                 (setq directory 
                                       (if (and rest (not (symbolp first)))
                                           (cons :relative (copy-list rest))
                                           (copy-list rest)))
                                 (return t))
                                ((not (and rest
                                           (%pathname-component-equal (first base-rest)
                                                                      first)))
                                 (return nil)))
                       (setq base-rest (rest base-rest)
                             rest (rest rest)
                             first (first rest)))
                  (make-pathname :host nil
                                    ;:device nil
                                    :directory directory
                                    :name (pathname-name pathname)
                                    :type (pathname-type pathname))))))
       pathname)))

(defun pathname-equal (arg1 arg2)
 ;;; Locale-based pathname comparison a la
  ;;	(lw:locale-string-equal (namestring arg1) (namestring arg2))
  ;; Args: arg1, arg2  Are being converted to pathnames if needed.
  ;; NB: Equality from the namestring "point of view"
  (unless (pathnamep arg1) (setq arg1 (pathname arg1)))
  (unless (pathnamep arg2) (setq arg2 (pathname arg2)))
  (and (%pathname-component-equal (pathname-host arg1) (pathname-host arg2))
       (%pathname-component-equal (pathname-device arg1) (pathname-device arg2))
       (%pathname-component-equal (pathname-directory arg1) (pathname-directory arg2))
       (%pathname-component-equal (pathname-name arg1) (pathname-name arg2))
       (%pathname-component-equal (pathname-type arg1) (pathname-type arg2))
       #-(or win32 mswindows :linux)
       (%pathname-component-equal (pathname-version arg1) (pathname-version arg2))))

#-lispworks
(defun pathname-location (pathname)
 ;;; New pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
  ;; and null NAME and TYPE components
  ;; Similar to CL-FAD:pathname-as-directory
  (make-pathname :name nil :type nil :version nil :defaults pathname))

(defun safe-filename-charp (char)
 ;;; Is not the character forbidden in filenames on some of the following platforms?
  ;; - Unix (Debian Reference, Chapter 1. GNU/Linux tutorials)
  ;;   { } ( ) [ ] ' ` " \ / > < | ; ! # & ^ * % @ $ #\Spaces #\Tabs #\Newline
  ;; - Windows
  ;;   \ / : * ? " < > |
  (declare (type character char))
  (or (not (standard-char-p char))
      (char-position char "{}()[]'`\"\\/><|;!#&^*%@$:? 
")))
      ;#+lispworks (sys::find-character$simple-base-string
      ;             #.(coerce "..." 'simple-base-string) 0 #.(length "...") char)
      ;#-lispworks (find char "..." :test #'char=)

(defun safe-namestring (string &optional (replace #\_))
 ;;; Substitute forbidden characters in the STRING given with the REPLACE char.
  (substitute-if replace #'safe-filename-charp string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  FILE FORMAT  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format is a keyword identifying (MIME) type. Usually results from pathname-type

(defvar *file-type-format-hashtable* (make-hash-table :test #'equalp))

#+lispworks (dspec:define-dspec-class define-file-format nil nil)
#+lispworks (dspec:define-form-parser (define-file-format
                                       (:parser dspec:single-form-form-parser)))

(defmacro define-file-format (symbol (&rest flags) &rest types)
 ;;; Args: symbol Keyword as a rule (if not, must be kept explicitly during delivery)
  ;;       flags  List of keywords or any Lisp objects denoting additional properties.
  ;;		  For example, :ascii means transfering by FTP in ASCII mode
  ;;       types  List of pathname type strings
  (with-gensyms (type)
   `(with-dspec (define-file-format ,symbol) ,:check-redefinition-p ,t
      ,@(when flags
          `((put ',symbol 'file-format-flags (list ,@flags))))
      (dolist (,type (list ,@types) ',symbol)
        (setf (gethash ,type *file-type-format-hashtable*) ',symbol)))))

(defun file-type-format (pathname &optional (type (pathname-type pathname)) errorp)
  (cond ((gethash type *file-type-format-hashtable*))
        (errorp
         (error "Undefined format~@[ ~s~] of the file: ~a." type pathname))
        (type
         (intern-upcase type))))

(defun file-format-flags (format)
  (get format 'file-format-flags))

;;; Misc

(defun load-list (pathname &key ((:package *package*) (load-time-value
                                                       (find-package :cl-user)))
                                (external-format
                                 #+(and lispworkds win32) win32:*multibyte-code-page-ef*
                                 #-(and lispworkds win32) :default))
 ;;; Read all forms from the file specified by the pathname and collect into a list.
  (with-open-file (stream pathname :direction :input :external-format external-format)
    (without-reader-errors ()
      (loop for form = (read stream nil *unbound-value*)
            until (eq form *unbound-value*)
            collect form))))

