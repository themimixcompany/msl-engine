;;;; reader.lisp

(uiop:define-package #:streams/reader
    (:use #:cl #:named-readtables)
  (:nicknames #:s/reader)
  (:export ;; #:streams-readtable
           ;; #:standard-readtable
           ))

(in-package #:streams/reader)

(mof:defcon +left-bracket+ #\[)
(mof:defcon +right-bracket+ #\])
(mof:defcon +percent+ #\%)
(mof:defcon +bang+ #\!)
(mof:defcon +tilde+ #\~)
(mof:defcon +dollar+ #\$)

(defun |[-reader| (stream char)
  "Use [\"/tmp/file.ext\"] as a shorthand for #P\"/tmp/file.ext\""
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :preserve)
    (pathname (mof:join (read-delimited-list +right-bracket+ stream t)))))

(set-macro-character +left-bracket+ #'|[-reader|)
(set-macro-character +right-bracket+ (get-macro-character #\) nil))

(defun |%-reader| (stream char)
  "Use %OBJECT as shorthand for EVAL-EXPR."
  (declare (ignore char))
  (list 'streams/core:eval-expr (list 'quote (read stream t nil t))))

(set-macro-character +percent+ #'|%-reader|)

(defun |$-reader| (stream char)
  "Use $OBJECT as shorthand for SHOW."
  (declare (ignore char))
  (list 'streams/core:show (list 'quote (read stream t nil t)) nil))

(set-macro-character +dollar+ #'|$-reader|)

(defun |~-reader| (stream char)
  "Use ~OBJECT as shorthand for DUMP"
  (declare (ignore char))
  (list 'streams/core:dump (list 'quote (read stream t nil t))))

(set-macro-character +tilde+ #'|~-reader|)
