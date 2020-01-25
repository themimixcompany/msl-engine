;;;; reader.lisp

(uiop:define-package #:streams/reader
    (:use #:cl #:named-readtables)
  (:nicknames #:s/reader)
  (:export #:syntax))

(in-package #:streams/reader)

(mof:defcon +left-bracket+ #\[)
(mof:defcon +right-bracket+ #\])
(mof:defcon +percent+ #\%)
(mof:defcon +space+ #\Space)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |[-reader| (stream char)
    "Use [/bones/hat scalpel.dragon] as a shorthand for #P\"/bones/hat scalpel.dragon\""
    (declare (ignore char))
    (pathname (mof:join (read-delimited-list +right-bracket+ stream t))))

  (defun |%-reader| (stream char)
    "Use %object as shorthand to display the contents of the slots of object."
    (declare (ignore char))
    (list 'streams/common:dump-object (read stream t nil t)))

  (defreadtable syntax
    (:merge :standard)
    (:macro-char +left-bracket+ #'|[-reader|)
    (:macro-char +right-bracket+ (get-macro-character #\)))
    (:macro-char +percent+ #'|%-reader|)
    (:case :preserve)))
