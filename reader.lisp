;;;; reader.lisp

(uiop:define-package #:streams/reader
    (:use #:cl)
  (:nicknames #:s/reader))

(in-package #:streams/reader)

(defun bracket-reader (stream char)
  "Use [\"/tmp/file.ext\"] as a shorthand for #P\"/tmp/file.ext\""
  (declare (ignore char))
  (pathname (mof:join-strings (read-delimited-list #\] stream t))))

(set-macro-character #\[ #'bracket-reader)
(set-macro-character #\] (get-macro-character #\) nil))

(defun percent-reader (stream char)
  "Use %object as shorthand to display the contents of the slots of object."
  (declare (ignore char))
  (list 'streams/common:dump-object (read stream t nil t)))
(set-macro-character #\% #'percent-reader)

(defun |#d-reader| (stream subchar arg)
  "Use #dobject as shorthand to display the contents of the slots of object."
   (declare (ignore subchar arg))
  (list 'streams/common:dump-object (read stream t nil t)))
(set-dispatch-macro-character #\# #\d #'|#d-reader|)
