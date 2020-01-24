;;;; reader.lisp

(uiop:define-package #:streams/reader
    (:use #:cl)
  (:nicknames #:s/reader))

(in-package #:streams/reader)

(mof:defcon +left-bracket+ #\[)
(mof:defcon +right-bracket+ #\])
(mof:defcon +space+ #\Space)

(defun bracket-reader (stream char)
  "Use [\"/tmp/file.ext\"] as a shorthand for #P\"/tmp/file.ext\""
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :preserve)
    (pathname (mof:join-strings (read-delimited-list +right-bracket+ stream t)))))

(set-macro-character +left-bracket+ #'bracket-reader)
(set-macro-character +right-bracket+ (get-macro-character #\) nil))

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

(defun read-next-object (separator delimiter &optional (input-stream *standard-input*))
  (flet ((peek-next-char () (peek-char t input-stream t nil t))
         (discard-next-char () (read-char input-stream t nil t)))
    (if (and delimiter (char= (peek-next-char) delimiter))
        (progn
          (discard-next-char)
          nil)
        (let* ((object (read input-stream t nil t))
               (next-char (peek-next-char)))
          (cond ((char= next-char separator) (discard-next-char))
                ((and delimiter (char= next-char delimiter)) nil)
                (t (error "Unexpected next char: ~S" next-char)))
          object))))

(defun read-separator (stream char)
  (declare (ignore stream))
  (error "Separator ~S shouldn't be read alone" char))

(defun read-left-bracket (stream char)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character +space+ #'read-separator)
    (loop :for object = (read-next-object +space+ +right-bracket+ stream)
          :while object
          :collect object :into objects
          :finally (return `(vector ,@objects)))))

;;(set-macro-character +left-bracket+ #'read-left-bracket)

(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))

;;(set-macro-character +right-bracket+ #'read-delimiter)
