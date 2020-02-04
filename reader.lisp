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
(mof:defcon +caret+ #\^)
(mof:defcon +bang+ #\!)
(mof:defcon +tilde+ #\~)
(mof:defcon +dollar+ #\$)
(mof:defcon +space+ #\Space)
(mof:defcon +left-paren+ #\()
(mof:defcon +right-paren+ #\))

(defun |[-reader| (stream char)
  "Use [\"/tmp/file.ext\"] as a shorthand for #P\"/tmp/file.ext\""
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :preserve)
    (pathname (mof:join (read-delimited-list +right-bracket+ stream t)))))

(set-macro-character +left-bracket+ #'|[-reader|)
(set-macro-character +right-bracket+ (get-macro-character #\) nil))

(defun |^-reader| (stream char)
  "Use ^OBJECT as shorthand for EVAL-EXPR."
  (declare (ignore char))
  (list 'streams/core:eval-expr (list 'quote (read stream t nil t))))

(set-macro-character +caret+ #'|^-reader|)

(defun |$-reader| (stream char)
  "Use $OBJECT as shorthand for SHOW."
  (declare (ignore char))
  (list 'streams/core:show (list 'quote (read stream t nil t))))

(set-macro-character +dollar+ #'|$-reader|)

(defun |~-reader| (stream char)
  "Use ~OBJECT as shorthand for DUMP"
  (declare (ignore char))
  (list 'streams/core:dump (list 'quote (read stream t nil t))))

(set-macro-character +tilde+ #'|~-reader|)

(defun read-next-object (separator delimiter &optional (input-stream *standard-input*))
  "Read the next lisp object from INPUT-STREAM while using SEPARATOR as token separator and DELIMITER as the end of expression."
  (flet ((peek-next-char () (peek-char t input-stream t nil t))
         (discard-next-char () (read-char input-stream t nil t)))
    (if (and delimiter (char= (peek-next-char) delimiter))
        (progn (discard-next-char)
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

(defun read-left-paren (stream char)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character +space+ 'read-separator)
    (loop :for object = (read-next-object +space+ +right-paren+ stream)
          :while object
          :collect object :into objects
          :finally (return `(vector ,@objects)))))

(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))

;;; (set-macro-character +left-paren+ #'read-left-paren)
;;; (set-macro-character +right-paren+ #'read-delimiter)

(defreadtable streams/syntax
  (:merge :standard)
  (:macro-char #\? (constantly nil) t)
  (:case :preserve))
