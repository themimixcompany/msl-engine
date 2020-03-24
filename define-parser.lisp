;;;; define-parser.lisp

(in-package #:stream)

(defmacro define-parser (name args &body body)
  "Define a function for defining parsers. NAME is the name of the parser
function; ARGS are the arguments passed to a parser—usually NIL; and BODY is the
body contents of the parser function."
  `(progn
     (defun ,name ,args ,@body)
     (setf (fdefinition ',(intern (subseq (string name) 1)) (,name)))))
