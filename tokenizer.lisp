;;;; tokenizer.lisp

(uiop:define-package #:streams/tokenizer
    (:use #:cl)
  (:nicknames #:s/tokenizer)
  (:export #:tokenize-expr-1
           #:tokenize-expr-2))

(in-package #:streams/tokenizer)

(defun tokenize-expr-1 (data)
  "Tokenize DATA using the built-in Lisp reader."
  (if (stringp data)
      (handler-bind ((sb-int:standard-readtable-modified-error
                       #'(lambda (c)
                           (let ((r (find-restart 'continue c)))
                             (when r
                               (invoke-restart r))))))
        (let ((package *package*))
          (with-standard-io-syntax
            (let ((*read-eval* nil)
                  (*package* package))
              (set-macro-character #\, (constantly '|,|))
              (streams/core:split-prefixes (marie:read-from-string* data))))))
      (streams/core:split-prefixes data)))

(defun tokenize-expr-2 (data)
  "Tokenize DATA using MaxPC."
  (streams/core:split-prefixes (maxpc:parse data (streams/expr:=sexp))))
