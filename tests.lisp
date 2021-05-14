;;;; tests.lisp

(uiop:define-package #:msl-engine/tests
  (:use #:cl
        #:fiveam
        #:msl-engine/specials
        #:msl-engine/config
        #:msl-engine/classes
        #:msl-engine/common
        #:msl-engine/parser
        #:msl-engine/reader
        #:msl-engine/writer
        #:marie))

(in-package #:msl-engine/tests)


;;--------------------------------------------------------------------------------------------------
;; top-level definitions
;;--------------------------------------------------------------------------------------------------

(def-suite all-tests)
(in-suite all-tests)


;;--------------------------------------------------------------------------------------------------
;; combined
;;--------------------------------------------------------------------------------------------------

(eval-always
  (defun extract (expr)
    "Return the string representation of EXPR after dispatching it. If optional argument CLEAR is true, clear the universe prior to evaluation."
    (recall expr :log nil))

  (defun tests-directory ()
    "Return the path for the tests."
    (let* ((system (asdf:find-system (intern +self+ (find-package :keyword))))
           (source-directory (asdf:system-source-directory system))
           (directory (uiop:merge-pathnames* source-directory "tests")))
      (when (uiop:directory-exists-p directory)
        directory)))

  (defun read-test-file (test-file)
    "Read the test file TEST from the tests directory."
    (let* ((directory (uiop:ensure-directory-pathname(tests-directory)))
           (basename (cat test-file ".mst"))
           (file (uiop:merge-pathnames* basename directory)))
      (format t "~A~%" file)
      (uiop:read-file-lines file)))

  (defun split-line (line)
    "Split a test expr into parts."
    (cl-ppcre:split "\\s+(⇒|→|-->|=>)\\s+" line))

  (defun format-line (&rest args)
    "Return a line string from ARGS."
    (fmt "~{~A~^ → ~}" args))

  (defun normalize-line (line)
    "Return a test expr string from LINE without extraneous characters."
    (destructuring-bind (&optional expression expr value)
        (split-line line)
      (when expression
        (cond ((∧ (¬ expr) (¬ value))
               (format-line expression))
              ((∧ expr (¬ value))
               (format-line expression expr))
              ((∧ expr value)
               (format-line expression expr value))))))

  (defun build-line (line)
    "Return a new line from the expression in LINE containing the corresponding answers for the expr and value fields."
    (destructuring-bind (&optional expression expr value)
        (split-line (normalize-line line))
      (when expression
        (multiple-value-bind (extract-expr extract-value)
            (extract expression)
          (cond ((∧ expression (¬ expr) (¬ value))
                 (format-line expression))
                ((∧ expression expr (¬ value))
                 (format-line expression extract-value))
                ((∧ expression expr value)
                 (format-line expression extract-expr extract-value)))))))

  (defun line-passes-p (line)
    "Return true if LINE evaluates to true."
    (destructuring-bind (&optional expression expr value)
        (split-line line)
      (when expression
        (multiple-value-bind (extract-expr extract-value)
            (extract expression)
          (if (¬ value)
              (format t "~&|> MSL: ~S~%—> VAL: ~S~%<— VAL: ~S~%"
                      expression expr extract-value)
              (format t "~&|> MSL: ~S~%—> EXP: ~S~%<— EXP: ~S~%—> VAL: ~S~%<— VAL: ~S~%"
                      expression expr extract-expr value extract-value))
          (cond
            ;; both the expected expression and value are specified in the test
            ((∧ expr value)
             (∧ (string= expr extract-expr)
                (string= value extract-value)))
            ;; only the expected value is specified in the test
            ((∧ expr (¬ value))
             (string= expr extract-value))
            ;; neither the expected expression nor the expected value are specified in the test
            ((∧ (¬ expr) (¬ value))
             t)
            ;; fallback
            (t nil))))))

  (defm define-test (name description)
    "Define a macro for creating tests."
    `(test ,name ,description
       ,@(loop :for line :in (read-test-file (string-downcase (string name)))
               ;; test for equality instead of boolean
               :collect `(is (line-passes-p ,line))))))

;;; rebuild text-expr line

;;; NOTE
;;;
;;; - TEST-ONE
;;;   - accept line of msl
;;;   - test it
;;;   - if it passes, return NIL, done
;;;   - otherwise, return both the expression and the value, and the original
;;; - TEST-FILE
;;;   - opens file from disk
;;;   - takes the next line
;;;   - calls TEST-ONE
;;;   - if NIL, loop to the next line
;;;   - otherwise, accumulate TEST-ONE results into a list
;;;   - when the file is done
;;;   - if the result list is empty, announce all tests passed, return NIL
;;;   - otherwise, show the result list, one failure per line, return failure list


;;--------------------------------------------------------------------------------------------------
;; tests
;;--------------------------------------------------------------------------------------------------

(define-test walt "Basic build-up of an atom and its metadata")
;; (define-test abstract-over-embed "Abstracting over an embedded atom and metadata")
;; (define-test abstract-over-metadata "Abstracting over an embedded atom and metadata")
;; (define-test chi-town-regex-currying "Regex currying")
;; (define-test circular-references "Circularity of two atoms which refer to each other")
;; (define-test complex-regex "Regex with inner expression or spaces")
;; (define-test data-types-formats "Datatypes and formats")
;; (define-test regex-on-embedding "Regex inside an embedded atom")
;; (define-test self-ref-complex "A single expression refers to itself using complex regex")
;; (define-test single-circular-reference "A single atom which refers to itself")
;; (define-test transform-on-meta "Using a bracketed transform in a metadata value, with and without a literal <value")
;;(define-test multiple-d-or-f "Multiple (d) or (f) in the same expression")

(defun run-with-fresh-universe (test)
  (with-fresh-universe ()
    (run! test)))

(def run-tests ()
  "Run all the tests defined in the suite."
  (run-with-fresh-universe 'all-tests))
