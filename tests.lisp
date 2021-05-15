;;;; tests.lisp

(uiop:define-package #:msl-engine/tests
  (:use #:cl
        #:fiveam
        ;;#:msl-engine/specials
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

(eval-always
  (defv *test-universe* (make-universe "test-universe"))

  (defun clean-expression (expression)
    "Do clean up on EXPRESSION."
    (string-upcase (cl-ppcre:regex-replace-all "\\s" expression "")))

  (defun clearp (expression)
    "Return true if EXPRESSION is the (@CLEAR) expression."
    (string= (clean-expression expression) "(@CLEAR)"))

  (defun split-line (line)
    "Split a test expr into parts."
    (cl-ppcre:split "\\s+(⇒|→|-->|=>)\\s+" line))

  (defun format-line (&rest args)
    "Return a line string from ARGS."
    (fmt "~{~A~^ → ~}" args))

  (defun extract (expr)
    "Return the string representation of EXPR after dispatching it. If optional argument CLEAR is true, clear the universe prior to evaluation."
    (if (clearp expr)
        (clear-universe)
        (recall expr :log nil)))

  (defun tests-directory ()
    "Return the path for the tests."
    (let* ((system (asdf:find-system (intern "MSL-ENGINE" (find-package :keyword))))
           (source-directory (asdf:system-source-directory system))
           (directory (uiop:merge-pathnames* source-directory "tests")))
      (when (uiop:directory-exists-p directory)
        directory)))

  (defun read-test-file (test-file)
    "Read the test file TEST from the tests directory."
    (let* ((directory (uiop:ensure-directory-pathname(tests-directory)))
           (basename (cat test-file ".mst"))
           (file (uiop:merge-pathnames* basename directory)))
      (uiop:read-file-lines file)))

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

  (defun compose-line (line)
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

  (defm define-test (name description)
    "Define a macro for creating tests."
    `(test ,name ,description
       ,@(loop :for file-line :in (read-test-file (string-downcase (string name)))
               :for line = (normalize-line file-line)
               :for line-split = (split-line line)
               :for line-expr = (car line-split)
               :for line-results = (cdr line-split)
               :for compose = (compose-line line)
               :for compose-split = (split-line compose)
               :for compose-results = (cdr compose-split)
               :collect `(is (string= ,line ,compose)
                             "~2&~S~2% evaluated to ~2&~S~2% which is not equal to ~2&~S~2%"
                             ,line-expr
                             ,(apply #'format-line line-results)
                             ,(apply #'format-line compose-results))))))


;;--------------------------------------------------------------------------------------------------
;; tests
;;--------------------------------------------------------------------------------------------------

(define-test walt "Basic build-up of an atom and its metadata")
(define-test abstract-over-embed "Abstracting over an embedded atom and metadata")
(define-test abstract-over-metadata "Abstracting over an embedded atom and metadata")
(define-test chi-town-regex-currying "Regex currying")
(define-test circular-references "Circularity of two atoms which refer to each other")
(define-test complex-regex "Regex with inner expression or spaces")
(define-test data-types-formats "Datatypes and formats")
(define-test regex-on-embedding "Regex inside an embedded atom")
(define-test self-ref-complex "A single expression refers to itself using complex regex")
(define-test single-circular-reference "A single atom which refers to itself")
(define-test transform-on-meta "Using a bracketed transform in a metadata value, with and without a literal <value")
(define-test multiple-d-or-f "Multiple (d) or (f) in the same expression")

(def run-tests ()
  "Run all the tests defined in the suite."
  (with-universe (*test-universe*)
    (run! 'all-tests)))
