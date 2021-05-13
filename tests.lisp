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
  (defun extract (expr &optional clear)
    "Return the string representation of EXPR after dispatching it. If optional argument CLEAR is true, clear the universe prior to evaluation."
    (when clear (clear))
    (dispatch expr :log nil)
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

  (defun split-test-expr (test-expr)
    "Split a test expr into parts."
    (cl-ppcre:split "\\s+(⇒|→|-->)\\s+" test-expr))

  (defun test-expr-passes-p (test-expr)
    (destructuring-bind (&optional expression expr value)
        (split-test-expr test-expr)
      (when expression
        (multiple-value-bind (extract-expr extract-value)
            (extract expression)
          (dbg expression extract-expr extract-value)
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
             nil)
            ;; fallback
            (t nil))))))

  (defm define-test (name description)
    "Define a macro for creating tests."
    `(progn
       (clear)
       (test ,name ,description
         ,@(loop :for test-expr :in (read-test-file (string-downcase (string name)))
                 :collect `(is (test-expr-passes-p ,test-expr)))))))


;;--------------------------------------------------------------------------------------------------
;; tests
;;--------------------------------------------------------------------------------------------------

(define-test walt "Basic build-up of an atom and its metadata")
(define-test abstract-over-embed "Abstracting over an embedded atom and metadata")
(define-test complex-regex "Regex with inner expression or spaces")
(define-test regex-on-embedding "Regex inside an embedded atom")
(define-test single-circular-reference "A single atom which refers to itself")
(define-test transform-on-meta "Using a bracketed transform in a metadata value, with and without a literal <value")

;(define-test abstract-over-metadata "Abstracting over an embedded atom and metadata")

;;(define-test chi-town-regex-currying "Regex currying")
;;(define-test circular-references "Circularity of two atoms which refer to each other")
;;(define-test data-types-formats "Datatypes and formats")
;;(define-test lost-attractions "Lost Attractions")
;;(define-test multiple-d-or-f "Multiple (d) or (f) in the same expression")
;;(define-test self-ref-complex "A single expression refers to itself using complex regex")

(defun call/fresh-universe (test)
  (with-fresh-universe ()
    (run! test)))

(def run-tests ()
  "Run all the tests defined in the suite."
  ;;(call/fresh-universe 'all-tests)
  (call/fresh-universe 'walt))

