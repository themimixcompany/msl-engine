;;;; tests.lisp

(uiop:define-package #:streams/tests
    (:use #:cl #:fiveam #:streams/core)
  (:nicknames #:s/tests)
  (:export #:run!
           #:all-tests
           #:run-tests))

(in-package #:streams/tests)

(def-suite all-tests)
(in-suite all-tests)

(defun run-tests ()
  "Run all the tests defined in the suite."
  (run! 'all-tests))

(test show-tests
  "Test the output of SHOW."
  (is (string= $(@walt "Walt" "Disney" :number 1 2 :species "Human" :state "IL") "Walt Disney"))
  (is (null $(@walt "Walt" "Disney" :number 1 2 :species (@person) :state "IL")))
  (is (null $(@walt (@nothing))))
  (is (string= $(@walt) "Walt Disney"))
  (is (string= $(@walt "WD" :number 0) "WD"))
  (is (string= $(@walt "XD" :number) "XD"))
  (is (string= $(@walt :number 1 :age 20 :gender m) "XD"))
  (is (null $(@david)))
  (is (null $(@david :number 0 :age 21 :gender m)))
  (is (string= $(@walt :number) "1"))
  (is (string= $(@walt :number :state) "XD"))
  (is (string= $(@walt :number :state "NY") "XD")))

(test eval-expr-tests
  "Test the values returned by EVAL-EXPR."
  (is (null nil)))
