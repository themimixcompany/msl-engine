;;;; tests.lisp

(uiop:define-package #:streams/tests
    (:use #:cl #:fiveam #:streams/core)
  (:export #:run!
           #:all-tests
           #:run-tests))

(in-package #:streams/tests)

(def-suite all-tests)
(in-suite all-tests)

(defun run-tests ()
  "Run all the tests defined in the suite."
  (run! 'all-tests))

(defun s (expr)
  "Return the string represantion of EXPR."
  (show expr nil))

(test show-tests
  "Test the output of SHOW."
  (is (string= (s '(@walt "Walt" "Disney" :number 100 200 300 :species (@person Human) :state "IL"))
               "Walt Disney"))
  (is (string= (s '(@walt))
               "Walt Disney"))
  (is (string= (s '(@walt "WD" :number 0))
               "WD"))
  (is (string= (s '(@walt "XD" :number))
               "XD"))
  (is (string= (s '(@walt :number 1 :age 20 :gender m))
               "XD"))
  (is (null (s '(@david))))
  (is (null (s '(@david :number 0 :age 21 :gender m))))
  (is (string= (s '(@walt :number))
               "1"))
  (is (string= (s '(@walt :number :state))
               "XD"))
  (is (string= (s '(@walt :number :state "NY"))
               "XD")))

(test eval-expr-tests
  "Test the values returned by EVAL-EXPR."
  (is (null nil)))
