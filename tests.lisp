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
  (run! 'all-tests))

(test show-tests
  "Test the output of the SHOW function."
  (is (string= (show '(@walt "Walt" "Disney"
                       :number 100 200 300 :species (@person Human) :state "IL") nil)
               "Walt Disney"))
  (is (string= (show '(@walt) nil)
               "Walt Disney"))
  (is (string= (show '(@walt "WD" :number 0) nil)
               "WD"))
  (is (string= (show '(@walt "XD" :number) nil)
               "XD"))
  (is (string= (show '(@walt :number 1 :age 20 :gender m) nil)
               "XD"))
  (is (null (show '(@david) nil)))
  (is (null (show '(@david :number 0 :age 21 :gender m) nil)))
  (is (string= (show '(@walt :number) nil)
               "1"))
  (is (string= (show '(@walt :number :state) nil)
               "XD"))
  (is (string= (show '(@walt :number :state "NY") nil)
               "XD")))
