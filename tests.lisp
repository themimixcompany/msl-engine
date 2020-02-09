;;;; tests.lisp

(uiop:define-package #:streams/tests
    (:use #:cl #:fiveam #:streams/core)
  (:nicknames #:s/tests)
  (:export #:run-tests))

(in-package #:streams/tests)

(def-suite all-tests)
(in-suite all-tests)

(defun run-tests ()
  "Run all the tests defined in the suite."
  (run! 'all-tests))

(test eval-expr-tests
  "Test the values returned by STREAMS/CORE:EVAL-EXPR."
  (is (null nil)))

(test show-tests
  "Test the values returned by STREAMS/CORE:SHOW."
  (is (string= $"(@walt Walt Disney :number 1 2 :species Human :state IL)" "Walt Disney"))
  (is (null $"(@walt Walt Disney :number 1 2 :species (@person) :state IL)"))
  (is (string= $"(@walt :species)" "Human"))
  (is (null $"(@walt (@nothing))"))
  (is (string= $"(@walt)" "Walt Disney"))
  (is (string= $"(@walt WD :number 0)" "WD"))
  (is (string= $"(@walt XD :number)" "XD"))
  (is (string= $"(@walt :number 1 :age 20 :gender m)" "XD"))
  (is (null $"(@david)"))
  (is (null $"(@david :number 0 :age 21 :gender m)"))
  (is (string= $"(@walt :number)" "1"))
  (is (string= $"(@walt:number)" "1"))
  (is (string= $"(@walt :number :state)" "XD"))
  (is (string= $"(@walt :number :state NY)" "XD"))
  (is (string= $"(@Houston Houston :state TX :nickname H-town)" "Houston"))
  (is (string= $"(@Houston:state)" "TX"))
  (is (string= $"(@energy-corridor An area in (@Houston), (@Houston:state) known as (@Houston :nickname The Energy Capital) concerned with petroleum exploration and drilling!)"
               "An area in Houston , TX known as The Energy Capital concerned with petroleum exploration and drilling!"))
  (is (string= $"(@energy-corridor :meta 0)" "0"))
  (is (string= $"(@energy-corridor:meta)" "0"))
  (is (null $"(@my-book)"))
  (is (null $"(@my-book:title)"))
  (is (string= $"(@my-book Vinyl Leaves)" "Vinyl Leaves"))
  (is (string= $"(@my-book:title Institutional Structures of Feelings)" "Institutional Structures of Feelings")))
