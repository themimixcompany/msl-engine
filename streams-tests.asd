#-ASDF3.1 (error "ASDF 3.1 or bust!")

(defpackage #:streams-tests-system
  (:use #:cl #:asdf))

(in-package #:streams-tests-system)

(defsystem #:streams-tests
  :description "streams-tests"
  :author "The Mimix Company <code@mimix.io>"
  :license "BlueOak-1.0.0"
  :version "1.0.0"
  :class :package-inferred-system
  :depends-on (#:streams
               #:fiveam
               "streams/tests")
  ;; :perform (test-op (o s) (uiop:symbol-call :fiveam :run! 'streams/tests:all-tests))
  )
