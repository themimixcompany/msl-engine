;;;; streams-tests.asd

#-ASDF3.1 (error "ASDF 3.1 or bust!")

(defpackage #:streams-tests-system
  (:use #:cl #:asdf))

(in-package #:streams-tests-system)

(defsystem #:streams-tests
  :description "streams-tests"
  :author "The Mimix Company <code@mimix.io>"
  :license "Blue Oak Model License 1.0.0"
  :version "1.0.2"
  :class :package-inferred-system
  :depends-on (#:streams
               #:fiveam
               "streams/tests")
  :perform (test-op (o c) (uiop:symbol-call :streams/tests :run-tests)))
