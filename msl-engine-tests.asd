;;;; msl-engine-tests.asd

#-ASDF3.1 (error "ASDF 3.1 or bust!")

(defpackage #:msl-engine-tests-system
  (:use #:cl #:asdf))

(in-package #:msl-engine-tests-system)

(defsystem #:msl-engine-tests
  :description "msl-engine-tests"
  :author "The Mimix Company <code@mimix.io>"
  :license "Blue Oak Model License 1.0.0"
  :version "1.0.2"
  :class :package-inferred-system
  :depends-on (#:msl-engine
               #:fiveam
               "msl-engine/tests")
  :perform (test-op (o c) (uiop:symbol-call :msl-engine/tests :run-tests)))
