;;;; streams.asd

#-ASDF3.1 (error "ASDF 3.1 or bust!")

(defpackage #:streams-system
  (:use #:cl #:asdf))

(in-package #:streams-system)

(defsystem #:streams
  :description "streams"
  :author "The Mimix Company <code@mimix.io>"
  :license "Blue Oak Model License 1.0.0"
  :version "1.5.5"
  :class :package-inferred-system
  :depends-on (#:cl-ppcre
               #:marie
               #:clack
               #:clack-handler-hunchentoot
               #:websocket-driver
               #:bordeaux-threads
               #:closer-mop
               #:maxpc
               #:osicat
               "streams/specials"
               "streams/classes"
               "streams/logger"
               "streams/expr"
               "streams/core"
               "streams/serve"
               "streams/build"
               "streams/initialize"
               "streams/etc"
               "streams/driver")
  :in-order-to ((test-op (test-op "streams-tests"))))
