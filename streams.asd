;;;; streams.asd

#-ASDF3.1 (error "ASDF 3.1 or bust!")

(defpackage #:streams-system
  (:use #:cl #:asdf))

(in-package #:streams-system)

(defsystem #:streams
  :description "streams"
  :author "The Mimix Company <code@mimix.io>"
  :license "Blue Oak Model License 1.0.0"
  :version "1.6.3"
  :class :package-inferred-system
  :depends-on (#:marie
               #:cl-ppcre
               #:clack
               #:clack-handler-hunchentoot
               #:websocket-driver
               #:bordeaux-threads
               #:closer-mop
               #:maxpc
               #:trivial-file-size
               #:local-time
               "streams/specials"
               "streams/classes"
               "streams/expr"
               "streams/unparser"
               "streams/logger"
               "streams/dispatcher"
               "streams/server"
               "streams/builder"
               "streams/startup"
               "streams/etc"
               "streams/driver")
  :in-order-to ((test-op (test-op "streams-tests"))))
