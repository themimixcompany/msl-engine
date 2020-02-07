;;;; streams.asd

#-ASDF3.1 (error "ASDF 3.1 or bust!")

(defpackage #:streams-system
  (:use #:cl #:asdf))

(in-package #:streams-system)

(defsystem #:streams
  :description "streams"
  :author "The Mimix Company <code@mimix.io>"
  :license "Blue Oak Model License 1.0.0"
  :version "1.5.1"
  :class :package-inferred-system
  :depends-on (#:cl-ppcre
               #:mof
               #:clack
               #:clack-handler-hunchentoot
               #:websocket-driver
               #:alexandria
               #:bordeaux-threads
               #:closer-mop
               #:named-readtables
               #:maxpc
               "streams/common"
               "streams/ethers"
               "streams/channels"
               "streams/expr"
               "streams/core"
               "streams/reader"
               "streams/serve"
               "streams/build"
               "streams/initialize")
  :in-order-to ((test-op (test-op "streams-tests"))))
