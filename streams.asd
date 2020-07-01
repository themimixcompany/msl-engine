;;;; streams.asd

#-ASDF3.1 (error "ASDF 3.1 or bust!")

(defpackage #:streams-system
  (:use #:cl #:asdf))

(in-package #:streams-system)

(defsystem #:streams
  :description "streams"
  :author "The Mimix Company <code@mimix.io>"
  :license "Blue Oak Model License 1.0.0"
  :version "2.2.15"
  :class :package-inferred-system
  :depends-on (#:cl-ppcre
               #:clack
               #:clack-handler-hunchentoot
               #:websocket-driver
               #:bordeaux-threads
               #:closer-mop
               #:maxpc
               #:trivial-file-size
               #:local-time
               #:slynk
               #:find-port
               #:marie
               "streams/specials"
               "streams/classes"
               "streams/common"
               "streams/parser"
               "streams/logger"
               "streams/dispatcher"
               "streams/unparser"
               "streams/bridge"
               "streams/admin-dispatcher"
               "streams/server"
               "streams/builder"
               "streams/startup"
               "streams/driver")
  :in-order-to ((test-op (test-op "streams-tests"))))

(defmethod perform :after ((o load-op) (c (eql (find-system :slynk))))
  (funcall (read-from-string "slynk::init")))
