;;;; msl-engine.asd

#-ASDF3.1 (error "ASDF 3.1 or bust!")

(defpackage #:msl-engine-system
  (:use #:cl #:asdf))

(in-package #:msl-engine-system)

(defsystem #:msl-engine
  :description "msl-engine"
  :author "The Mimix Company <code@mimix.io>"
  :license "Blue Oak Model License 1.0.0"
  :version "3.0.2"
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
               #:cl-json
               #:marie
               #:msl-engine/specials
               #:msl-engine/classes
               #:msl-engine/common
               #:msl-engine/parser
               #:msl-engine/logger
               #:msl-engine/writer
               #:msl-engine/reader
               #:msl-engine/bridge
               #:msl-engine/admin-writer
               #:msl-engine/json
               #:msl-engine/server
               #:msl-engine/startup
               #:msl-engine/builder
               #:msl-engine/driver)
  :in-order-to ((test-op (test-op "msl-engine-tests"))))

(defmethod perform :after ((o load-op) (c (eql (find-system :slynk))))
  (funcall (read-from-string "slynk::init")))
