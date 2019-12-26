;;;; streams.asd

#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage #:streams-system
  (:use #:cl #:asdf))

(in-package #:streams-system)

(defsystem #:streams
  :description "streams"
  :author "Rommel MARTINEZ <rom@mimix.io>"
  :license "BlueOak-1.0.0"
  :version "1.0.2"
  :serial t
  :depends-on (#:cl-ppcre
               #:mof
               #:clack
               #:clack-handler-hunchentoot
               #:websocket-driver
               #:alexandria
               #:bordeaux-threads)
  :components ((:file "packages")
               (:file "utils")
               (:file "websocket")
               (:file "build")))
