;;;; engine.asd

(asdf:defsystem #:engine
  :description "engine"
  :author "Rommel MARTINEZ <rom@mimix.io>"
  :license  "Blue Oak License"
  :version "0.0.1"
  :serial t
  :depends-on (#:clack
               #:clack-handler-hunchentoot
               #:websocket-driver
               #:alexandria
               #:bordeaux-threads
               #:mof
               #:cl-ppcre
               #:trivial-dump-core)
  :components ((:file "packages")
               (:file "websocket")
               (:file "build")))
