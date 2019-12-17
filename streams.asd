;;;; streams.asd

(asdf:defsystem #:streams
  :description "streams"
  :author "Rommel MARTINEZ <rom@mimix.io>"
  :license  "BlueOak-1.0.0"
  :version "0.0.2"
  :serial t
  :depends-on (#:clack
               #:clack-handler-hunchentoot
               #:websocket-driver
               #:alexandria
               #:bordeaux-threads
               #:mof
               #:cl-ppcre)
  :components ((:file "packages")
               (:file "websocket")
               (:file "build")))
