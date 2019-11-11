;;;; webs.asd

(asdf:defsystem #:webs
  :description "webs"
  :author "Rommel MARTINEZ <rom@mimix.io>"
  :license  "Blue Oak License"
  :version "0.0.1"
  :serial t
  :depends-on (#:clack
               #:websocket-driver
               #:alexandria
               #:mof)
  :components ((:file "packages")
               (:file "utils")
               (:file "webs")))
