;;;; streams.asd

#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage #:streams-system
  (:use #:cl #:asdf))

(in-package #:streams-system)

(defsystem #:streams
  :description "streams"
  :author "Rommel MARTINEZ <rom@mimix.io>"
  :license  "BlueOak-1.0.0"
  :version "1.0.1"
  :serial t
  :depends-on (#:uiop
               #:cl-ppcre
               #:cl-csv
               #:cl-nlp
               #:mof
               #:closer-mop
               #:cl-cpus
               #:lparallel
               #:cl-xlsx
               #:clack
               #:clack-handler-hunchentoot
               #:websocket-driver
               #:alexandria
               #:bordeaux-threads
               #+sbcl #:sb-sprof)
  :serial t
  :components ((:file "packages")
               (:file "websocket")
               (:file "build")
               (:file "globals")
               (:file "classes")
               (:file "nlp")
               (:file "setup")
               (:file "common")
               (:file "world")
               (:file "clos")
               (:file "dump")
               (:file "void")
               (:file "unit")
               (:file "constraints")
               (:file "matching")
               (:file "import")
               (:file "writers")
               (:file "initialize")
               (:file "tests")))
