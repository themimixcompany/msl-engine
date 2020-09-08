;;;; driver.lisp

(uiop:define-package :streams/driver
  (:nicknames #:streams)
  (:use #:uiop/common-lisp
        #:marie)
  (:use-reexport #:streams/specials
                 #:streams/classes
                 #:streams/common
                 #:streams/parser
                 #:streams/logger
                 #:streams/writer
                 #:streams/reader
                 #:streams/bridge
                 #:streams/admin-writer
                 #:streams/json
                 #:streams/server
                 #:streams/builder
                 #:streams/startup))

(provide "streams")
(provide "STREAMS")
