;;;; driver.lisp

(uiop:define-package :streams/driver
  (:nicknames :streams)
  (:use :uiop/common-lisp)
  (:use-reexport #:streams/specials
                 #:streams/classes
                 #:streams/expr
                 #:streams/log-writer
                 #:streams/dispatcher
                 #:streams/log-reader
                 #:streams/server
                 #:streams/builder
                 #:streams/startup
                 #:streams/etc))

(provide "streams")
(provide "STREAMS")
