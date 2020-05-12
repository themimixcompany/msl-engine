;;;; driver.lisp

(uiop:define-package :streams/driver
  (:nicknames :streams)
  (:use :uiop/common-lisp)
  (:use-reexport #:streams/specials
                 #:streams/classes
                 #:streams/common
                 #:streams/parser
                 #:streams/sexp
                 #:streams/unparser
                 #:streams/logger
                 #:streams/dispatcher
                 #:streams/server
                 #:streams/builder
                 #:streams/startup))

(provide "streams")
(provide "STREAMS")
