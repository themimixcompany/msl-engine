;;;; driver.lisp - re-export all the functionality in STREAMS

(uiop:define-package :streams/driver
  (:nicknames :streams)
  (:use :uiop/common-lisp)
  (:use-reexport #:streams/specials
                 #:streams/classes
                 #:streams/logger
                 #:streams/expr
                 #:streams/core
                 #:streams/serve
                 #:streams/build
                 #:streams/initialize
                 #:streams/etc))

(provide "streams")
(provide "STREAMS")
