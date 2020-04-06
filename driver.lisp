;;;; driver.lisp

(uiop:define-package :streams/driver
  (:nicknames :streams)
  (:use :uiop/common-lisp)
  (:use-reexport #:streams/specials
                 #:streams/classes
                 #:streams/expr
                 #:streams/log-writer
                 #:streams/core
                 #:streams/log-reader
                 #:streams/serve
                 #:streams/build
                 #:streams/init
                 #:streams/etc))

(provide "streams")
(provide "STREAMS")
