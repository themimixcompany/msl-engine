;;;; driver.lisp - re-export all the functionality in STREAMS

(uiop:define-package :streams/driver
  (:nicknames :streams)
  (:use :uiop/common-lisp)
  (:use-reexport #:streams/specials
                 #:streams/classes
                 #:streams/expr
                 #:streams/core
                 #:streams/serve
                 #:streams/build
                 #:streams/initialize))

(provide "streams")
(provide "STREAMS")
