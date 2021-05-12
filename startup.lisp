;;;; startup.lisp

(uiop:define-package #:msl-engine/startup
  (:use #:cl
        #:msl-engine/specials
        #:msl-engine/classes
        #:msl-engine/common
        #:marie))

(in-package #:msl-engine/startup)

(def initialize-universe ()
  "Initialize the universe."
  (setf *universe* (make-universe "main")))

(def load-msl-engine ()
  "Call the base initialization functions."
  (initialize-universe))

(load-msl-engine)
