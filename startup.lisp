;;;; startup.lisp

(uiop:define-package #:streams/startup
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:streams/common
        #:marie))

(in-package #:streams/startup)

(def initialize-universe ()
  "Initialize the universe."
  (setf *universe* (make-universe "main")))

(def load-streams ()
  "Call the base initialization functions."
  (initialize-universe))

(load-streams)
