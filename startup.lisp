;;;; startup.lisp

(uiop:define-package #:streams/startup
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:streams/common
        #:streams/logger
        #:streams/writer
        #:streams/bridge
        #:marie))

(in-package #:streams/startup)

(def initialize-universe ()
  "Initialize the universe."
  (setf *universe* (make-universe "main")))

(defun load-module ()
  "Run the module."
  (initialize-universe)
  (print-banner)
  (ensure-log-file-exists)
  (restore-log))

(load-module)
