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

(def restore-log-data ()
  "Restore the most recent log log file."
  (ensure-log-file-exists)
  (restore-log))

(defun load-module ()
  "Run the module."
  (initialize-universe)
  (print-banner)
  (when *restore-log*
    (restore-log-data)))

(load-module)
