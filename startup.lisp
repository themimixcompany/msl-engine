;;;; startup.lisp

(uiop:define-package #:streams/startup
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:streams/logger
        #:streams/dispatcher
        #:streams/bridge
        #:marie))

(in-package #:streams/startup)

(defun initialize-universe ()
  "Initialize the universe."
  (setf *universe* (make-universe)))

(initialize-universe)

(defun restore-log-data ()
  "Restore the most recent log log file."
  (restore-log))

(restore-log-data)
