;;;; startup.lisp

(uiop:define-package #:streams/startup
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:streams/common
        #:streams/logger
        #:streams/dispatcher
        #:streams/bridge
        #:marie))

(in-package #:streams/startup)

(defun* initialize-universe ()
  "Initialize the universe."
  (setf *universe* (make-universe)))

(defun* restore-log-data ()
  "Restore the most recent log log file."
  (restore-log))

(defun main ()
  "Run the module."
  (initialize-universe)
  (restore-log-data))

(main)
