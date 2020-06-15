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

(defun* initialize-universe ()
  "Initialize the universe."
  (setf *universe* (make-universe)))

(defun* restore-log-data ()
  "Restore the most recent log log file."
  (restore-log))

(defun* start-slynk-server ()
  "Start a slynk server."
  (slynk:create-server :port *slynk-port* :dont-close t))

(defun main ()
  "Run the module."
  (initialize-universe)
  (restore-log-data)
  (start-slynk-server))

(main)
