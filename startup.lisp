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

(defun find-open-port ()
  "Return an open for slynk."
  (find-port:find-port :min 40000 :max 50000))

(defun* start-slynk-server ()
  "Start a slynk server."
  (let ((port (find-open-port))
        (*slynk-debug-p* nil))
    (when port
      (slynk:create-server :port port :dont-close t)
      (print-debug (fmt "Slynk port opened at ~A" port)))))

(defun main ()
  "Run the module."
  (initialize-universe)
  (restore-log-data)
  (start-slynk-server))

(main)
