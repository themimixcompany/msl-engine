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

(defun initialize-universe ()
  "Initialize the universe."
  (setf *universe* (make-universe)))

(defun print-banner ()
  "Print information about the software."
  (debug-print (fmt "streams v~A" *system-version*)))

(defun restore-log-data ()
  "Restore the most recent log log file."
  (ensure-log-file-exists)
  (restore-log))

(defun main ()
  "Run the module."
  (initialize-universe)
  (print-banner)
  (restore-log-data))

(main)
