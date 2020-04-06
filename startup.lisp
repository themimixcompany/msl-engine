;;;; startup.lisp

(uiop:define-package #:streams/startup
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:streams/log-reader))

(in-package #:streams/startup)

(defun initialize-universe ()
  "Initialize the universe."
  (setf *universe* (make-universe)))

(initialize-universe)

(defun restore-log (&key (machine *machine*))
  "Re-initialize the universe"
  (read-log (log-path* :machine machine)))

(restore-log)
