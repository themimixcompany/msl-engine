;;;; startup.lisp

(uiop:define-package #:streams/startup
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:streams/log-reader))

(in-package #:streams/startup)

(defun initialize-mx-universe ()
  "Initialize the mx-universe."
  (setf *mx-universe* (make-mx-universe)))

(initialize-mx-universe)

(defun restore-log (&key (machine *machine*))
  "Re-initialize the universe"
  (read-log (log-path*)))

(restore-log)
