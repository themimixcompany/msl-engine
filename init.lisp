;;;; init.lisp

(uiop:define-package #:streams/init
  (:use #:cl))

(defun initialize-mx-universe ()
  "Initialize the mx-universe."
  (setf streams/specials:*mx-universe* (streams/classes:make-mx-universe)))

(initialize-mx-universe)

(defun restore-log (&key (machine streams/specials:*machine*))
  "Re-initialize the universe"
  (streams/log-reader:read-log (streams/log-reader:log-path*)))

(restore-log)
