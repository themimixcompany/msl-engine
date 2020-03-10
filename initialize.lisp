;;;; initialize.lisp

(uiop:define-package #:streams/initialize
    (:use #:cl))

(defun initialize-mx-universe ()
  "Initialize the mx-universe."
  (setf streams/specials:*mx-universe* (streams/classes:make-mx-universe)))

(initialize-mx-universe)
