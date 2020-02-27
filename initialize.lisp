;;;; initialize.lisp

(uiop:define-package #:streams/initialize
    (:use #:cl)
  (:nicknames #:s/initialize))

(defun initialize-mx-universe ()
  "Initialize the mx-universe."
  (setf streams/ethers:*mx-universe* (streams/channels:make-mx-universe)))

(initialize-mx-universe)
