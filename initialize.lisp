;;;; initialize.lisp

(uiop:define-package #:streams/initialize
    (:use #:cl)
  (:nicknames #:s/initialize))

(defun initialize-mx-universe ()
  "Initialize the mx-universe."
  (setf streams/ethers:*mx-universe* (streams/channels:make-mx-universe)))

(defun initialize-mx-machine ()
  "Initialize the default mx-machine."
  (setf streams/ethers:*mx-machine* (streams/channels:make-mx-machine)))

(initialize-mx-universe)
(initialize-mx-machine)
