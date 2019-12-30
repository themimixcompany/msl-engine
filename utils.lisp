;;;; utils.lisp

(uiop:define-package #:streams/utils
    (:use #:cl)
  (:export #:hide-debugger-output))

(in-package #:streams/utils)

(defun hide-debugger-output ()
  "Hide the debugger output."
  (setf *debugger-hook*
        (lambda (condition hook)
          (declare (ignore hook))
          (format *error-output* "Caught error: ~A" condition)
          (finish-output *error-output*))))
