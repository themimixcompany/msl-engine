;;;; utils.lisp

(in-package #:streams/core)

(defun hide-debugger-output ()
  "Hide the debugger output."
  (setf *debugger-hook*
        (lambda (condition hook)
          (declare (ignore hook))
          (format *error-output* "Caught error: ~A" condition)
          (finish-output *error-output*))))

