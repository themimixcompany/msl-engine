;;;; hooks.lisp

(uiop:define-package #:streams/hooks
  (:use #:cl
        #:marie))

(in-package #:streams/hooks)

(defun close-foreign-libs ()
  "Close foreign libs in use by streams at application save time."
  (let (#+sbcl (sb-ext:*muffled-warnings* 'style-warning))
    (mapc #'cffi:close-foreign-library '(cl+ssl::libssl))))

(defun open-foreign-libs ()
  "Open foreign libs in use by streams at application start time."
  (let (#+sbcl (sb-ext:*muffled-warnings* 'style-warning))
    (cl+ssl:reload)))

#+sbcl (push #'open-foreign-libs sb-ext:*init-hooks*)
#+ccl (push #'open-foreign-libs ccl:*lisp-startup-functions*)

#+sbcl (push #'close-foreign-libs sb-ext:*save-hooks*)
#+ccl (push #'close-foreign-libs ccl:*save-exit-functions*)
