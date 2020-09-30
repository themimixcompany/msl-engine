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

(push #'open-foreign-libs #+sbcl sb-ext:*init-hooks*
                          #+ccl ccl:*lisp-startup-functions*)

(push #'close-foreign-libs #+sbcl sb-ext:*save-hooks*
                           #+ccl ccl:*save-exit-functions*)
