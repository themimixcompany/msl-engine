;;;; admin-dispatcher.lisp

(uiop:define-package #:streams/admin-dispatcher
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:streams/common
        #:streams/parser
        #:streams/unparser
        #:streams/dispatcher
        ;;#:streams/logger
        #:marie))

(in-package #:streams/admin-dispatcher)

(defun* admin-version ()
  "Run the admin command VERSION."
  *system-version*)

(defun* admin-clear ()
  "Run the admin command CLEAR."
  (clear)
  nil)

;; (defun* admin-restore-log ()
;;   "Run the admin command RESTORE-LOG."
;;   (restore-log)
;;   nil)

;; (defun* admin-rotate-log ()
;;   "Run the admin command RESTORE-LOG."
;;   (rotate-log)
;;   nil)

(defparameter* *admin-commands*
  '((("@" "VERSION")     . admin-version)
    (("@" "CLEAR")       . admin-clear)
    (("@" "RESTORE-LOG") . admin-restore-log)
    (("@" "ROTATE-LOG")  . admin-rotate-log))
  "The alist of paths and command symbols.")

(defun* admin-command-p (expr)
  "Return true if EXPR is a valid admin command."
  (when-let ((head (head expr)))
    (when* (assoc head *admin-commands* :test #'equal))))

(defun* admin-command (expr)
  "Return the admin command for EXPR."
  (when-let* ((head (head expr))
              (value (assoc-value head *admin-commands* :test #'equal)))
    value))

(defun* admin-dispatch (expr)
  "Dispatch an admin command."
  (let* ((command (admin-command expr))
         (value (if command (funcall command) nil)))
    (cond ((null value) expr)
          (t value))))
