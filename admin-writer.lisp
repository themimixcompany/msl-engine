;;;; admin-writer.lisp

(uiop:define-package #:msl-engine/admin-writer
  (:use #:cl
        #:msl-engine/specials
        #:msl-engine/classes
        #:msl-engine/common
        #:msl-engine/parser
        #:msl-engine/reader
        #:msl-engine/logger
        #:msl-engine/writer
        #:msl-engine/bridge
        #:marie))

(in-package #:msl-engine/admin-writer)


;;--------------------------------------------------------------------------------------------------
;; assignments
;;--------------------------------------------------------------------------------------------------

(defp *admin-commands*
    '((("@" "VERSION")     . admin-version)
      (("@" "VER")         . admin-version)
      (("@" "CLEAR")       . admin-clear)
      (("@" "RESTORE-LOG") . admin-restore-log)
      (("@" "RESTORE")     . admin-restore-log)
      (("@" "ROTATE-LOG")  . admin-rotate-log)
      (("@" "ROTATE")      . admin-rotate-log))
  "The alist of paths and command symbols.")


;;--------------------------------------------------------------------------------------------------
;; handlers
;;--------------------------------------------------------------------------------------------------

(defun admin-version ()
  "Handle the admin command VERSION."
  (format-parens "@VER" *system-version*))

(defun admin-clear ()
  "Handle the admin command CLEAR."
  (clear)
  nil)

(defun admin-restore-log ()
  "Handle the admin command RESTORE-LOG."
  (restore-log)
  nil)

(defun admin-rotate-log ()
  "Handle the admin command RESTORE-LOG."
  ;;(rotate-log)
  nil)


;;--------------------------------------------------------------------------------------------------
;; entrypoints
;;--------------------------------------------------------------------------------------------------

(defun admin-command (expr)
  "Return the admin command for EXPR."
  (when-let* ((head (head expr))
              (value (assoc-value (head expr) *admin-commands* :test #'equalp)))
    value))

(def admin-dispatch (expr)
  "Dispatch an admin command."
  (let* ((command (admin-command expr))
         (value (when command (funcall command))))
    (cond ((null value) expr)
          (t value))))
