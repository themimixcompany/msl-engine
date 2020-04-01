;;;; logger.lisp

(uiop:define-package #:streams/logger
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:*machine-name*
           #:*maximum-file-size*
           #:log-value))

(in-package #:streams/logger)

(marie:define-constant* +base-directory+
    (marie:home (marie:cat "." +self+ "/"))
  "The path to the default configuration and storage directory.")

(marie:define-constant* +default-log-file+
    (flet ((fn (parent path)
             (uiop:merge-pathnames* parent path)))
      (fn +base-directory+ (fn +self+ ".log")))
  "The path to the default file for logging.")

(marie:define-constant* +log-file-suffix+
  ".log"
  "The default file suffix for log files.")

(defparameter *maximum-file-size*
  5242880
  "The maximum filesize of logging files in bytes.")

(defvar *machine-name*
  "my-machine"
  "The default name to use as the machine name.")

(marie:define-constant* +day-names+
    '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
  "The enumeration of week day names.")

(marie:define-constant* +default-date+
    (local-time:format-timestring nil (local-time:now))
  "The default date and time string used for logging.")

(defun file-size (path)
  "Return the size of file indicated in PATH."
  (osicat-posix:stat-size (osicat-posix:stat path)))

(defun max-file-size-p (path)
  "Return true if the file indicated in PATH exceeds the maximum allowable size."
  (> (file-size path) *maximum-file-size*))

(defun build-path (path)
  "Return a new path based from the base directory."
  (uiop:merge-pathnames* +base-directory+ path))

(defun make-log-file-path (path)
  "Return a log file pathname from PATH."
  (build-path (marie:cat path +log-file-suffix+)))

(defun log-file-exists-p (name)
  "Return true if the log file indicated by PATH exists under the log directory."
  (let ((path (make-log-file-path name)))
    (uiop:file-exists-p path)))

(defun create-empty-file (path)
  "Create an empty file from PATH."
  (with-open-file (stream path :if-does-not-exist :create)))

(defun ensure-file-exists (path)
  "Create the log file indicated by PATH if it does not exist yet."
  (unless (uiop:file-exists-p path)
    (ensure-directories-exist path)
    (create-empty-file path)))

(defun purge-file (path)
  "Zero-out the file indicated by PATH."
  (uiop:delete-file-if-exists path)
  (create-empty-file path))

(defun purge-file* (path)
  "Zero-out the file indicated by PATH if it exceeds the threshold."
  (when (max-file-size-p path)
    (purge-file path)))

(defun make-machine-log-path (machine &optional (date +default-date+))
  "Return a log file path using MACHINE and the current date as specifiers."
  (make-log-file-path (marie:cat machine "." date)))

(defun log-value (value)
  "Write VALUE to the computed log file."
  (when (stringp value)
    (let ((path (make-machine-log-path *machine-name*)))
      (ensure-file-exists path)
      (purge-file* path)
      (with-open-file (stream path :direction :output :if-exists :append)
        (format stream "~A~%" value)))))
