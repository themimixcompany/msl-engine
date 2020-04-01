;;;; logger.lisp

(uiop:define-package #:streams/logger
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:log-value))

(in-package #:streams/logger)

(marie:define-constant* +base-directory+
  (marie:home (marie:cat +self+ "/"))
  "The path to the default configuration and storage directory.")

(marie:define-constant* +default-log-file+
  (flet ((fn (parent path)
           (uiop:merge-pathnames* parent path)))
    (fn +base-directory+ (fn +self+ ".log")))
  "The path to the default file for logging.")

(marie:define-constant* +log-file-suffix+
  ".log"
  "The default file suffix for log files.")

(defparameter *max-file-size*
  5242880                               ;5 MiB
  "The maximum filesize of logging files in bytes.")

(defun file-size (path)
  "Return the size of file indicated in PATH."
  (osicat-posix:stat-size (osicat-posix:stat path)))

(defun max-file-size-p (path)
  "Return true if the file indicated in PATH exceeds the maximum allowable size."
  (> (file-size path) *max-file-size*))

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

(defun ensure-log-file-exists (name)
  "Create the log file indicated by PATH if it does not exist yet."
  (let ((path (make-log-file-path name)))
    (unless (uiop:file-exists-p path)
      (ensure-directories-exist path)
      (create-empty-file path))
    path))

(defun purge-file (path)
  "Zero-out the file indicated by PATH."
  (uiop:delete-file-if-exists path)
  (create-empty-file path))

(defun purge-log-file (name)
  "Zero-out the log file indicated by PATH."
  (let ((path (make-log-file-path name)))
    (when (uiop:file-exists-p path)
      (purge-file path))))

(marie:define-constant* +day-names+
    '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
  "The enumeration of week day names.")

(defun current-date ()
  "Return the current date and time in YYYYMMDDHHMM.TZ format."
  ;; (multiple-value-bind
  ;;       (second minute hour day-of-month month year day-of-week dstp tz)
  ;;     (get-decoded-time)
  ;;   (declare (ignorable day-of-week dstp))
  ;;   (format nil "~d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d.+~2,'0d"
  ;;           year month day-of-month
  ;;           hour minute second
  ;;           (- tz)))
  (local-time:format-timestring nil (local-time:now)))

(defvar *machine-name*
  "my-machine"
  "The default name to use as the machine name.")

(marie:define-constant* +default-date+
    (current-date)
  "The default date and time string used for logging.")

(defun make-log-path (machine &optional (date +default-date+))
  "Return a log file path using MACHINE and the current date as specifiers."
  (make-log-file-path (marie:cat machine "." date)))

(defvar *default-log-path*
  (make-log-path *machine-name*)
  "The default pathname used for logging.")

(defun log-value (value name)
  "Write the value VALUE to the log file indicated by NAME."
  ;; Add size constraints
  (ensure-log-file-exists name)
  (let ((path (make-log-file-path name)))
    (with-open-file (stream path :direction :output :if-exists :append)
      (format stream "~A~%" value)
      t)))
