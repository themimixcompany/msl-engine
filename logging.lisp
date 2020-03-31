;;;; logging.lisp

(uiop:define-package #:streams/logging
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:max-file-size-p
           #:ensure-log-file-exists))

(in-package #:streams/logging)

(defun file-size (path)
  "Return the size of file indicated in PATH."
  (osicat-posix:stat-size (osicat-posix:stat path)))

(defun max-file-size-p (path)
  "Return true if the file indicated in PATH exceeds the maximum allowable size."
  (> (file-size path) *max-file-size*))

(defun build-path (path)
  "Return a new path based from the base directory."
  (uiop:merge-pathnames* *base-directory* path))

(defun make-log-file-path (path)
  "Return a log file pathname from PATH."
  (build-path (marie:cat path ".log")))

(defun log-file-exists-p (name)
  "Return true if the log file indicated by PATH exists under the log directory."
  (let ((path (make-log-file-path name)))
    (uiop:file-exists-p path)))

(defun ensure-log-file-exists (name)
  "Create the log file indicated by PATH if it does not exist yet."
  (let ((path (make-log-file-path name)))
    (unless (uiop:file-exists-p path)
      (ensure-directories-exist path)
      (with-open-file (stream path :if-does-not-exist :create)))
    path))

(defun purge-file (path)
  "Zero-out the file indicated by PATH."
  (uiop:delete-file-if-exists path)
  (with-open-file (stream path :if-does-not-exist :create)))

(defun purge-log-file (name)
  "Zero-out the log file indicated by PATH."
  (let ((path (make-log-file-path name)))
    (when (uiop:file-exists-p path)
      (purge-file path))))
