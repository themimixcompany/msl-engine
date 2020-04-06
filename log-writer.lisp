;;;; log-writer.lisp

(uiop:define-package #:streams/log-writer
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:log-path
           #:write-log))

(in-package #:streams/log-writer)

(defun current-date ()
  "Return the current date and time in ISO 8601 format."
  (local-time:format-timestring nil (local-time:now)))

(defun file-size (path)
  "Return the size of file indicated in PATH."
  (trivial-file-size:file-size-in-octets path))

(defun maximum-file-size-p (path)
  "Return true if the file indicated in PATH exceeds the maximum allowable size."
  (when (uiop:file-exists-p path)
    (> (file-size path) *maximum-log-size*)))

(defun build-path (path)
  "Return a new path based from the base directory."
  (uiop:merge-pathnames* *log-directory* path))

(defun make-log-file-path (path)
  "Return a log file pathname from PATH."
  (build-path (marie:cat path #\. +log-file-suffix+)))

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
  (when (maximum-file-size-p path)
    (purge-file path)))

(defun make-machine-log-path (machine &optional (date +default-date+))
  "Return a log file path using MACHINE. Optional parameter DATE is for
specifying another date value."
  (make-log-file-path (marie:cat machine #\. date)))

(defun log-path (&key (machine *machine*) (date (log-date *mx-universe*)))
  "Return a log path for MACHINE under DATE."
  (make-machine-log-path machine date))

(defun update-log-date (mx-universe)
  "Update the log date on MX-UNIVERSE to the current one."
  (setf (log-date mx-universe)
        (local-time:format-timestring nil (local-time:now))))

(defun log-file (&optional update)
  "Return the current log file of the universe."
  (when update
    (update-log-date *mx-universe*))
  (make-machine-log-path *machine* (log-date *mx-universe*)))

(defun write-log (value)
  "Write VALUE to the computed log file."
  (flet ((fn (path)
           (ensure-file-exists path)
           (with-open-file (stream path :direction :output :if-exists :append)
             (format stream "~A~%" value))))
    (when (stringp value)
      (cond ((maximum-file-size-p (log-file))
             (fn (log-file t)))
            (t (fn (log-file)))))))
