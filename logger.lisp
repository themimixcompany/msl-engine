;;;; logger.lisp

(uiop:define-package #:streams/logger
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:*maximum-file-size*
           #:*machine-name*
           #:log-value))

(in-package #:streams/logger)

(defvar *base-directory*
    (marie:home (marie:cat #\. +self+ #\/))
  "The path to the default configuration and storage directory.")

(marie:define-constant* +log-file-suffix+
  "msl"
  "The default file suffix for log files.")

(marie:define-constant* +iso-8601-regex+
  "\\d{4}-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d(\\.\\d+)?(([+-]\\d\\d:\\d\\d)|Z)?"
  "The regular expression for ISO 8601 dates.")

(defparameter *maximum-file-size*
  5242880
  "The maximum filesize of logging files in bytes.")

(defvar *machine-name*
  "my-machine"
  "The default name to use as the machine name.")

(marie:define-constant* +day-names+
    '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
  "The enumeration of week day names.")

(defun current-date ()
  "Return the current date and time in ISO 8601 format."
  (local-time:format-timestring nil (local-time:now)))

(defun file-size (path)
  "Return the size of file indicated in PATH."
  (trivial-file-size:file-size-in-octets path))

(defun maximum-file-size-p (path)
  "Return true if the file indicated in PATH exceeds the maximum allowable size."
  (when (uiop:file-exists-p path)
    (> (file-size path) *maximum-file-size*)))

(defun build-path (path)
  "Return a new path based from the base directory."
  (uiop:merge-pathnames* *base-directory* path))

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

(defun update-log-date (mx-universe)
  "Update the log date on MX-UNIVERSE to the current one."
  (setf (log-date mx-universe)
        (local-time:format-timestring nil (local-time:now))))

(defun log-file (&optional update)
  "Return the current log file of the universe."
  (when update
    (update-log-date *mx-universe*))
  (make-machine-log-path *machine-name* (log-date *mx-universe*)))

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

;;(uiop:read-file-lines (streams/logger::log-file))

(defun build-alt-case-regex (string)
  (labels ((fn (args acc)
             (cond ((null args) acc)
                   (t (fn (cdr args)
                          (marie:cat acc "[" (car args) (char-upcase (car args)) "]"))))))
    (fn (loop :for char :across string :collect char) "")))

(defun log-files (&key (directory *base-directory*) (machine-name *machine-name*) sort)
  "Return all the log files in DIRECTORY."
  (let* ((files (uiop:directory-files directory))
         (entries (remove-if-not #'(lambda (file)
                                     (let ((name (file-namestring file))
                                           (suffix (build-alt-case-regex +log-file-suffix+)))
                                       (cl-ppcre:scan (marie:cat "^" machine-name "\\."
                                                                 +iso-8601-regex+ "\\."
                                                                 suffix "$")
                                                      name)))
                                 files)))
    (if sort
        (mapcar #'(lambda (path)
                    (uiop:merge-pathnames* *base-directory* path))
                (sort (mapcar #'file-namestring entries) #'string<))
        entries)))

(defun last-log-file (&key (directory *base-directory*) (machine-name *machine-name*))
  "Return the most recent log file path of MACHINE."
  (marie:last* (log-files :directory directory :machine-name machine-name :sort t)))
