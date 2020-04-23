;;;; logger.lisp

(uiop:define-package #:streams/logger
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:marie))

(in-package #:streams/logger)

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
  (build-path (cat path #\. +log-file-suffix+)))

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

(defun make-machine-log-path (machine date)
  "Return a log file path using MACHINE. Optional parameter DATE is for
specifying another date value."
  (make-log-file-path (cat machine #\. date)))

(defun log-path (&key (machine *machine*) (date (log-date *universe*)))
  "Return a log path for MACHINE under DATE."
  (make-machine-log-path machine date))

(defun alt-case-re (string)
  "Return a regular expression string for downcased and upcased members of string."
  (labels ((fn (args acc)
             (cond ((null args) acc)
                   (t (fn (cdr args)
                          (cat acc
                               "["
                               (char-downcase (car args))
                               (char-upcase (car args))
                               "]"))))))
    (fn (loop :for char :across string :collect char) "")))

(defun build-paths (directory)
  "Return a path with corrected string representations."
  (let ((files (uiop:directory-files directory)))
    #+ccl (mapcar #'(lambda (entry)
                      (uiop:ensure-pathname
                       (cl-ppcre:regex-replace-all "\\\\" (uiop:native-namestring entry) "")))
                  files)
    #-(or ccl) files))

(defun log-paths (&key (directory *log-directory*) (machine *machine*) sort)
  "Return all the log files in DIRECTORY."
  (let* ((files (build-paths directory))
         (entries (remove-if-not #'(lambda (file)
                                     (let ((name (file-namestring file))
                                           (suffix (alt-case-re +log-file-suffix+)))
                                       (cl-ppcre:scan (cat "^" machine "\\."
                                                           +iso-8601-re+ "\\."
                                                           suffix "$")
                                                      name)))
                                 files)))
    (if sort
        (mapcar #'(lambda (path)
                    (uiop:merge-pathnames* *log-directory* path))
                (sort (mapcar #'file-namestring entries) #'string<))
        entries)))

(defun* (log-path* t) (&key (directory *log-directory*) (machine *machine*))
  "Return the most recent log path of MACHINE."
  (last* (log-paths :directory directory :machine machine :sort t)))

(defun update-log-date (universe)
  "Update the log date on UNIVERSE to the current one."
  (setf (log-date universe)
        (local-time:format-timestring nil (local-time:now))))

(defun* (write-log t) (value)
  "Write VALUE to the computed log file."
  (flet ((fn (path)
           (ensure-file-exists path)
           (with-open-file (stream path :direction :output :if-exists :append)
             (format stream "~A~%" value))))
    (when (stringp value)
      (cond ((maximum-file-size-p (log-path*))
             (fn (make-machine-log-path *machine* (log-date *universe*))))
            (t (fn (log-path*)))))))
