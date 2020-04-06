;;;; log-reader.lisp

(uiop:define-package #:streams/log-reader
  (:use #:cl
        #:streams/specials
        #:streams/classes)
  (:export #:log-paths
           #:log-path*
           #:read-log))

(in-package #:streams/log-reader)

(defun alt-case-re (string)
  "Return a regular expression string for downcased and upcased members of string."
  (labels ((fn (args acc)
             (cond ((null args) acc)
                   (t (fn (cdr args)
                          (marie:cat acc
                                     "["
                                     (char-downcase (car args))
                                     (char-upcase (car args))
                                     "]"))))))
    (fn (loop :for char :across string :collect char) "")))

(defun log-paths (&key (directory *log-directory*) (machine *machine*) sort)
  "Return all the log files in DIRECTORY."
  (let* ((files (uiop:directory-files directory))
         (entries (remove-if-not #'(lambda (file)
                                     (let ((name (file-namestring file))
                                           (suffix (alt-case-re +log-file-suffix+)))
                                       (cl-ppcre:scan (marie:cat "^" machine "\\."
                                                                 +iso-8601-re+ "\\."
                                                                 suffix "$")
                                                      name)))
                                 files)))
    (if sort
        (mapcar #'(lambda (path)
                    (uiop:merge-pathnames* *log-directory* path))
                (sort (mapcar #'file-namestring entries) #'string<))
        entries)))

(defun log-path* (&key (directory *log-directory*) (machine *machine*))
  "Return the most recent log path of MACHINE."
  (marie:last* (log-paths :directory directory :machine machine :sort t)))

(defun read-log (path)
  "Read the log file specified under PATH."
  (let ((exprs (uiop:read-file-lines path)))
    (loop :for expr :in exprs :do (streams/dispatch expr))))
