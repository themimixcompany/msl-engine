;;;; startup.lisp

(uiop:define-package #:streams/startup
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:streams/logger
        #:marie))

(in-package #:streams/startup)

(defun initialize-universe ()
  "Initialize the universe."
  (setf *universe* (make-universe)))

(initialize-universe)

(defun* (read-log t) (path)
  "Read the log file specified under PATH."
  (let ((exprs (uiop:read-file-lines path)))
    (loop :for expr :in exprs :do (streams/dispatcher:dispatch expr nil))))

(defun restore-log (&key (machine *machine*))
  "Re-initialize the universe"
  (let ((path (log-path* :machine machine)))
    (when path
      (read-log (log-path* :machine machine)))))

(restore-log)
