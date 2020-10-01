;;;; bridge.lisp

(uiop:define-package #:streams/bridge
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:streams/common
        #:streams/logger
        #:streams/writer
        #:marie))

(in-package #:streams/bridge)

(def read-log (path)
  "Read the log file specified under PATH."
  (let ((exprs (uiop:read-file-lines path)))
    (loop :for expr :in exprs :do (dispatch expr :log nil))
    exprs))

(def restore-log (&key (machine *machine*))
  "Re-initialize the universe"
  (when *restore-log*
    (let* ((path (log-path :machine machine))
           (exprs (read-log path))
           (length (length exprs)))
      (debug-print (fmt "Read ~A expressions from history." length)))))
