;;;; bridge.lisp

(uiop:define-package #:msl-engine/bridge
  (:use #:cl
        #:msl-engine/specials
        #:msl-engine/classes
        #:msl-engine/common
        #:msl-engine/logger
        #:msl-engine/writer
        #:marie))

(in-package #:msl-engine/bridge)

(def read-log (path)
  "Read the log file specified under PATH."
  (let ((exprs (uiop:read-file-lines path)))
    (loop :for expr :in exprs :do (dispatch expr :log nil))
    exprs))

(def restore-log (&key (machine *machine*))
  "Re-initialize the universe"
  (ensure-log-file-exists)
  (let* ((path (log-path :machine machine))
         (exprs (read-log path))
         (length (length exprs)))
    (debug-print (fmt "Read ~A expressions from history." length))))
