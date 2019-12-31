;;;; utils.lisp

(uiop:define-package #:streams/utils
    (:use #:cl)
  (:export #:hide-debugger-output
           #:defclass*))

(in-package #:streams/utils)

(defun hide-debugger-output ()
  "Hide the debugger output."
  (setf *debugger-hook*
        (lambda (condition hook)
          (declare (ignore hook))
          (format *error-output* "Caught error: ~A" condition)
          (finish-output *error-output*))))

(defmacro defclass* (name (&rest superclasses) (&rest slot-specs) &optional class-option)
  "Define a class and conditionally export its symbols."
  (let ((exports (mapcan (lambda (spec)
                           (when (getf (cdr spec) :export)
                             (let ((name (or (getf (cdr spec) :accessor)
                                             (getf (cdr spec) :reader)
                                             (getf (cdr spec) :writer))))
                               (when name (list name)))))
                         slot-specs)))
    `(progn
       (defclass ,name (,@superclasses)
         ,(append
           (mapcar (lambda (spec)
                     (let ((export-pos (position :export spec)))
                       (if export-pos
                           (append (subseq spec 0 export-pos)
                                   (subseq spec (+ 2 export-pos)))
                           spec)))
                   slot-specs)
           (when class-option (list class-option))))
       ,@(mapcar (lambda (name) `(export ',name))
                 exports))))
