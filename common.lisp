;;;; common.lisp

(uiop:define-package #:streams/common
    (:use #:cl)
  (:nicknames #:s/common)
  (:export #:hide-debugger-output
           #:defclass*
           #:slots
           #:propertiesp
           #:build-properties
           #:read-preserve
           #:read-mx-atom))

(in-package #:streams/common)

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

(defun slots (object)
  "Return the slot names of an object."
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(defun propertiesp (properties)
  "Return true if PROPERTIES is a proper properties data, where PROPERTIES is a key-value plist and that the 0th and 2nd items must be :PRIMARY-KEY and :PRIMARY-VALUE, respectively. An example of a valid PROPERTIES data is:

(:PRIMARY-KEY 'WALT :PRIMARY-VALUE \"Walt Disney\" :NO 10 :SPECIES \"human\")
"
  (and (evenp (length properties))
       (every #'keywordp (loop :for key :in properties :by #'cddr :collect key))
       (member :primary-key properties)
       (eql (elt properties 0) :primary-key)
       (member :primary-value properties)
       (eql (elt properties 2) :primary-value)))

(defun build-properties (properties)
  "Ensure that PROPERTIES is a plist that conforms to a proper mx-atom structure."
  (destructuring-bind (p-key p-value &rest body)
      properties
    (let ((props `(:primary-key ,p-key :primary-value ,p-value ,@body)))
      (when (propertiesp props)
        props))))

(defun read-preserve (string)
  "Read from STRING preserving case."
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (read-from-string string)))
