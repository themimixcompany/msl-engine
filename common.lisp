;;;; common.lisp

(uiop:define-package #:streams/common
    (:use #:cl)
  (:nicknames #:s/common)
  (:export #:hide-debugger-output
           #:defclass*
           #:slots
           #:read-preserve
           #:dump-object
           #:dump-table
           #:assoc-key
           #:assoc-value
           #:dotted-pair-p
           #:string-convert
           #:build-string))

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

(defun read-preserve (string)
  "Read from STRING preserving case."
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (read-from-string string)))

(defun dump-object (object)
  "Display the contents of OBJECT."
  (loop :for slot :in (streams/common:slots object)
        :do (format t "~A -> ~S~%" slot (funcall slot object))))

(defun dump-table (table)
  "Print the contents of hash table TABLE."
  (maphash #'(lambda (k v) (format t "~A => ~A~%" k v))
           table))

(defun assoc-key (key items)
  "Return the key found in ITEMS if KEY is found."
  (let ((val (assoc key items)))
    (when val
      (car val))))

(defun assoc-value (key items)
  "Return the value found in ITEMS if KEY is found."
  (let ((val (assoc key items)))
    (when val
      (cdr val))))

(defun dotted-pair-p (pair)
  "Return true if LIST is a dotted list."
  (cond ((atom (cdr pair)) t)
        ((listp (cdr pair)) nil)
        (t nil)))

(defun string-convert (value)
  "Convert VALUE to a string."
  (etypecase value
    (number (format nil "~A" value))
    (string value)
    (t (string value))))

(defun build-string (items)
  "Return a string from the concatenation of items."
  (let ((strings (loop :for item :in items :collect (string-convert item))))
    (format nil "~{~A~^ ~}" strings)))
