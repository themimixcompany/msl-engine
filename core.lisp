;;;; core.lisp

(uiop:define-package #:streams/core
    (:use #:cl)
  (:nicknames #:s/core)
  (:export #:mx-atom
           #:@))

(in-package #:streams/core)

(defun mx-atom-data-p (data)
  "Return true if DATA is a valid mx-atom."
  (or (and (symbolp data) (not (keywordp data)))
      (stringp data)))

(defun read-mx-atom (&rest body)
  "Read the mx-atom data from BODY recursively. The first value retured is the main data while the second value returned is the metadata."
  (labels ((fn (args data)
             (cond ((keywordp (car args)) (values (nreverse data) args))
                   ((null args) (values (nreverse data) nil))
                   ((mx-atom-data-p (car args))
                    (fn (cdr args) (cons (car args) data)))
                   (t (fn (cddr args) data)))))
    (fn body nil)))

(defun build-pairs (items)
  "Group items into pairs."
  (when (evenp (length items))
    (labels ((fn (items acc)
               (cond ((null items) (nreverse acc))
                     (t (fn (cddr items)
                            (cons (list (first items) (second items))
                                  acc))))))
      (fn items nil))))

(defun build-map (items &key (test #'keywordp) (constructor #'cons))
  "Create key-value mappings from ITEMS."
  (loop :for item :in (build-pairs items)
        :when (funcall test (car item))
        :collect (destructuring-bind (k v)
                     item
                   (funcall constructor k v))))

(defun build-mx-atom (&rest args)
  "Return a valid MX-ATOM from ARGS."
  (multiple-value-bind (data metadata)
      (apply #'read-mx-atom args)
    (let ((head (build-map data :test #'symbolp))
          (body (build-map metadata)))
      (streams/channels:make-mx-atom 'basic head body))))

(defun resolve-mx-atom (mx-atom)
  "Expand MX-ATOM into its constituent parts with respect to the surrounding context."
  (declare (ignorable mx-atom))
  nil)

(defun mx-atom-key (mx-atom)
  "Return the first key used to identify MX-ATOM."
  (destructuring-bind ((k . v) &optional kv)
      (streams/channels:data mx-atom)
    (declare (ignore v kv))
    k))

(defun mx-atom-value (mx-atom)
  "Return the first value used to identify MX-ATOM."
  (destructuring-bind ((k . v) &optional kv)
      (streams/channels:data mx-atom)
    (declare (ignore k kv))
    v))

(defmacro write-context (context)
  "Build a context symbol."
  (let* ((symbol (ecase context
                  (m 'machine)
                  (w 'world)
                  (s 'stream)
                  (v 'view)
                  (c 'canon)))
         (var (mof:cat "streams/globals:*mx-" (string symbol) "*")))
    (read-from-string var)))

;;; Note: accept a second value for the name of the subcontext
(defmacro context (context &body body)
  "Set the current context to CONTEXT then evaluate BODY."
  `(let ((streams/globals:*context* (write-context ,context)))
     ,@body))

;;; Note: enable embedding of other mx-atom expressions
(defun evaluate-mx-atom (&rest values)
  "Evaluate an mx-atom under VALUES, store into the current context, then return the mx-atom and the context as values."
  (macrolet ((context ()
               `(streams/channels:table streams/globals:*context*))
             (hash (k)
               `(gethash ,k (context))))
    (destructuring-bind (name &body body)
        values
      (declare (ignorable body))
      (if (mof:solop values)
          (multiple-value-bind (v presentp)
              (hash name)
            (when presentp
              (values v
                      (context))))
          (let* ((mx-atom (apply #'build-mx-atom values))
                 (key (mx-atom-key mx-atom)))
            (setf (hash key) mx-atom)
            (values (hash key)
                    (context)))))))

(defun mx-atom (&rest values)
  "Return the primary mx-atom value from the evaluation of VALUES."
  (multiple-value-bind (a h)
      (apply #'evaluate-mx-atom values)
    (declare (ignorable h))
    (mx-atom-value a)))

(defun @ (&rest args)
  "Apply MX-ATOM to ARGS."
  (apply #'mx-atom args))
