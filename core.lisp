;;;; core.lisp

(uiop:define-package #:streams/core
    (:use #:cl)
  (:nicknames #:s/core)
  (:export #:@))

(in-package #:streams/core)

(defun dump-mx-atom (mx-atom)
  "Display the contents of MX-ATOM."
  (loop :for slot :in (streams/common:slots mx-atom)
        :do (format t "~A: ~S~%" slot (funcall slot mx-atom))))

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

(defmacro write-context (context)
  "Build a context symbol from the globals."
  (read-from-string (mof:cat "streams/globals:" (string context))))

(defmacro set-context (context &body body)
  "Set the current context to CONTEXT then evaluate BODY."
  `(let ((streams/globals:*context* (write-context ,context)))
     ,@body))

(defun evaluate-mx-atom (&rest values)
  "Evaluate an mx-atom under VALUES, store into the current context, then return the mx-atom and the context as values."
  (let* ((mx-atom (apply #'build-mx-atom values))
         (key (mx-atom-key mx-atom)))
    (macrolet ((context ()
                 `(streams/channels:table streams/globals:*context*))
               (hash (k)
                 `(gethash ,k (context))))
      (setf (hash key) mx-atom)
      (values (hash key)
              (context)))))

;;; Note: use symbol-macros to designate the contexts

(defun dispatch ()
  nil)
