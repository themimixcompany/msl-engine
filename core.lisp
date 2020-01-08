;;;; core.lisp

(uiop:define-package #:streams/core
    (:use #:cl)
  (:nicknames #:s/core)
  (:export #:mx-atom
           #:@))

(in-package #:streams/core)

(defun mx-atom-data-p (value)
  "Return true if VALUE is a valid mx-atom data.."
  (or (and (symbolp value) (not (keywordp value)))
      (stringp value)))

(defun mx-atom-metadata-p (value)
  "Return true if VALUE is a valid mx-atom metadata."
  (keywordp value))

(defun read-mx-atom (name &rest body)
  "Read the mx-atom data from NAME and BODY recursively, and return the result of the read as multiple values."
  (labels ((fn (args data metadata)
             (cond ((null args)
                    (values name (nreverse data) metadata))
                   ((mx-atom-data-p (car args))
                    (fn (cdr args) (cons (car args) data) metadata))
                   ((mx-atom-metadata-p (car args))
                    (fn (cddr args) data (append (list (car args) (cadr args)) metadata)))
                   (t (fn (cdr args) data metadata)))))
    (fn body nil nil)))

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
  (multiple-value-bind (name data metadata)
      (apply #'read-mx-atom args)
    (let ((data (s/common:build-string data))
          (metadata (nreverse (build-map metadata))))
      (streams/channels:make-mx-atom 'a name data metadata))))

(defun resolve-mx-atom (mx-atom)
  "Expand MX-ATOM into its constituent parts with respect to the surrounding context."
  (declare (ignorable mx-atom))
  nil)

(defun mx-atom-name (mx-atom)
  "Return the name used to identify MX-ATOM."
  (streams/channels:name mx-atom))

(defun mx-atom-data (mx-atom)
  "Return the first value used to identify MX-ATOM."
  (streams/channels:data mx-atom))

(defmacro write-context (context)
  "Build a context symbol."
  (let* ((symbol (ecase context
                  (m 'machine)
                  (w 'world)
                  (s 'stream)
                  (v 'view)
                  (c 'canon)))
         (var (mof:cat "streams/ethers:*mx-" (string symbol) "*")))
    (read-from-string var)))

;;; Note: accept a second value for the name of the subcontext
;;; Note: generate the context on-the-fly if it does not exist, yet
(defmacro context (context &body body)
  "Set the current context to CONTEXT then evaluate BODY."
  `(let ((streams/ethers:*context* (write-context ,context)))
     ,@body))
(defmacro m (&body body) `(context m ,@body))
(defmacro w (&body body) `(context w ,@body))
(defmacro s (&body body) `(context s ,@body))
(defmacro v (&body body) `(context v ,@body))
(defmacro c (&body body) `(context c ,@body))

(defmacro define-context-macros (contexts)
  "Define the macro shorthands for the context setters."
  `(progn
     ,@(loop :for c :in contexts
             :collect `(defmacro ,c (&body body)
                         (context ,c body)))))

(defun yield-context ()
  "Return the current context or the default context."
  (or streams/ethers:*context* streams/ethers:*mx-machine*))

;;; Note: enable embedding of other mx-atom expressions
(defun evaluate-mx-atom (&rest values)
  "Evaluate an mx-atom expression under VALUES, store into the current ctext, then return the mx-atom and the ctext as values."
  (macrolet ((ctext ()
               `(streams/channels:table (yield-context)))
             (hash (k)
               `(gethash ,k (ctext))))
    (destructuring-bind (name &body body)
        values
      (declare (ignore body))
      (if (mof:solop values)
          (multiple-value-bind (v presentp)
              (hash name)
            (when presentp
              (values v
                      (ctext))))
          (let* ((mx-atom (apply #'build-mx-atom values))
                 (key (mx-atom-name mx-atom)))
            (setf (hash key) mx-atom)
            (values mx-atom
                    (ctext)))))))

(defun mx-atom (&rest values)
  "Return the result of evaluating VALUES."
  (multiple-value-bind (mx-atom table)
      (apply #'evaluate-mx-atom values)
    (when mx-atom
      (values (mx-atom-data mx-atom)
              mx-atom
              table))))

;;; Hook (@foo ...) with the condition system
;;; Get information from the condition.
;;; If the first character is @, dispatch the designated function
(defun @ (&rest args)
  "Apply MX-ATOM to ARGS."
  (apply #'mx-atom args))
