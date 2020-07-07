;;;; json.lisp

(uiop:define-package #:streams/json
  (:use #:cl
        #:streams/specials
        #:streams/classes
        #:streams/common
        #:marie))

(in-package #:streams/json)


;;--------------------------------------------------------------------------------------------------
;; conditions
;;--------------------------------------------------------------------------------------------------

(define-condition message-error (error)
  ((text :initarg :text
         :reader text)))


;;--------------------------------------------------------------------------------------------------
;; main definitions
;;--------------------------------------------------------------------------------------------------

(defun* json-to-lisp (string)
  "Return a lisp representation of STRING."
  (json:decode-json-from-string string))

(defun* lisp-to-json (string)
  "Return a json representation of STRING."
  (json:encode-json-to-string string))

(defun* json-object-p (string)
  "Return true if STRING is a JSON object."
  (when*
    (handler-case (json:decode-json-from-string string)
      (json:json-syntax-error nil))))

(defun* message-data (message)
  "Return the appropriate data type from MESSAGE."
  (flet ((fn (v)
           (let ((value (if (stringp v) (list v) v)))
             (if (or (length= value 1)
                     (length= value 2))
                 value
                 nil))))
    (if (json-object-p message)
        (fn (json-to-lisp message))
        (fn message))))

(defun* make-return-data (message lisp-data js-data)
  "Return an appropriate return data."
  (cond ((json-object-p message) (lisp-to-json (list lisp-data js-data)))
        ((null lisp-data) "NIL")
        (t lisp-data)))

(defvar* *json-tests*
  "tests.json"
  "The basename of the JSON tests source file.")

(defun json-tests-path ()
  "Return the path for the JSON tests."
  (let* ((system (asdf:find-system (intern +self+ (find-package :keyword))))
         (source-directory (asdf:system-source-directory system))
         (file-path (uiop:merge-pathnames* *json-tests* source-directory)))
    (when (uiop:file-exists-p file-path)
      file-path)))

(defun* read-json-tests ()
  "Return a string from reading the JSON tests."
  (when-let* ((path (json-tests-path))
              (value (uiop:read-file-string path)))
    (json-to-lisp value)))

(defun* read-path (path set)
  "Return the value specified by PATH in SET."
  (labels ((fn (path value)
             (cond ((or (and (null path) (atom value) (stringp value))
                        (null path))
                    value)
                   ((and path (consp value))
                    (fn (cdr path) (assoc-value (car path) value)))
                   (t nil))))
    (fn (uiop:ensure-list path) set)))

(defmacro* make-request (type operation set)
  "Return a request for querying SET."
  `(when-let ((value (read-path '(,type ,operation) ,set)))
     value))

(defun* msl-send (set)
  "Return the msl send expression from set."
  (make-request :msl :send set))

(defun* msl-expect (set)
  "Return the msl expect expression from set."
  (make-request :msl :expect set))

(defun* admin-send (set)
  "Return the msl admin expression from set."
  (make-request :admin :send set))

(defun* admin-expect (set)
  "Return the msl admin expression from set."
  (make-request :admin :expect set))
