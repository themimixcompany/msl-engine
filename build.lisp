;;;; build.lisp

(in-package #:engine)

(defgeneric build (&optional root)
  (:documentation "Build the executable of the engine for different OSes."))

(defmethod build :before (&optional root)
  (setf *debugger-hook*
        (lambda (condition hook)
          (declare (ignore hook))
          (format *error-output* "Caught error: ~A" condition)
          (finish-output *error-output*)
          (uiop:quit))))

(defmethod build (&optional (root *default-pathname-defaults*))
  (let ((arch (string (uiop:architecture))))
    (labels ((make-name (name)
               (let ((suffix (cond ((string= name "windows") (mof:cat name "_" arch ".exe"))
                                   (t (mof:cat name "_" arch)))))
                 (mof:cat "engine_" suffix))))
      (let* ((base-name (cond ((uiop:os-unix-p) (make-name "unix"))
                              ((uiop:os-windows-p) (make-name "windows"))
                              ((uiop:os-macosx-p) (make-name "macos"))
                              (t (error "No matching OS found."))))
             (path (uiop:merge-pathnames* root base-name)))
        (uiop:ensure-all-directories-exist (list (namestring path)))
        (trivial-dump-core:save-executable path #'engine:main)))))
