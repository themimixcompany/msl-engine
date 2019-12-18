;;;; build.lisp

(in-package #:streams/core)

(defgeneric build (&optional root)
  (:documentation "Build the executable of streams for different platforms."))

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
                 (mof:cat "streams_" suffix))))
      (let* ((base-name (cond ((uiop:os-macosx-p) (make-name "macos"))
                              ((uiop:os-windows-p) (make-name "windows"))
                              ((uiop:os-unix-p) (make-name "unix"))
                              (t (error "No matching OS found."))))
             (path (uiop:subpathname* root base-name)))
        (uiop:ensure-all-directories-exist (list (namestring path)))
        #+sbcl
        (sb-ext:save-lisp-and-die path :toplevel #'streams/core:main :executable t)
        #+ccl
        (ccl:save-application path :toplevel-function #'streams/core:main :prepend-kernel t)
        #+clisp
        (ext:saveinitmem path :init-function #'(lambda () (funcall 'streams/core:main) (ext:exit))
                              :executable t :norc t)))))
