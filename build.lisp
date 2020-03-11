;;;; build.lisp

(uiop:define-package #:streams/build
    (:use #:cl)
  (:export #:build))

(in-package #:streams/build)

(defgeneric build (&optional root)
  (:documentation "Build the executable of streams for different platforms."))

(defmethod build :before (&optional root)
  (declare (ignorable root))
  (marie:muffle-debugger))

(defmethod build (&optional (root *default-pathname-defaults*))
  (let ((arch (string (uiop:architecture))))
    (labels ((make-name (name)
               (let ((suffix (cond ((string= name "windows") (marie:cat name "_" arch ".exe"))
                                   (t (marie:cat name "_" arch)))))
                 (marie:cat "streams_" suffix))))
      (let* ((base-name (cond ((uiop:os-macosx-p) (make-name "macos"))
                              ((uiop:os-windows-p) (make-name "windows"))
                              ((uiop:os-unix-p) (make-name "unix"))
                              (t (error "No matching OS found."))))
             (path (uiop:subpathname* root base-name)))
        (uiop:ensure-all-directories-exist (list (namestring path)))
        #+sbcl
        (sb-ext:save-lisp-and-die path :toplevel #'streams/serve:serve :executable t :compression t)
        #+ccl
        (ccl:save-application path :toplevel-function #'streams/serve:serve :prepend-kernel t)
        #+clisp
        (ext:saveinitmem path :init-function #'(lambda () (funcall 'streams/serve:serve)
                                                 (ext:exit))
                              :executable t :norc t)))))
