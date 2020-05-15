;;;; builder.lisp

(uiop:define-package #:streams/builder
  (:use #:cl
        #:marie))

(in-package #:streams/builder)

(defun* build (&optional (root *default-pathname-defaults*))
  "Build the executable of streams for different platforms."
  (with-muffled-debugger
    (let ((arch (string (uiop:architecture))))
      (flet ((fn (name)
               (let ((suffix (cond ((string= name "windows") (cat name "_" arch ".exe"))
                                   (t (cat name "_" arch)))))
                 (cat "streams_" suffix))))
        (let* ((base-name (cond ((uiop:os-macosx-p) (fn "macos"))
                                ((uiop:os-windows-p) (fn "windows"))
                                ((uiop:os-unix-p) (fn "unix"))
                                (t (error "No matching OS found."))))
               (path (uiop:subpathname* root base-name)))
          (uiop:ensure-all-directories-exist (list (namestring path)))
          #+sbcl
          (sb-ext:save-lisp-and-die path :toplevel #'streams/server:serve
                                         :executable t
                                         :compression nil)
          #+ccl
          (ccl:save-application path :toplevel-function #'streams/server:serve
                                     :prepend-kernel t)
          #+clisp
          (ext:saveinitmem path :init-function #'(lambda () (funcall 'streams/server:serve) (ext:exit))
                                :executable t
                                :norc t))))))
