;;;; builder.lisp

(uiop:define-package #:streams/builder
  (:use #:cl
        #:streams/common
        #:streams/startup
        #:streams/server
        #:marie))

(in-package #:streams/builder)

(defun load-streams-serve ()
  "Initialize the universe, restore log data, and start the server."
  (print-banner)
  (load-streams)
  (serve))

(def build (&optional (root *default-pathname-defaults*))
  "Build the executable of streams for different platforms."
  (with-muffled-debugger ()
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
          (sb-ext:save-lisp-and-die path :toplevel #'load-streams-serve
                                         :executable t
                                         :compression nil)
          #+ccl
          (ccl:save-application path :toplevel-function #'load-streams-serve
                                     :prepend-kernel t)
          #+lispworks
          (lw:deliver #'load-streams-serve path 0
                      :multiprocessing t
                      :keep-pretty-printer t)
          #-(or sbcl ccl lispworks)
          (error "Building for ~A is currently not supported." (lisp-implementation-type)))))))
