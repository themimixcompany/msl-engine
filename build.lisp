;;;; build.lisp

(in-package #:engine)

(defun build ()
  "Build the executable of the engine for different OSes."
  (let* ((suffix (cond ((uiop:os-macosx-p) "_macos")
                       ((uiop:os-windows-p) "_windows")
                       ((uiop:os-unix-p) "_linux")
                       (t (error "No matching OS found."))))
         (path (pathname (mof:cat "engine/engine" suffix))))
    (sb-ext:save-lisp-and-die path :toplevel #'engine:main :executable t)))