;;;; build.lisp

(in-package #:engine)

(defun build ()
  "Build the executable of the engine for different OSes."
  (let* ((suffix (cond ((uiop:os-macosx-p) "_macos")
                       ((uiop:os-windows-p) "_windows.exe")
                       ((uiop:os-unix-p) "_linux")
                       (t (error "No matching OS found."))))
         (path (pathname (mof:cat "engine/engine" suffix))))
    (uiop:ensure-all-directories-exist (list (namestring path)))
    (trivial-dump-core:save-executable path #'engine:main)))