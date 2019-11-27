;;;; build.lisp

(in-package #:engine)

(defun set-debugger-hook ()
  (setf *debugger-hook*
        (lambda (condition hook)
          (declare (ignore hook))
          (format *error-output* "Caught error: ~A" condition)
          (finish-output *error-output*)
          (uiop:quit))))

(defun build ()
  "Build the executable of the engine for different OSes."
  (let* ((suffix (cond ((uiop:os-macosx-p) "_macos")
                       ((uiop:os-windows-p) "_windows.exe")
                       ((uiop:os-unix-p) "_linux")
                       (t (error "No matching OS found."))))
         (path (pathname (mof:cat "engine/engine" suffix))))
    (uiop:ensure-all-directories-exist (list (namestring path)))
    (set-debugger-hook)
    (trivial-dump-core:save-executable path #'engine:main)))
